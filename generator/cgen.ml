(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Syntax;;
open Format;;
open Generate;;

(* hash variant types.  taken from ocaml source *)
let hash_variant s =
  let accu = ref 0 in  
    for i = 0 to String.length s - 1 do 
        accu := 223 * !accu + Char.code s.[i]
          done;
            (* reduce to 31 bits *)
    accu := !accu land (1 lsl 31 - 1);
      (* make it signed for 64 bits architectures *)
    if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu


let output_c_conversions (h:formatter) (c:formatter) (dl:xdr_def list)
      shn base_name =
  let typenames, typemap = extract_type_info dl in
  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in

  let constants = ref [] in

  fprintf h "@[<v>";
  fprintf h "#include <caml/mlvalues.h>@,";
  fprintf h "#include <caml/alloc.h>@,";
  fprintf h "#include <caml/memory.h>@,";
  fprintf h "#include <caml/callback.h>@,";
  fprintf h "#include \"%s.h\"@," base_name;

  fprintf c "@[<v>";
  fprintf c "#include <assert.h>@,";
  fprintf c "#include <caml/mlvalues.h>@,";
  fprintf c "#include <caml/alloc.h>@,";
  fprintf c "#include <caml/memory.h>@,";
  fprintf c "#include <caml/callback.h>@,";
  fprintf h "#include \"%s.h\"@," base_name;
  fprintf c "#include \"%s.h\"@," base_name;
  fprintf c "#include \"%s\"@," shn;

  let output_to_struct_field name t =
    match t with
    | T_opaque_fixed n ->
        let (_, size) = (constant !n) in
        let size = string_of_uint4 size in
        fprintf c "field = alloc_string(%s);@," size;
        fprintf c "memcpy(String_val(field), arg->%s, %s);@," name size
    | T_string_unlimited
    | T_string _ ->
        fprintf c "field = copy_string(arg->%s);@," name
    | T_opaque_unlimited
    | T_opaque _ ->
        fprintf c "field = alloc_string(arg->%s.%s_len);@," name name;
        fprintf c "memcpy(String_val(field), arg->%s.%s_val, " name name;
        fprintf c "arg->%s.%s_len);@," name name
    | T_refer_to (_, n) ->
        fprintf c "field = _to_%s(&arg->%s);@," !n name
    | T_bool ->
        fprintf c "field = arg->%s ? Val_true : Val_false;@," name
    | T_option (T_refer_to (_, n)) ->
        fprintf c "@[<v 4>if (arg->%s == NULL)@," name;
        fprintf c "field = Val_int(0);@]@,";
        fprintf c "@[<v 4>else {@,";
        fprintf c "CAMLlocal1 (temp);@,";
        fprintf c "temp = _to_%s(arg->%s);@," !n name;
        fprintf c "field = alloc(1, 0);@,";
        fprintf c "Store_field(field, 0, temp);@]@,";
        fprintf c "}@,";
    | T_option _ ->
        fprintf c "/* unknown T_option field: %s */@," name
    | T_array_unlimited (T_int _) ->
        fprintf c "field = alloc(arg->%s.%s_len, 0);@," name name;
        fprintf c "@[<v 4>{@,";
        fprintf c "int i;@,";
        fprintf c "CAMLlocal1 (elt);@,";
        fprintf c "@[<v 4>for (i = arg->%s.%s_len - 1; i >= 0; i--) {@,"
            name name;
        fprintf c "elt = copy_int32(arg->%s.%s_val[i]);@," name name;
        fprintf c "Store_field(field, i, elt);@]@,";
        fprintf c "}@]@,";
        fprintf c "}@,";
    | _ -> fprintf c "/* unknown field: name = %s */@," name
  in

  let output_of_struct_field name t =
    match t with
    | T_opaque_fixed n ->
        let (_, size) = (constant !n) in
        let size = string_of_uint4 size in
        fprintf c "memcpy(to->%s, String_val(field), %s);@," name size
    | T_string_unlimited
    | T_string _ ->
        fprintf c "to->%s = strdup(String_val(field));@," name
    | T_opaque_unlimited
    | T_opaque _ ->
        fprintf c "to->%s.%s_len = string_length(field);@," name name;
        fprintf c "to->%s.%s_val = " name name;
        fprintf c "malloc(to->%s.%s_len);@," name name;
        fprintf c "memcpy(to->%s.%s_val, String_val(field), " name name;
        fprintf c "to->%s.%s_len);@," name name;
    | T_refer_to (_, n) ->
        fprintf c "_of_%s(field, &to->%s);@," !n name
    | T_bool ->
        fprintf c "to->%s = Int_val(field);@," name
    | T_option (T_refer_to (_, n)) ->
        fprintf c "@[<v 4>if (field == Val_int(0))@,";
        fprintf c "to->%s = NULL;@]@," name;
        fprintf c "@[<v 4>else {@,";
        fprintf c "to->%s = malloc(sizeof(%s));@," name !n;
        fprintf c "_of_%s(Field(field, 0), to->%s);@]@," !n name;
        fprintf c "}@,"
    | T_array_unlimited (T_int _) ->
        fprintf c "to->%s.%s_len = Wosize_val(field);@," name name;
        fprintf c "@[<hov 4>to->%s.%s_val =@ " name name;
        fprintf c "malloc(sizeof(int) * to->%s.%s_len);@]@," name name;
        fprintf c "@[<v 4>{@,";
        fprintf c "int i;@,";
        fprintf c "@[<v 4>for (i = to->%s.%s_len - 1; i >= 0; i--)@,"
            name name;
        fprintf c "to->%s.%s_val[i] = Int32_val(Field(field, i));@]@]@,"
            name name;
        fprintf c "}@,";
    | _ -> fprintf c "/* unknown field: name = %s */@," name
  in

  (* returns list of (cConstant, variantName, None|Some (cField, type)) *)
  let extract_union_cases u =
    let string_of_constant con =
      let (sign, absval) = constant con in
      let absval' = string_of_uint4 absval in
      if sign then "-"^absval' else absval'
    in
    let discr_type = get_type_of_decl u.discriminant in
    let getField { decl_type = dt; decl_symbol = { xdr_name = xn } } =
      match dt with
      | T_refer_to (_, n) ->
          Some (xn, !n)
      | T_void ->
          None
      | _ ->
          fprintf c "/* default: unknown type */@,";
          None
    in
    let default =
      match u.default with
      | None -> None
      | Some xd -> getField xd
    in
    let make_variant_name con =
      match discr_type with
      | T_bool ->
          if (string_of_constant con) = "0" then "False" else "True"
      | T_enum l ->
          let (id, _) =
              List.find (fun (id, n) -> constant !n = constant con) l
          in
          id.ocaml_name
      | T_int _
      | T_uint _ ->
          let (sign, absval) = constant con in
          (if sign then "__" else "_") ^ (string_of_uint4 absval)
      | _ ->
          "Unknown discriminant type"
    in
    let get_variant_name con om =
      match om with
      | None -> make_variant_name con
      | Some name -> name
    in
    let explicit_cases =
      List.map (fun (con, om, td) ->
          (string_of_constant !con, get_variant_name !con om, getField td))
      u.cases
    in
    let default_cases =
      match discr_type with
      | T_int _ | T_uint _ -> [("default", "default", default)]
      | _ ->
          let missed_cases =
            List.filter
              (fun n -> not (List.exists (fun (c,_,_) -> !c = n) u.cases))
              (values_of_enum_type discr_type)
          in
          List.map
            (fun n -> (string_of_constant n, get_variant_name n None, default))
            missed_cases
    in
    explicit_cases @ default_cases
  in

  let output_to_type name t =
    match t with
    | T_opaque_fixed n ->
        let (_, size) = (constant !n) in
        let size = string_of_uint4 size in
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "CAMLparam0 ();@,";
        fprintf c "CAMLlocal1 (res);@,@,";
        fprintf c "res = alloc_string(%s);@," size;
        fprintf c "memcpy(String_val(res), *arg, %s);@," size;
        fprintf c "@,CAMLreturn (res);@]@,";
        fprintf c "}@,@,"

    | T_string_unlimited
    | T_string _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "return copy_string(*arg);@]@,";
        fprintf c "}@,@,"

    | T_opaque_unlimited
    | T_opaque _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "CAMLparam0 ();@,";
        fprintf c "CAMLlocal1 (res);@,@,";
        fprintf c "res = alloc_string(arg->%s_len);@," name;
        fprintf c "memcpy(String_val(res), arg->%s_val, arg->%s_len);@,"
            name name;
        fprintf c "@,CAMLreturn (res);@]@,";
        fprintf c "}@,@,"

    | T_union u ->
        let { decl_symbol = { xdr_name = disc_name } } = u.discriminant in
        let discr_type = get_type_of_decl u.discriminant in
        let cases = extract_union_cases u in

        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "CAMLparam0 ();@,";
        fprintf c "CAMLlocal2 (res, field);@,@,";
        fprintf c "@[<v 4>switch (arg->%s) {@," disc_name;
        let default = ref None in
        List.iter
          (fun (cConstant, variantName, arg) ->
            if not (List.mem variantName !constants)
              then constants := variantName :: !constants;
            if cConstant = "default" then default := Some arg else
            (fprintf c "@[<v 4>case %s:@," cConstant;
            (match arg with
            | None ->
                fprintf c "res = Val_int(VARIANT_%s);@," variantName
            | Some (cField, typeName) ->
                fprintf c "res = alloc(2, 0);@,";
                fprintf c "field = _to_%s(&arg->%s_u.%s);@,"
                    typeName name cField;
                fprintf c "Store_field(res, 0, Val_int(VARIANT_%s));@,"
                    variantName;
                fprintf c "Store_field(res, 1, field);@,");
            fprintf c "break;@]@,"))
          cases;
        fprintf c "@[<v 4>default:@,";
        (match !default with
        | None ->
            fprintf c "assert(0);@]@]@,"
        | Some None ->
            fprintf c "res = alloc(2, 0);@,";
            (match discr_type with
            | T_int _ ->
                fprintf c "field = copy_int32(arg->%s);@," disc_name
            | T_uint _ ->
                fprintf c "field = copy_int32(arg->%s);@," disc_name
            | _ ->
                fprintf c "/* unknown default (discriminant) type */@,");
            fprintf c "Store_field(res, 0, Val_int(VARIANT_default));@,";
            fprintf c "Store_field(res, 1, field);@]@]@,"
        | _ ->
            fprintf c "/* unknown default type */@,");
        fprintf c "}@,";
        fprintf c "@,CAMLreturn (res);@]@,";
        fprintf c "}@,@,"

    | T_struct lst ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "CAMLparam0 ();@,";
        fprintf c "CAMLlocal2 (res, field);@,@,";
        fprintf c "res = alloc(%d, tag_%s);@," (List.length lst) name;
        let count = ref 0 in
        let handleField decl =
          let fieldName = decl.decl_symbol.xdr_name in
          output_to_struct_field fieldName decl.decl_type;
          fprintf c "Store_field(res, %d, field);@," !count;
          count := !count + 1
        in
        List.iter handleField lst;
        fprintf c "@,CAMLreturn (res);@]@,";
        fprintf c "}@,@,"

    | T_enum _
    | T_int _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "return copy_int32(*arg);@]@,";
        fprintf c "}@,@,"

    | T_uint _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "return copy_int32(*arg);@]@,";
        fprintf c "}@,@,"

    | T_hyper _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "return copy_int64(*arg);@]@,";
        fprintf c "}@,@,"

    | T_uhyper _ ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "return copy_int64(*arg);@]@,";
        fprintf c "}@,@,"

    | T_option (T_refer_to (_, n)) ->
        fprintf h "value _to_%s(%s *arg);@," name name;

        fprintf c "@[<v 4>value _to_%s(%s *arg) {@," name name;
        fprintf c "CAMLparam0 ();@,";
        fprintf c "CAMLlocal1 (res);@,@,";
        fprintf c "@[<v 4>if (*arg == NULL)@,";
        fprintf c "res = Val_int(0);@]@,";
        fprintf c "@[<v 4>else {@,";
        fprintf c "CAMLlocal1 (temp);@,";
        fprintf c "temp = _to_%s(*arg);@," !n;
        fprintf c "res = alloc(1, 0);@,";
        fprintf c "Store_field(res, 0, temp);@]@,";
        fprintf c "}@,";
        fprintf c "@,CAMLreturn (res);@]@,";
        fprintf c "}@,@,"

    | _ -> fprintf c "/*@, * unknown type: %s@, */@,@," name

  in

  let output_of_type name t =
    match t with
    | T_opaque_fixed n ->
        let (_, size) = (constant !n) in
        let size = string_of_uint4 size in
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "memcpy(to, String_val(from), %s);@]@," size;
        fprintf c "}@,@,"

    | T_string_unlimited
    | T_string _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "*to = strdup(String_val(from));@]@,";
        fprintf c "}@,@,"

    | T_opaque_unlimited
    | T_opaque _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "to->%s_len = string_length(from);@," name;
        fprintf c "to->%s_val = malloc(to->%s_len);@," name name;
        fprintf c "memcpy(to->%s_val, String_val(from), to->%s_len);@]@,"
            name name;
        fprintf c "}@,@,"

    | T_union u ->
        let { decl_symbol = { xdr_name = disc_name } } = u.discriminant in
        let discr_type = get_type_of_decl u.discriminant in
        let cases = extract_union_cases u in

        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "CAMLparam1 (from);@,@,";
        fprintf c "int key;@,";
        fprintf c "@[<v 4>if (Is_block(from))@,";
        fprintf c "key = Int_val(Field(from, 0));@]@,";
        fprintf c "@[<v 4>else@,";
        fprintf c "key = Int_val(from);@]@,";
        fprintf c "@[<v 4>switch (key) {@,";
        List.iter
          (fun (cConstant, variantName, arg) ->
            if not (List.mem variantName !constants)
              then constants := variantName :: !constants;
            fprintf c "@[<v 4>case VARIANT_%s:@," variantName;
            if cConstant = "default"
              then
                begin
                  match discr_type with
                  | T_int _ ->
                      fprintf c "to->%s = Int32_val(Field(from, 1));@,"
                          disc_name
                  | T_uint _ ->
                      fprintf c "to->%s = Int32_val(Field(from, 1));@,"
                          disc_name
                  | _ ->
                      fprintf c "/* unknown default (discriminant) type */@,"
                end
              else
                begin
                  fprintf c "to->%s = %s;@," disc_name cConstant;
                  (match arg with
                  | None -> ()
                  | Some (cField, typeName) ->
                      fprintf c "_of_%s(Field(from, 1), &to->%s_u.%s);@,"
                          typeName name cField)
                end;
            fprintf c "break;@]@,")
          cases;
        fprintf c "@[<v 4>default:@,";
        fprintf c "assert(0);@]@]@,";
        fprintf c "}@,";
        fprintf c "@,CAMLreturn0;@]@,";
        fprintf c "}@,@,"

    | T_struct lst ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "CAMLparam1 (from);@,";
        fprintf c "CAMLlocal1 (field);@,@,";
        let count = ref 0 in
        let handleField decl =
          let fieldName = decl.decl_symbol.xdr_name in
          fprintf c "field = Field(from, %d);@," !count;
          count := !count + 1;
          output_of_struct_field fieldName decl.decl_type
        in
        List.iter handleField lst;
        fprintf c "@,CAMLreturn0;@]@,";
        fprintf c "}@,@,"

    | T_enum _
    | T_int _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "*to = Int32_val(from);@]@,";
        fprintf c "}@,@,"

    | T_uint _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "*to = Int32_val(from);@]@,";
        fprintf c "}@,@,"

    | T_hyper _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "*to = Int64_val(from);@]@,";
        fprintf c "}@,@,"

    | T_uhyper _ ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "*to = Int64_val(from);@]@,";
        fprintf c "}@,@,"

    | T_option (T_refer_to (_, n)) ->
        fprintf h "void _of_%s(value from, %s *to);@," name name;

        fprintf c "@[<v 4>void _of_%s(value from, %s *to) {@," name name;
        fprintf c "CAMLparam1 (from);@,@,";
        fprintf c "@[<v 4>if (from == Val_int(0))@,";
        fprintf c "*to = NULL;@]@,";
        fprintf c "@[<v 4>else {@,";
        fprintf c "*to = malloc(sizeof(%s));@," !n;
        fprintf c "_of_%s(Field(from, 0), *to);@]@," !n;
        fprintf c "}@,";
        fprintf c "@,CAMLreturn0;@]@,";
        fprintf c "}@,@,"

    | _ -> fprintf c "/* @, * unknown type: %s@, */@,@," name

  in

  fprintf c "@,/* tags for struct types */@,";
  List.iter
    (function
      | Typedef td ->
          (match td.decl_type with
          | T_struct _ ->
              fprintf c "static int tag_%s;@," td.decl_symbol.xdr_name
          | _ -> ())
      | _ -> ())
    dl;

  fprintf h "@,/* functions to convert from C to OCaml types */@,";
  fprintf c "@,/* functions to convert from C to OCaml types */@,";
  List.iter
    (function
      | Typedef td ->
          output_to_type td.decl_symbol.xdr_name td.decl_type
      | _ -> ())
  dl;

  fprintf h "@,/* functions to convert from OCaml to C types */@,";
  fprintf c "@,/* functions to convert from OCaml to C types */@,";
  List.iter
    (function
      | Typedef td ->
          output_of_type td.decl_symbol.xdr_name td.decl_type
      | _ -> ())
  dl;

  fprintf h "@,/* type values of OCaml variant types used */@,";
  List.iter
    (fun name ->
      fprintf h "#define VARIANT_%s %d@," name (hash_variant name))
    (List.rev !constants);

  fprintf h "@,/* initialize tags, callback values, variant values */@,";
  fprintf h "CAMLprim value %s_init(value arg);@," base_name;

  fprintf c "/* initialize tags, callback values, variant values */@,";
  fprintf c "@[<v 4>CAMLprim value %s_init(value arg) {@," base_name;
  fprintf c "CAMLparam1 (arg);@,";
  let count = ref 0 in
  List.iter
    (function
      | Typedef td ->
          (match td.decl_type with
          | T_struct _ ->
              fprintf c "tag_%s = Tag_val(Field(arg, %d));@,"
                  td.decl_symbol.xdr_name !count;
              count := !count + 1
          | _ -> ())
      | _ -> ())
    dl;

  fprintf c "@,/* verify that the variant type values are correct */@,";
  List.iter
    (fun name ->
      fprintf c "@[<hov 4>assert(Int_val(hash_variant(\"%s\")) ==@ " name;
      fprintf c "VARIANT_%s);@]@," name)
    (List.rev !constants);
    
  fprintf c "@,CAMLreturn (Val_unit);@]@,";
  fprintf c "}@,";

  fprintf c "@]@\n";
  fprintf h "@]@\n"

let output_c_server (c:formatter) (dl:xdr_def list) csrvhn base_name =
  fprintf c "@[<v>";

  let output_proc name version argtype restype =
    fprintf c "@,%s *@," restype;
    fprintf c "%s_%s_svc(%s *argp, struct svc_req *rqstp)@,"
        name version argtype;
    fprintf c "@[<v 4>{@,";
    fprintf c "CAMLparam0 ();@,";
    fprintf c "CAMLlocal3 (cred, arg, res);@,";
    if restype = "void"
      then
        fprintf c "static char * result = NULL;@,"
      else
        fprintf c "static %s result;@," restype;
    fprintf c "static value * %s = NULL;@," name;
    fprintf c "@[<v 4>if (%s == NULL)@ " name;
    fprintf c "%s = caml_named_value(\"%s\");@]@," name name;
    if restype <> "void"
      then
        begin
          fprintf c "@[<v 4>else@ ";
          fprintf c "xdr_free((xdrproc_t) xdr_%s, (char *) &result);@]@,"
              restype
        end;
    fprintf c "assert(%s != NULL);@," name;
    fprintf c "cred = decode_credentials(rqstp);@,";
    if argtype = "void"
      then
        fprintf c "arg = Val_unit;@,"
      else
        fprintf c "arg = _to_%s(argp);@," argtype;
    fprintf c "res = callback2(*%s, cred, arg);@," name;
    if restype <> "void"
      then fprintf c "_of_%s(res, &result);@," restype;
    fprintf c "@[<v 4>if (checkFd())@,";
    fprintf c "printf(\"file descriptor leak in %s\\n\");@]@," name;
    fprintf c "CAMLreturn (&result);@]@,";
    fprintf c "}@,"

  in

  fprintf c "#include <rpc/xdr.h>@,";
  fprintf c "#include <assert.h>@,";
  fprintf c "#include <caml/memory.h>@,";
  fprintf c "#include <caml/mlvalues.h>@,";
  fprintf c "#include <caml/alloc.h>@,";
  fprintf c "#include <caml/callback.h>@,";
  fprintf c "#include \"%s.h\"@," base_name;
  fprintf c "#include \"%s\"@," csrvhn;
  fprintf c "@,extern value decode_credentials(struct svc_req *rqstp);@,";
  fprintf c "extern int checkFd(void);@,";
  
  List.iter
    (function
      | Progdef { prog_symbol = { xdr_name = program_name };
                  prog_def = version_list } ->
          fprintf c "@,/* Procedure definitions for %s */@," program_name;
          List.iter
            (function { version_symbol = { xdr_name = version_name };
                        version_def = proc_list;
                        version_number = version } ->
              let num = string_of_uint4 version in
              List.iter
                (function { proc_symbol = { xdr_name = proc_name };
                            proc_params = proc_param_list;
                            proc_result = proc_result_type } ->
                  let argtype =
                    (match proc_param_list with
                    | [param] ->
                        (match param with
                        | T_refer_to (_, n) -> !n
                        | T_void -> "void"
                        | _ ->
                            fprintf c "/* unknown arg type in %s */@,"
                                proc_name;
                            "unknown")
                    | _ ->
                        fprintf c "/* unknown multiple args in %s */@,"
                            proc_name;
                        "unknown")
                  in
                  let restype =
                    (match proc_result_type with
                    | T_refer_to (_, n) -> !n
                    | T_void -> "void"
                    | _ ->
                        fprintf c "/* unknown return type in %s */@," proc_name;
                        "unknown")
                  in
                  output_proc (String.lowercase proc_name) num argtype restype)
                proc_list)
            version_list
      | _ -> ())
    dl;

  fprintf c "@]@\n";
