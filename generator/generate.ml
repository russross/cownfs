(* $Id: generate.ml,v 1.12 2003/05/23 13:57:07 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Syntax;;
open Format;;


(* Common functions *)


let extract_type_info dl =
  let typenames = Hashtbl.create 100 in
  let typemap = Hashtbl.create 100 in
  List.iter
    (function
	 Typedef td ->
	   if not (Hashtbl.mem typenames td.decl_symbol.xdr_name) then begin
	     (* Only get the first type definition *)
	     Hashtbl.add 
	       typenames 
	       td.decl_symbol.xdr_name 
	       td.decl_symbol.ocaml_name;
	     Hashtbl.add
	       typemap
	       td.decl_symbol.xdr_name
	       td.decl_type
	   end
       | _ -> ()
    )
    dl;
  (typenames,typemap)
;;


let rec get_type_from_map typemap t =
  match t with
      T_refer_to (_,n) ->
	( try get_type_from_map typemap (Hashtbl.find typemap !n)
	  with
	      Not_found -> assert false
	)
    | t -> t
;;


let output_uint4_pattern f (sign,n) =
  assert (not sign);
  let (n1,n2,n3,n4) = Rtypes.dest_uint4 n in
  fprintf f "('\\%03d','\\%03d','\\%03d','\\%03d')"
            (Char.code n1) 
            (Char.code n2)
            (Char.code n3)
            (Char.code n4);
;;
 
let output_uint4 f (sign,n) =
  assert (not sign);
  fprintf f "(Rtypes.mk_uint4";
  output_uint4_pattern f (sign,n);
  fprintf f ")"
;;
    

let output_int4_pattern f (sign,n) =
  let plus1 (a,b,c,d) = 
    if d < 255 then 
      (a,b,c,d+1) 
    else
      if c < 255 then
	(a,b,c+1,0)
      else
	if b < 255 then
	  (a,b+1,0,0)
	else
	  if a < 255 then
	    (a+1,0,0,0)
	  else
	    (0,0,0,0)
  in
  let neg (a,b,c,d) =
    plus1 (255-a, 255-b, 255-c, 255-d)
  in
  let (n1,n2,n3,n4) = Rtypes.dest_uint4 n in
  let (m1,m2,m3,m4) =
    if sign then 
      neg (Char.code n1, Char.code n2, Char.code n3, Char.code n4) 
    else 
      (Char.code n1, Char.code n2, Char.code n3, Char.code n4) 
  in
  fprintf f "('\\%03d','\\%03d','\\%03d','\\%03d')"
            m1 m2 m3 m4
;;

let output_int4 f (sign,n) =
  fprintf f "(Rtypes.mk_int4";
  output_int4_pattern f (sign,n);
  fprintf f ")"
;;


let strip_enum_list l =
  (* Remove double enums, sort enums *)
  let constants = ref [] in
  let rec strip l =
    match l with
	(id, c) :: l' ->
	  let c' = constant !c in
	  if List.mem c' !constants then
	    strip l'
	  else begin
	    constants := c' :: !constants;
	    (id, c) :: strip l'
	  end
      | [] -> []
  in
  let cmp (id1,c1) (id2,c2) =
    let (sign1,v1) = constant !c1 in
    let (sign2,v2) = constant !c2 in
    match (sign1,sign2) with
	(false,false) ->
	  compare (Rtypes.int32_of_uint4 v1) (Rtypes.int32_of_uint4 v2)
      | (true,true) ->
	  -(compare (Rtypes.int32_of_uint4 v1) (Rtypes.int32_of_uint4 v2))
      | (false, true) ->
	  1
      | (true, false) ->
	  -1
  in
  List.sort cmp (strip l)
;;
	    

let n0 = constant_of_string "0";;
let n1 = constant_of_string "1";;

let enum_type t =
  match t with
      T_enum l -> strip_enum_list l
    | T_bool ->
	[ mk_mapped_id "FALSE" "False", ref(Constant n0);
	  mk_mapped_id "TRUE"  "True",  ref(Constant n1);
	]
    | _ ->
	assert false
;;

let values_of_enum_type t =
  match t with
      T_enum l ->
	List.map (fun (_,c) -> !c) (strip_enum_list l)
    | T_bool ->
	[ Constant n0; Constant n1 ]
    | _ ->
	assert false
;;


(**********************************************************************)
(* Output constant definitions                                        *)
(**********************************************************************)

let output_consts (mli:formatter) (f:formatter) (dl:xdr_def list) =

  let output_signed_const id c =
    (* MLI: *)
    fprintf mli "val %s : Rtypes.int4;;@\n" id.ocaml_name;
    (* ML: *)
    fprintf f "let %s = " id.ocaml_name;
    output_int4 f c;
    fprintf f ";;@\n"
  in

  let output_unsigned_const id c =
    (* MLI: *)
    fprintf mli "val %s : Rtypes.uint4;;@\n" id.ocaml_name;
    (* ML: *)
    fprintf f "let %s = " id.ocaml_name;
    output_uint4 f c;
    fprintf f ";;@\n"
  in

  let rec output_type t = (
    match t with
      | T_option t'         -> output_type t'
      | T_array_fixed(_,t') -> output_type t'
      | T_array(_,t')       -> output_type t'
      | T_array_unlimited t'-> output_type t'
      | T_enum l            -> List.iter 
                                 (fun (id,c) -> 
				    output_signed_const id (constant !c)
                                 ) 
	                         (strip_enum_list l)
      | T_struct td         -> List.iter output_type_decl td
      | T_union u           -> output_type_decl (u.discriminant);
                               List.iter (fun (_,_,td) -> 
                                            output_type_decl td) u.cases;
                               (match u.default with
                                    Some td -> output_type_decl td
                                  | None    -> ()
                               )
      | _                   -> ()
  )

  and output_type_decl td =
    output_type td.decl_type

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    List.iter output_type proc.proc_params;
    output_type proc.proc_result
  in

  fprintf f "@[<v>";
  fprintf mli "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_type_decl td
       | Progdef prog -> 
	   check_program prog
       | Constdef(id, (sign,c)) ->
	   if sign then
	     output_signed_const id (sign,c)
	   else
	     output_unsigned_const id (sign,c)
    )
    dl;

  fprintf f "@]";
  fprintf mli "@]"
;;


(**********************************************************************)
(* Output O'Caml type declarations                                    *)
(**********************************************************************)

let output_type_declarations (f:formatter) (dl:xdr_def list) =
  let typenames, typemap = extract_type_info dl in
      (* typenames: maps xdr_name to ocaml_name *)
      (* typemap: maps xdr_name to bound type *)
  let anontype = ref 0 in
  let deferred = Queue.create() in
  let firstdecl = ref true in

  let begin_decl() =
    if !firstdecl then
      fprintf f "type "
    else
      fprintf f "and ";
    firstdecl := false
  in

  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in
  

  let rec output_type t = (
    match t with
	T_opaque_fixed _ 
      | T_opaque _
      | T_opaque_unlimited
      | T_string _
      | T_string_unlimited ->
	  fprintf f "string"
      | T_option t' ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ option@]"
      | T_array_fixed(_,t') ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_array(_,t') ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_array_unlimited t' ->
          fprintf f "@[<hv 2>";
	  output_type t';
	  fprintf f "@ array@]"
      | T_int v ->
	  (match v with
	       Abstract-> fprintf f "Rtypes.int4"
	     | INT32   -> fprintf f "int32"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	  )
      | T_uint v ->
	  (match v with
	       Abstract-> fprintf f "Rtypes.uint4"
	     | INT32   -> fprintf f "int32"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	  )
      | T_hyper v ->
	  (match v with
	       Abstract-> fprintf f "Rtypes.int8"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	     | _       -> assert false
	  )
      | T_uhyper v ->
	  (match v with
	       Abstract-> fprintf f "Rtypes.uint8"
	     | INT64   -> fprintf f "int64"
	     | Unboxed -> fprintf f "int"
	     | _       -> assert false
	  )
      | T_double 
      | T_float ->
	  fprintf f "float"
      | T_bool ->
	  fprintf f "bool"
      | T_void ->
	  fprintf f "unit"
      | T_refer_to (_,s) ->
	  let n =
	    try Hashtbl.find typenames !s 
	    with Not_found -> assert false
	  in
	  fprintf f "%s" n
      | T_enum _ ->
	  fprintf f "Rtypes.int4"
      | T_struct tdl ->
	  let n = "_t" ^ string_of_int !anontype in
	  incr anontype;
	  Queue.add (n,t) deferred;
	  fprintf f "%s" n
      | T_union u ->
	  let discr_type = get_type_of_decl u.discriminant in
	  let make_tag c =
	    let (sign,absval) = constant c in
	    match discr_type with
		(T_int _|T_uint _) ->
		  (if sign then "__" else "_") ^ string_of_uint4 absval
	      | T_bool ->
		  assert(not sign);
		  ( match string_of_uint4 absval with
			"0" -> "False"
		      | "1" -> "True"
		      | _   -> assert false
		  )
	      | T_enum l ->
		  ( try
		      let id,_ =
			List.find
			  (fun (id,n) -> constant !n = (sign,absval))
			  l
		      in
		      id.ocaml_name
		    with
			Not_found -> assert false
		  )
	      | _ ->
		  assert false
	  in
	  let output_tag c om td =
	    let tag = 
	      match om with
		  None -> make_tag c
		| Some om_tag -> om_tag
	    in
	    if get_type_of_decl td = T_void then
	      fprintf f "@,| `%s " tag
	    else begin
	      fprintf f "@,| `%s of (" tag;
	      output_type td.decl_type;
	      fprintf f ") "
	    end
	  in

	  fprintf f "@[<hv>[ ";
	  List.iter (fun (c,om,td) -> output_tag !c om td) u.cases;
	  ( match u.default with
		None -> ()         (* TODO: Check! *)
	      | Some td ->
		  (* If the discriminant is countable, the missing cases are
		   * enumerated here. Otherwise, a "default" tag is generated.
		   *)
		  if match discr_type with T_int _ | T_uint _ -> true
		                                          | _ -> false
		  then begin
		    (* The default case is represented by a default tag *)
		    let tag = "default" in
		    fprintf f "@,| `%s of (" tag;
		    if get_type_of_decl td = T_void then
		      output_type u.discriminant.decl_type
		    else begin
		      fprintf f "(";
		      output_type u.discriminant.decl_type;
		      fprintf f ") * (";
		      output_type td.decl_type;
		      fprintf f ")";
		    end;
		    fprintf f ") ";
		  end
		  else begin
		    (* Iterate over all possible values of the discriminant: *)
		    let l = values_of_enum_type discr_type in
		    List.iter
		      (fun n ->
			 (* Find out the missing cases: *)
			 if not (List.exists (fun (c,_,_) -> !c = n)
				             u.cases) then begin
			   (* n is missing! *)
			   output_tag n None td
			 end
		      )
		      l
		  end
	  );
	  fprintf f "@,]@]"
  )
	
  and output_declaration n t = (
    fprintf f "@[<hov 6>";
    begin_decl();
    fprintf f "%s = @\n" n;
    (match t with
	T_struct tdl ->
	  fprintf f "@[<hov>{ ";
	  List.iter
	    (fun td' ->
	       if td'.decl_symbol.xdr_name <> "" then begin
		 fprintf f "@\n  mutable %s : @[<b 4>@," td'.decl_symbol.ocaml_name;
		 output_type td'.decl_type;
		 fprintf f "@];";
	       end
		 (* else: td' is a void component *)
	    )
	    tdl;
	  fprintf f "@\n}@]";
      | t ->
	  output_type t);
    fprintf f "@]@\n";
  )

  and output_tuple_declaration n args = (
    fprintf f "@[<hov 6>";
    begin_decl();
    fprintf f "%s = @\n" n;
    fprintf f "(@[<hv> ";
    let isfirst = ref true in
    List.iter
      (fun arg ->
	 if not !isfirst then fprintf f " *@ ";
	 isfirst := false;
	 output_type arg;
      )
      args;
    fprintf f " )@]";
    fprintf f "@]@\n";
  )

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^ 
	      proc.proc_symbol.ocaml_name in

    ( match proc.proc_params with
	  [] -> assert false
	| [arg] -> 
	    output_declaration 
	      ("t_" ^ pvp ^ "'arg")
	      arg
	| args ->
	    output_tuple_declaration
	      ("t_" ^ pvp ^ "'arg")
	      args
    );
    output_declaration 
      ("t_" ^ pvp ^ "'res")
      proc.proc_result

  and output_deferred_structs() = (
    try
      while true do
	let (n,t) = Queue.take deferred in
	output_declaration n t
      done
    with
	Queue.Empty -> ()
  )
  in

  fprintf f "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_declaration td.decl_symbol.ocaml_name td.decl_type
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  output_deferred_structs();

  if not !firstdecl then fprintf f ";;@\n";

  fprintf f "@]"
;;

(**********************************************************************)
(* Output XDR type definition                                         *)
(**********************************************************************)

let output_xdr_type (mli:formatter) (f:formatter) (dl:xdr_def list) = 
  let typenames, typemap = extract_type_info dl in
    (* typenames: maps xdr_name to ocaml_name *)
    (* typemap: maps xdr_name to bound type *)
 
  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in

  let generated_types = ref [] in

  let rec output_type rectypes t = (
    match t with
	T_opaque_fixed n ->
	  fprintf f "@[<hv 2>Xdr.X_opaque_fixed@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_opaque n ->
	  fprintf f "@[<hv 2>Xdr.X_opaque@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_opaque_unlimited ->
	  fprintf f "Xdr.x_opaque_max"
      | T_string n ->
	  fprintf f "@[<hv 2>Xdr.X_string@ ";
	  output_uint4 f (constant !n);
	  fprintf f "@]";
      | T_string_unlimited ->
	  fprintf f "Xdr.x_string_max"
      | T_option t' ->
	  fprintf f "@[<hv 2>Xdr.x_optional@ (";
	  output_type rectypes t';
	  fprintf f ")@]";
      | T_array_fixed(n,t') ->
	  fprintf f "@[<hv 2>Xdr.X_array_fixed(@,";
	  output_type rectypes t';
	  fprintf f ",@ ";
	  output_uint4 f (constant !n);
	  fprintf f ")@]";
      | T_array(n,t') ->
	  fprintf f "@[<hv 2>Xdr.X_array(@,";
	  output_type rectypes t';
	  fprintf f ",@ ";
	  output_uint4 f (constant !n);
	  fprintf f ")@]";
      | T_array_unlimited t' ->
	  fprintf f "@[<hv>Xdr.x_array_max@ (";
	  output_type rectypes t';
	  fprintf f ")@]";
      | T_int _ ->
	  fprintf f "Xdr.X_int"
      | T_uint _ ->
	  fprintf f "Xdr.X_uint"
      | T_hyper _ ->
	  fprintf f "Xdr.X_hyper"
      | T_uhyper _ ->
	  fprintf f "Xdr.X_uhyper"
      | T_double ->
	  fprintf f "Xdr.X_double"
      | T_float ->
	  fprintf f "Xdr.X_float"
      | T_bool ->
	  fprintf f "Xdr.x_bool"
      | T_void ->
	  fprintf f "Xdr.X_void"
      | T_refer_to (_,s) ->
	  if List.mem !s !generated_types then begin
	    (* There was already a complete definition for this type *)
	    let n =
	      try Hashtbl.find typenames !s 
	      with Not_found -> assert false
	    in
	    fprintf f "xdrt_%s" n
	  end
	  else if List.mem !s rectypes then begin
	    (* There was already the beginning of a definition for this type: *)
	    fprintf f "Xdr.X_refer \"%s\"" !s
	  end
	  else begin
	    let t' = get_type t in
	    fprintf f "@[<hv 2>Xdr.X_rec(\"%s\",@ " !s;
	    output_type (!s :: rectypes) t';
	    fprintf f ")@]";
	  end
      | T_enum l ->
	  fprintf f "@[<hv 2>Xdr.X_enum@ [@ ";
	  List.iter
	    (fun (id,c) ->
	       fprintf f "  (\"%s\", " id.xdr_name;
	       output_int4 f (constant !c);
	       fprintf f ");@ ";
	    )
	    (strip_enum_list l);
	  fprintf f "]@]";
      | T_struct tdl ->
	  fprintf f "@[<hv 2>Xdr.X_struct@ @[<hv>[@ ";
	  List.iter
	    (fun d ->
	       if d.decl_type <> T_void then begin
		 fprintf f "  @[<hv 2>(\"%s\",@ (" d.decl_symbol.xdr_name;
		 output_type rectypes d.decl_type;
		 fprintf f "));@]@ ";
	       end
	    )
	    tdl;
	  fprintf f "]@]@]";
      | T_union u ->
	    let discr_type = get_type_of_decl u.discriminant in
	    if match discr_type with T_int _ | T_uint _ -> true
                                                    | _ -> false
	    then begin
	      (* Unions of integers *)
	      let constr, printint = 
		match discr_type with
		    T_int _  -> "Xdr.X_union_over_int",  output_int4
		  | T_uint _ -> "Xdr.X_union_over_uint", output_uint4
		  | _        -> assert false
	      in
	      fprintf f "@[<hv 2>";
	      fprintf f "%s(" constr;
	      fprintf f "@[<hv>[@ ";
	      List.iter
		(fun (c, _, d) ->
		   fprintf f "  @[<hv 2>";
		   printint f (constant !c);
		   fprintf f ",@ (";
		   output_type rectypes d.decl_type;
		   fprintf f ");@]@ ";
		)
		u.cases;
	      fprintf f "],@ ";
	      begin match u.default with
		  None ->
		    fprintf f "None"
		| Some d ->
		    fprintf f "Some(";
		    output_type rectypes d.decl_type;
		    fprintf f ")"
	      end;
	      fprintf f ")@]@]";
	    end
	    else begin
	      (* Unions of enumerators (and bools) *)
	      fprintf f "@[<hv 2>";
	      fprintf f "Xdr.X_union_over_enum(@,(";
	      output_type rectypes discr_type;
	      fprintf f "),@ [@ ";
	      let l = enum_type discr_type in
	      List.iter
		(fun (c, _, d) ->
		   let name, _ =
		     try
		       List.find
			 (fun (id, c') -> !c' = !c)
			 l
		     with Not_found -> assert false
		   in
		   fprintf f "  @[<hv 2>\"%s\"" name.xdr_name;
		   fprintf f ",@ (";
		   output_type rectypes d.decl_type;
		   fprintf f ");@]@ ";
		)
		u.cases;
    	      fprintf f "],@ ";
	      begin match u.default with
		  None ->
		    fprintf f "None"
		| Some d ->
		    fprintf f "Some(";
		    output_type rectypes d.decl_type;
		    fprintf f ")"
	      end;
	      fprintf f ")@]@]";
	    end
  )

  and output_xdr_declaration n t =
    (* MLI: *)
    fprintf mli "val %s : Xdr.xdr_type_term;;@\n" n;
    (* ML: *)
    fprintf f "@[<hv 2>let %s =@ " n;
    (* fprintf f "@[<hv 2>Xdr.validate_xdr_type@ ("; *)
    output_type [] t;
    (* fprintf f ")@]"; *)
    fprintf f "@]@\n;;@\n"

  and output_xdr_tuple_declaration n tl =
    (* MLI: *)
    fprintf mli "val %s : Xdr.xdr_type_term;;@\n" n;
    (* ML: *)
    fprintf f "@[<hv 2>let %s =@ " n;
    (* fprintf f "@[<hv 2>Xdr.validate_xdr_type@ ("; *)
    fprintf f "@[<hv 2>Xdr.X_struct@ @[<hv>[@ ";
    let k = ref 0 in
    List.iter
      (fun t ->
	 fprintf f "  (\"%s\", " (string_of_int !k);
	 output_type [] t;
	 fprintf f ");@ ";
	 incr k;
      )
      tl;
    fprintf f "]@]@]";
    (* fprintf f ")@]"; *)
    fprintf f "@]@\n;;@\n";

  and check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^ 
	      proc.proc_symbol.ocaml_name in
    
    ( match proc.proc_params with
	  [] -> assert false
	| [arg] -> 
	    output_xdr_declaration 
	      ("xdrt_" ^ pvp ^ "'arg")
	      arg
	| args ->
	    output_xdr_tuple_declaration
	      ("xdrt_" ^ pvp ^ "'arg")
	      args
    );
    output_xdr_declaration 
      ("xdrt_" ^ pvp ^ "'res")
      proc.proc_result
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_xdr_declaration 
	     ("xdrt_" ^ td.decl_symbol.ocaml_name)
	     (T_refer_to (R_any, ref td.decl_symbol.xdr_name));
	   generated_types := td.decl_symbol.xdr_name :: !generated_types
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]";
  fprintf f "@]"
;;


(**********************************************************************)
(* Output conversion functions                                        *)
(**********************************************************************)

let output_conversions (mli:formatter) (f:formatter) (dl:xdr_def list) =

  (* Names of conversions:
   * - For every named type t there are two conversion functions:
   *   _to_<t> : value -> t
   *   _of_<t> : t -> value
   * - For every procedure argument and procedure result, there are
   *   such functions, too:
   *   _to_<prog>'<vers>'<proc>'arg
   *   _to_<prog>'<vers>'<proc>'res
   *   _of_<prog>'<vers>'<proc>'arg
   *   _of_<prog>'<vers>'<proc>'res
   *   Here, <prog>, <vers>, and <proc> are the names of the program, the
   *   version, and the procedure, resp. The character ' is used as
   *   delimiter
   *)

  let typenames, typemap = extract_type_info dl in
      (* typenames: maps xdr_name to ocaml_name *)
      (* typemap: maps xdr_name to bound type *)

  let get_type t = get_type_from_map typemap t in
  let get_type_of_decl td = get_type td.decl_type in

  let rec output_toconv_for_type (var:string) (t:xdr_type) = (
    (* Generates an expression that converts the xdr_value variable var
     * into the O'Caml value corresponding to t
     *)
    fprintf f "@[<hv>";

    ( match t with
	| T_void ->
	    fprintf f "()"
	| T_opaque_fixed _
	| T_opaque _ 
	| T_opaque_unlimited ->
	    fprintf f "(Xdr.dest_xv_opaque %s)" var
	| T_string _
	| T_string_unlimited ->
	    fprintf f "(Xdr.dest_xv_string %s)" var
	| T_option t' ->
	    fprintf f "@[<hv>";
	    fprintf f "(match Xdr.dest_xv_union_over_enum_fast %s with@ " var;
	    fprintf f "| (0, _) -> None@ ";
	    fprintf f "| (1, x) -> Some ";
	    output_toconv_for_type "x" t';
	    fprintf f "@ ";
	    fprintf f "| _ -> assert false";
	    fprintf f                          ")";
	    fprintf f "@]";
	| T_array_fixed(_,t') ->
	    output_toconv_for_array var t'
	| T_array(_,t') ->
	    output_toconv_for_array var t'
	| T_array_unlimited t' ->
	    output_toconv_for_array var t'
	| T_int Abstract ->
	    fprintf f "(Xdr.dest_xv_int %s)" var
	| T_int INT32 ->
	    fprintf f "(Rtypes.int32_of_int4 (Xdr.dest_xv_int %s))" var
	| T_int INT64 ->
	    fprintf f "(Rtypes.int64_of_int4 (Xdr.dest_xv_int %s))" var
	| T_int Unboxed ->
	    fprintf f "(Rtypes.int_of_int4 (Xdr.dest_xv_int %s))" var
	| T_uint Abstract ->
	    fprintf f "(Xdr.dest_xv_uint %s)" var
	| T_uint INT32 ->
	    fprintf f "(Rtypes.logical_int32_of_uint4 (Xdr.dest_xv_uint %s))" var
	| T_uint INT64 ->
	    fprintf f "(Rtypes.int64_of_uint4 (Xdr.dest_xv_uint %s))" var
	| T_uint Unboxed ->
	    fprintf f "(Rtypes.int_of_uint4 (Xdr.dest_xv_uint %s))" var
	| T_hyper Abstract ->
	    fprintf f "(Xdr.dest_xv_hyper %s)" var
	| T_hyper INT64 ->
	    fprintf f "(Rtypes.int64_of_int8 (Xdr.dest_xv_hyper %s))" var
	| T_hyper Unboxed ->
	    fprintf f "(Rtypes.int_of_int8 (Xdr.dest_xv_hyper %s))" var
	| T_hyper _ -> assert false
	| T_uhyper Abstract ->
	    fprintf f "(Xdr.dest_xv_uhyper %s)" var
	| T_uhyper INT64 ->
	    fprintf f "(Rtypes.logical_int64_of_uint8 (Xdr.dest_xv_uhyper %s))" var
	| T_uhyper Unboxed ->
	    fprintf f "(Rtypes.int_of_uint8 (Xdr.dest_xv_uhyper %s))" var
	| T_uhyper _ -> assert false
	| T_double ->
	    fprintf f "(Rtypes.float_of_fp8 (Xdr.dest_xv_double %s))" var
	| T_float ->
	    fprintf f "(Rtypes.float_of_fp4 (Xdr.dest_xv_float %s))" var
	| T_bool ->
	    fprintf f "(Xdr.dest_xv_enum_fast %s = 1)" var
	| T_refer_to (_,n) ->
	    let ocaml_n = 
	      try Hashtbl.find typenames !n
	      with Not_found -> assert false
	    in
	    fprintf f "(_to_%s %s)" ocaml_n var
	| T_enum l ->
	    fprintf f "@[<hv>";
	    fprintf f "(match Xdr.dest_xv_enum_fast %s with@ " var;
	    let k = ref 0 in
	    List.iter
	      (fun (id,c) ->
		 fprintf f "@[<hv>";
		 fprintf f "| %d ->@;<1 4>" !k;
		 output_int4 f (constant !c);
		 fprintf f "@]";
		 fprintf f "@ ";
		 incr k
	      )
	      (strip_enum_list l);
	    fprintf f "| _ -> assert false@ ";
	    fprintf f ")";
	    fprintf f "@]";
	| T_struct tl ->
	    fprintf f "@[<hv>";
	    fprintf f "(let s = Xdr.dest_xv_struct_fast %s in@;<1 3>" var;
	    fprintf f "{ @[<hv>";
	    let isfirst = ref true in
	    let k = ref 0 in
	    List.iter
	      (fun d ->
		 if d.decl_type <> T_void then begin
		   if not !isfirst then fprintf f "@,";
		   isfirst:= false;
		   let ocaml_n = d.decl_symbol.ocaml_name in
		   (* let xdr_n   = d.decl_symbol.xdr_name in *)
		   fprintf f "%s = " ocaml_n;
		   fprintf f "@[<hv>";
		   fprintf f "(fun x -> ";
		   output_toconv_for_type "x" d.decl_type;
		   fprintf f ")@ s.(%d)" !k;
		   fprintf f "@]";
		   fprintf f "; ";
		   incr k
		 end
	      )
	      tl;
	    fprintf f "@]@;<0 3>})";
	    fprintf f "@]";
	| T_union u ->
	    let discr_type = get_type_of_decl u.discriminant in
	    if match discr_type with T_int _ | T_uint _ -> true
	                                            | _ -> false
	    then begin
	      (* Unions of integers *)
	      let dest1, dest2, printint_pattern = 
		match discr_type with
		    T_int _  -> "Rtypes.dest_int4",  
		                "Xdr.dest_xv_union_over_int",
			        output_int4_pattern
		  | T_uint _ -> "Rtypes.dest_uint4", 
		                "Xdr.dest_xv_union_over_uint",
			        output_uint4_pattern
		  | _        -> assert false
	      in
	      fprintf f "@[<hv>";
	      fprintf f "(let discriminant, value = %s %s in@ " dest2 var;
	      fprintf f "match %s discriminant, value with" dest1;
	      List.iter
		(fun (c, om, d) ->
		   let (sign,n) = constant !c in
		   let tag =
		     match om with
			 None ->
			   (if sign then "__" else "_") ^ string_of_uint4 n
		       | Some om_tag -> om_tag
		   in
		   let (n1,n2,n3,n4) = Rtypes.dest_uint4 n in
		   fprintf f "@ @[<hv>";
		   fprintf f "| (";
		   printint_pattern f (sign,n);
		   fprintf f ", x) ->@;<1 6>";
		   fprintf f "`%s " tag;
		   if get_type_of_decl d <> T_void then 
		     output_toconv_for_type "x" d.decl_type;
		   fprintf f "@]"
		)
		u.cases;
	      begin match u.default with
		  None -> 
		    fprintf f "@ | _ -> assert false";
		| Some d ->
		    fprintf f "@ @[<hv>";
		    fprintf f "| (_, x) ->@;<1 6>";
		    let int_conversion = 
		      match discr_type with
			  T_int Abstract  -> ""
			| T_int INT32     -> "Rtypes.int32_of_int4 "
			| T_int INT64     -> "Rtypes.int64_of_int4 "
			| T_int Unboxed   -> "Rtypes.int_of_int4 "
			| T_uint Abstract -> ""
			| T_uint INT32    -> "Rtypes.logical_int32_of_uint4 "
			| T_uint INT64    -> "Rtypes.int64_of_uint4 "
			| T_uint Unboxed  -> "Rtypes.int_of_uint4 "
			| _ -> assert false
		    in
		    fprintf f "`default(@[<hv>%sdiscriminant" int_conversion;
		    if get_type_of_decl d <> T_void then begin
		      fprintf f ",@ ";
		      output_toconv_for_type "x" d.decl_type;
		    end;
		    fprintf f "@])";
		    fprintf f "@]"
	      end;
	      (* Add a type coercion: *)
	      fprintf f "@ :> @[<hv>[";
	      List.iter
		(fun (c, om, d) ->
		   let (sign,n) = constant !c in
		   let tag =
		     match om with
			 None ->
			   (if sign then "__" else "_") ^ string_of_uint4 n 
		       | Some om_tag -> om_tag
		   in
		   fprintf f "@ | `%s" tag;
		   if get_type_of_decl d <> T_void then 
		     fprintf f " of _";
		)
		u.cases;
	      begin match u.default with
		  None ->
		    ()
		| Some d ->
		    fprintf f "@ | `default of ";
		    if get_type_of_decl d <> T_void then 
		      fprintf f "(_ * _)"
		    else
		      fprintf f "_"
	      end;
	      fprintf f "@ ]@]@ ";
	      fprintf f ")";
	      fprintf f "@]";
	    end
	    else begin
	      (* Unions of enumerators (and bools) *)
	      fprintf f "@[<hv>";
	      fprintf f "(";
	      let have_mkdefault = ref false in
	      ( match u.default with
		    None -> 
		      ()
		  | Some d -> 
		      if get_type_of_decl d <> T_void then begin
			fprintf f "let mkdefault x =@;<1 4>";
			fprintf f "@[<hv>";
			output_toconv_for_type "x" d.decl_type;
			fprintf f "@]";
			fprintf f " in@ ";
			have_mkdefault := true;
		      end
	      );
	      fprintf f "match Xdr.dest_xv_union_over_enum_fast %s with" var;
	      let l = enum_type discr_type in
	      let k = ref 0 in
	      List.iter
		(fun (id,c) ->
		   let om, d =
		     try
		       let c, om, d =
			 (List.find
			    (fun (c',om',d') -> !c' = !c)
			    u.cases
			 ) in
		       om, Some d
		     with
			 Not_found -> None, None
		   in
		   if d <> None || u.default <> None then begin
		     let tag =
		       match om with
			   None -> id.ocaml_name
			 | Some om_tag -> om_tag
		     in
		     fprintf f "@ @[<hv>";
		     fprintf f "| (%d, x) ->@;<1 6>" !k;
		     fprintf f "`%s " tag;
		     match d with
			 Some dd ->
			   if get_type_of_decl dd <> T_void then 
			     output_toconv_for_type "x" dd.decl_type;
		       | None ->
			   if !have_mkdefault then
			     fprintf f "(mkdefault x)"
		   end;
		   fprintf f "@]";
		   incr k
		)
		l;
	      fprintf f "@ | _ -> assert false";
	      (* Add a type coercion: *)
	      fprintf f "@ :> @[<hv>[";
	      List.iter
		(fun (id, c) ->
		   begin try
		     let _, om, d =
		       (List.find
			  (fun (c',_,d') -> !c' = !c)
			  u.cases
		       )
		     in
		     let tag = match om with
			 None -> id.ocaml_name
		       | Some om_tag -> om_tag
		     in
		     fprintf f "@ | `%s" tag;
		     if get_type_of_decl d <> T_void then 
		       fprintf f " of _";
		   with
		       Not_found ->
			 if u.default <> None then begin
			   fprintf f "@ | `%s" id.ocaml_name;
			   if !have_mkdefault then
			     fprintf f " of _"
			 end
		   end;
		)
		l;
	      fprintf f "@ ]@]@ ";
	      fprintf f ")";
	      fprintf f "@]";
	    end
    );
    fprintf f "@]"
  )

  and output_toconv_for_array var t' =
    fprintf f "@[<hv>";
    fprintf f "(Array.map@;<1 2>(fun x -> ";
    output_toconv_for_type "x" t';
    fprintf f ")@ (Xdr.dest_xv_array %s))" var;
    fprintf f "@]";

  and output_toconv_for_tuple var tl =
    fprintf f "@[<hv>";
    fprintf f "(let s = Xdr.dest_xv_struct_fast %s in@;<1 3>" var;
    fprintf f "( @[<hv>";
    let isfirst = ref true in
    let n = ref 0 in
    List.iter
      (fun t ->
	 if not !isfirst then fprintf f ", @,";
	 isfirst:= false;
	 fprintf f "@[<hv>";
	 fprintf f "(fun x -> ";
	 output_toconv_for_type "x" t;
	 fprintf f ")@ s.(%d)" !n;
	 fprintf f "@]";
	 incr n;
      )
      tl;
    fprintf f "@]@;<0 3>))";
    fprintf f "@]";
  in

  let firstdecl = ref true in

  let begin_decl() =
    if !firstdecl then
      fprintf f "let rec "
    else
      fprintf f "and ";
    firstdecl := false
  in

  let output_toconv_declaration n t tname =
    (* MLI: *)
    fprintf mli "val _to_%s : Xdr.xdr_value -> %s;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_to_%s (x:Xdr.xdr_value) : %s =@;<1 2>" 
      n
      tname;
    output_toconv_for_type "x" t;
    fprintf f "@]@\n"
  in

  let output_toconv_tuple_declaration n tl tname =
    (* MLI: *)
    fprintf mli "val _to_%s : Xdr.xdr_value -> %s;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_to_%s (x:Xdr.xdr_value) : %s =@;<1 2>" 
      n
      tname;
    output_toconv_for_tuple "x" tl;
    fprintf f "@]@\n"
  in

  let rec output_ofconv_for_type (name:string) (var:string) (t:xdr_type) = (
    (* Generates an expression converting the O'Caml value contained in the
     * variable with name var to the corresponding XDR value
     *)
    fprintf f "@[<hv>";
    ( match t with
	| T_void ->
	    fprintf f "Xdr.XV_void"
	| T_opaque_fixed _
	| T_opaque _
	| T_opaque_unlimited ->
	    fprintf f "(Xdr.XV_opaque %s)" var
	| T_string _
	| T_string_unlimited ->
	    fprintf f "(Xdr.XV_string %s)" var
	| T_option t' ->
	    fprintf f "@[<hv>";
	    fprintf f "(match %s with@ " var;
	    fprintf f "| None   -> Xdr.xv_none@ ";
	    fprintf f "| Some x -> @[<hv 2>Xdr.xv_some@ ";
	    output_ofconv_for_type name "x" t';
	    fprintf f "@]@ ";
	    fprintf f ")@]";
	| T_array_fixed(_,t') ->
	    output_ofconv_for_array name  var t'
	| T_array(_,t') ->
	    output_ofconv_for_array name var t'
	| T_array_unlimited t' ->
	    output_ofconv_for_array name var t'
	| T_int Abstract ->
	    fprintf f "(Xdr.XV_int %s)" var
	| T_int INT32 ->
	    fprintf f "(Xdr.XV_int (Rtypes.int4_of_int32 %s))" var
	| T_int INT64 ->
	    fprintf f "(Xdr.XV_int (Rtypes.int4_of_int64 %s))" var
	| T_int Unboxed ->
	    fprintf f "(Xdr.XV_int (Rtypes.int4_of_int %s))" var
	| T_uint Abstract ->
	    fprintf f "(Xdr.XV_uint %s)" var
	| T_uint INT32 ->
	    fprintf f "(Xdr.XV_uint (Rtypes.logical_uint4_of_int32 %s))" var
	| T_uint INT64 ->
	    fprintf f "(Xdr.XV_uint (Rtypes.uint4_of_int64 %s))" var
	| T_uint Unboxed ->
	    fprintf f "(Xdr.XV_uint (Rtypes.uint4_of_int %s))" var
	| T_hyper Abstract ->
	    fprintf f "(Xdr.XV_hyper %s)" var
	| T_hyper INT64 ->
	    fprintf f "(Xdr.XV_hyper (Rtypes.int8_of_int64 %s))" var
	| T_hyper Unboxed ->
	    fprintf f "(Xdr.XV_hyper (Rtypes.int8_of_int %s))" var
	| T_hyper _ -> assert false
	| T_uhyper Abstract ->
	    fprintf f "(Xdr.XV_uhyper %s)" var
	| T_uhyper INT64 ->
	    fprintf f "(Xdr.XV_uhyper (Rtypes.logical_uint8_of_int64 %s))" var
	| T_uhyper Unboxed ->
	    fprintf f "(Xdr.XV_uhyper (Rtypes.uint8_of_int %s))" var
	| T_uhyper _ -> assert false
	| T_double ->
	    fprintf f "(Xdr.XV_double (Rtypes.fp8_of_float %s))" var
	| T_float ->
	    fprintf f "(Xdr.XV_float (Rtypes.fp4_of_float %s))" var
	| T_bool ->
	    fprintf f "(if %s then Xdr.xv_true else Xdr.xv_false)" var
	| T_refer_to (_,n) ->
	    let ocaml_n = 
	      try Hashtbl.find typenames !n
	      with Not_found -> assert false
	    in
	    fprintf f "(_of_%s %s)" ocaml_n var
	| T_enum l ->
	    fprintf f "@[<hv>";
	    fprintf f "(match Rtypes.dest_int4 %s with@ " var;
	    let k = ref 0 in
	    List.iter
	      (fun (id,c) ->
		 fprintf f "@[<hv>";
		 fprintf f "| ";
		 output_int4_pattern f (constant !c);
		 fprintf f "@ -> Xdr.XV_enum_fast %d" !k;
		 fprintf f "@]";
		 fprintf f "@ ";
		 incr k;
	      )
	      (strip_enum_list l);
	    fprintf f "| _ -> failwith \"RPC/XDR error: invalid enum value for type `%s'\"@ " name;
	    fprintf f ")";
	    fprintf f "@]";
	| T_struct tdl ->
	    fprintf f "@[<hv>(@[<hv 2>Xdr.XV_struct_fast@ ";
	    fprintf f "[|@ ";
	    List.iter
	      (fun d ->
		 if d.decl_type <> T_void then begin
		   let ocaml_n = d.decl_symbol.ocaml_name in
		   let xdr_n   = d.decl_symbol.xdr_name in
		   fprintf f "  @[<hv 2>(";
		   fprintf f "let x = %s.%s in@ " var ocaml_n;
		   output_ofconv_for_type name "x" d.decl_type;
		   fprintf f ")@];@ ";
		 end
	      )
	      tdl;
	    fprintf f "|]@])@]"
	| T_union u ->
	    let discr_type = get_type_of_decl u.discriminant in
	    if match discr_type with T_int _ | T_uint _ -> true
	                                            | _ -> false
	    then begin
	      (* Unions of integers *)
	      let dest, constr, printint = 
		match discr_type with
		    T_int _  -> "Rtypes.dest_int4",  
		                "Xdr.XV_union_over_int",
			        output_int4
		  | T_uint _ -> "Rtypes.dest_uint4", 
		                "Xdr.XV_union_over_uint",
			        output_uint4
		  | _        -> assert false
	      in
	      fprintf f "@[<hv>";
	      fprintf f "(match %s with" var;
	      List.iter
		(fun (c,om,d) ->
		   let (sign,n) = constant !c in
		   let tag =
		     match om with
			 None ->
			   (if sign then "__" else "_") ^ string_of_uint4 n
		       | Some om_tag -> om_tag
		   in
		   fprintf f "@ @[<hv 6>| `%s " tag;
		   if get_type_of_decl d <> T_void then fprintf f "x ";
		   fprintf f "->@ ";
		   fprintf f "%s(@[<hv>" constr;
		   printint f (sign,n);
		   fprintf f ",@ ";
		   if get_type_of_decl d = T_void then
		     fprintf f "Xdr.XV_void"
		   else
		     output_ofconv_for_type name "x" d.decl_type;
		   fprintf f "@])";
		   fprintf f "@]";
		)
		u.cases;
	      begin match u.default with
		  None ->
		    ()
		| Some d ->
		    fprintf f "@ @[<hv 6>| ";
		    let int_conversion = 
		      match discr_type with
			  T_int Abstract  -> ""
			| T_int INT32     -> "Rtypes.int4_of_int32 "
			| T_int INT64     -> "Rtypes.int4_of_int64 "
			| T_int Unboxed   -> "Rtypes.int4_of_int "
			| T_uint Abstract -> ""
			| T_uint INT32    -> "Rtypes.logical_uint4_of_int32 "
			| T_uint INT64    -> "Rtypes.int4_of_uint64 "
			| T_uint Unboxed  -> "Rtypes.int4_of_uint "
			| _ -> assert false
		    in
		    if get_type_of_decl d <> T_void then begin
		      fprintf f "`default(discriminant,value) ->@ ";
		      fprintf f "let x = ";
		      output_ofconv_for_type name "value" d.decl_type;
		      fprintf f " in@ ";
		      fprintf f "%s(%sdiscriminant, x)@]" constr int_conversion;
		    end
		    else begin
		      fprintf f "`default discriminant ->@ ";
		      fprintf f "%s(%sdiscriminant, Xdr.XV_void)@]" constr int_conversion
		    end
	      end;
	      fprintf f "@ )@]";
	    end
	    else begin
	      (* Unions of enumerators (and bools) *)
	      fprintf f "@[<hv>";
	      fprintf f "(";
	      let have_mkdefault = ref false in
	      let default_is_void = ref false in
	      ( match u.default with
		    None ->
		      ()
		  | Some d -> 
		      if get_type_of_decl d <> T_void then begin
			fprintf f "let mkdefault x =@;<1 4>";
			fprintf f "@[<hv>";
			output_ofconv_for_type name "x" d.decl_type;
			fprintf f "@]";
			fprintf f " in@ ";
			have_mkdefault := true;
		      end
		      else default_is_void := true
	      );
	      fprintf f "match %s with" var;
	      let l = enum_type discr_type in
	      let k = ref 0 in
	      List.iter
		(fun (id,c) ->
		   let om, d =
		     try
		       let _, om, d =
			 (List.find
			    (fun (c',_,d') -> !c' = !c)
			    u.cases
			 )
		       in
		       om, Some d
		     with
			 Not_found -> None, None
		   in
		   if d <> None || u.default <> None then begin
		     let tag = match om with
			 None -> id.ocaml_name 
		       | Some om_tag -> om_tag
		     in
		     fprintf f "@ @[<hv 6>";
		     fprintf f "| `%s " tag;

		     begin match d with
			 None ->
			   if not !default_is_void then fprintf f "x "
		       | Some dd ->
			   if get_type_of_decl dd <> T_void then fprintf f "x "
		     end;

		     fprintf f "->@ ";
		     fprintf f "@[<hv 2>Xdr.XV_union_over_enum_fast(%d,@ " !k;
		     
		     begin match d with
			 None ->
			   if !have_mkdefault then
			     fprintf f "(mkdefault x)"
			   else
			     fprintf f "Xdr.XV_void"
		       | Some dd ->
			   if get_type_of_decl dd <> T_void then 
			     output_ofconv_for_type name "x" dd.decl_type
			   else
			     fprintf f "Xdr.XV_void"
		     end;
		     
		     fprintf f ")@]@]"
		   end;
		   incr k
		)
		l;
	      fprintf f ")@]"
	    end
    );
    fprintf f "@]"
  )
  and output_ofconv_for_array name var t' =
    fprintf f "@[<hv 2>Xdr.XV_array@ ";
    fprintf f "@[<hv 2>(Array.map@ ";
    fprintf f "(fun x -> ";
    output_ofconv_for_type name "x" t';
    fprintf f ")@ %s)@]@]" var;

  and output_ofconv_for_tuple name var tl =
    fprintf f "@[<hv 1>";
    fprintf f "(let (";
    let n = ref 0 in
    let isfirst = ref true in
    List.iter
      (fun t ->
	 if not !isfirst then fprintf f ", ";
	 isfirst := false;
	 fprintf f "x%d" !n;
	 incr n
      )
      tl;
    fprintf f ") = %s in@ " var;
    fprintf f "@[<hv 2>Xdr.XV_struct_fast@ [|@ ";
    n := 0;
    List.iter
      (fun t ->
	 fprintf f "  @[<hv 2>(";
	 output_ofconv_for_type name ("x" ^ string_of_int !n) t;
	 fprintf f ");@]@ ";
	 incr n
      )
      tl;
    fprintf f "|]@]@ )@]"
  in

  let output_ofconv_declaration n t tname =
    (* MLI: *)
    fprintf mli "val _of_%s : %s -> Xdr.xdr_value;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_of_%s (x:%s) : Xdr.xdr_value =@;<1 2>" 
      n
      tname;
    output_ofconv_for_type n "x" t;
    fprintf f "@]@\n"
  in

  let output_ofconv_tuple_declaration n tl tname =
    (* MLI: *)
    fprintf mli "val _of_%s : %s -> Xdr.xdr_value;;@\n" n tname;
    (* ML: *)
    fprintf f "@[<hv>";
    begin_decl();
    fprintf f "_of_%s (x:%s) : Xdr.xdr_value =@;<1 2>" 
      n
      tname;
    output_ofconv_for_tuple n "x" tl;
    fprintf f "@]@\n"
  in

  let rec check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    List.iter (check_procedure prog vers) vers.version_def

  and check_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^
	      vers.version_symbol.ocaml_name ^ "'" ^ 
	      proc.proc_symbol.ocaml_name in
    
    ( match proc.proc_params with
	  [] -> assert false
	| [arg] -> 
	    output_toconv_declaration 
	      (pvp ^ "'arg")
	      arg
	      ("t_" ^ pvp ^ "'arg");
	    output_ofconv_declaration 
	      (pvp ^ "'arg")
	      arg
	      ("t_" ^ pvp ^ "'arg");
	| args ->
	    output_toconv_tuple_declaration
	      (pvp ^ "'arg")
	      args
	      ("t_" ^ pvp ^ "'arg");
	    output_ofconv_tuple_declaration
	      (pvp ^ "'arg")
	      args
	      ("t_" ^ pvp ^ "'arg")
    );
    output_toconv_declaration 
      (pvp ^ "'res")
      proc.proc_result
      ("t_" ^ pvp ^ "'res");
    output_ofconv_declaration 
      (pvp ^ "'res")
      proc.proc_result
      ("t_" ^ pvp ^ "'res");
  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
	 Typedef td ->
	   output_toconv_declaration 
	     td.decl_symbol.ocaml_name 
	     td.decl_type
	     td.decl_symbol.ocaml_name;
	   output_ofconv_declaration 
	     td.decl_symbol.ocaml_name 
	     td.decl_type
	     td.decl_symbol.ocaml_name
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  if not !firstdecl then fprintf f ";;@\n";
  fprintf mli "@]";
  fprintf f "@]"
;;

(**********************************************************************)
(* Output program definitions                                         *)
(**********************************************************************)

let output_progdefs (mli:formatter) (f:formatter) (dl:xdr_def list) =

  let rec check_program prog =
    List.iter (check_version prog) prog.prog_def

  and check_version prog vers =
    let pv = 
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "val program_%s : Rpc_program.t;;@\n" pv;
    (* ML: *)
    fprintf f "@[<hv 2>let program_%s =@ " pv;
    fprintf f "@[<hv 2>Rpc_program.create@ ";
    output_uint4 f (false, prog.prog_number);
    fprintf f "@ ";
    output_uint4 f (false, vers.version_number);
    fprintf f "@ ";
    fprintf f "(Xdr.validate_xdr_type_system [])@ ";
    fprintf f "@[<hv 2>[";
    List.iter (declare_procedure prog vers) vers.version_def;
    fprintf f "@]@ ]";
    fprintf f "@]@]@\n;;@\n";

  and declare_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in
    fprintf f "@ @[<hv 2>";
    fprintf f "\"%s\",@ (" proc.proc_symbol.xdr_name;
    output_uint4 f (false, proc.proc_number);
    fprintf f ",@ xdrt_%s'arg,@ xdrt_%s'res);" pvp pvp;
    fprintf f "@]";
  in
  
  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;

(**********************************************************************)
(* Output clients                                                     *)
(**********************************************************************)

let output_client (mli:formatter) (f:formatter) (dl:xdr_def list) auxname =

  let rec check_program prog =
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig@ " prog.prog_symbol.ocaml_name;
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " prog.prog_symbol.ocaml_name;
    (* Both: *)
    List.iter (check_version prog) prog.prog_def;
    (* MLI: *)
    fprintf mli "@]@ ";
    fprintf mli "end@ ";
    fprintf mli "@]@\n";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]@\n";

  and check_version prog vers =
    let pv = 
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig" vers.version_symbol.ocaml_name;
    fprintf mli "@ ";
    fprintf mli "open %s@ " auxname;
    fprintf mli "val @[<hv 4>create_client :@ ?esys:Unixqueue.event_system ->@ ?program_number:Rtypes.uint4 -> @ ?version_number:Rtypes.uint4 -> @ Rpc_client.connector ->@ Rpc.protocol ->@ Rpc_client.t@]";
    fprintf mli "@ ";
    fprintf mli "val @[<hv 4>create_portmapped_client :@ ?esys:Unixqueue.event_system ->@ ?program_number:Rtypes.uint4 -> @ ?version_number:Rtypes.uint4 -> @ string ->@ Rpc.protocol ->@ Rpc_client.t@]";
    fprintf mli "@ ";
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " vers.version_symbol.ocaml_name;
    fprintf f "open %s@ " auxname;
    fprintf f "let _program = program_%s@ " pv;

    fprintf f "@ ";
    fprintf f "@[<hv 2>let create_client@ ";
    fprintf f "?(esys = Unixqueue.create_unix_event_system())@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    fprintf f "connector@ ";
    fprintf f "protocol =@ ";
    fprintf f "  Rpc_client.create ?program_number ?version_number esys connector protocol _program";
    fprintf f "@]";

    fprintf f "@ @ ";
    fprintf f "@[<hv 2>let create_portmapped_client ?esys ?program_number ?version_number host protocol =@ ";
    fprintf f "let port = Rpc_portmapper.port_of_program _program host protocol in@ ";
    fprintf f "create_client ?esys ?program_number ?version_number (Rpc_client.Inet(host,port)) protocol";
    fprintf f "@]";

    fprintf f "@ @ ";

    (* Both: *)
    List.iter (define_procedure prog vers) vers.version_def;

    (* MLI: *)
    fprintf mli "@]@ end@ @]";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]";

  and define_procedure prog vers proc =
    let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
	      vers.version_symbol.ocaml_name ^ "'" ^
	      proc.proc_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "val @[<hv 4>%s :@ Rpc_client.t ->@ %s ->@ %s@]@ " 
      proc.proc_symbol.ocaml_name
      ("t_" ^ pvp ^ "'arg")
      ("t_" ^ pvp ^ "'res");
    fprintf mli "val @[<hv 4>%s'async :@ Rpc_client.t ->@ %s ->@ ((unit -> %s) -> unit) ->@ unit@]@ "
      proc.proc_symbol.ocaml_name
      ("t_" ^ pvp ^ "'arg")
      ("t_" ^ pvp ^ "'res");
    (* ML: *)
    fprintf f "@[<hv 2>";
    fprintf f "let %s client arg =@ " proc.proc_symbol.ocaml_name;
    (* fprintf f "assert(Rpc_client.program client == _program);@ "; *)
    fprintf f "_to_%s'res (Rpc_client.sync_call client \"%s\" (_of_%s'arg arg))"
      pvp proc.proc_symbol.xdr_name pvp;
    fprintf f "@]@ @ ";

    fprintf f "@[<hv 2>";
    fprintf f "let %s'async client arg pass_reply =@ " proc.proc_symbol.ocaml_name;
    (* fprintf f "assert(Rpc_client.program client == _program);@ "; *)
    fprintf f "Rpc_client.add_call client \"%s\" (_of_%s'arg arg)@ " 
      proc.proc_symbol.xdr_name pvp;
    fprintf f "  (fun g -> pass_reply (fun () -> _to_%s'res (g())))@ " pvp;
    fprintf f "@]@ @ "

  in
  
  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;

(**********************************************************************)
(* Output servers                                                     *)
(**********************************************************************)

let output_server (mli:formatter) (f:formatter) (dl:xdr_def list) auxname =

  let rec check_program prog =
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig@ " prog.prog_symbol.ocaml_name;
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " prog.prog_symbol.ocaml_name;
    (* Both: *)
    List.iter (check_version prog) prog.prog_def;
    (* MLI: *)
    fprintf mli "@]@ ";
    fprintf mli "end@ ";
    fprintf mli "@]@\n";
    (* ML: *)
    fprintf f "@]@ ";
    fprintf f "end@ ";
    fprintf f "@]";

  and check_version prog vers =
    let pv = 
      prog.prog_symbol.ocaml_name ^ "'" ^ vers.version_symbol.ocaml_name in
    (* MLI: *)
    fprintf mli "@[<v>";
    fprintf mli "@[<v 2>module %s : sig" vers.version_symbol.ocaml_name;
    fprintf mli "@ ";
    fprintf mli "open %s@ " auxname;
    fprintf mli "val @[<hv 4>create_server :@ ?limit:int ->@ ?program_number:Rtypes.uint4 ->@ ?version_number:Rtypes.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<v>";
    fprintf f "@[<v 2>module %s = struct@ " vers.version_symbol.ocaml_name;
    fprintf f "open %s@ " auxname;
    fprintf f "let _program = program_%s@ " pv;

    fprintf f "@ ";
    fprintf f "@[<hv 2>let create_server@ ";
    fprintf f "?(limit = 20)@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>t_%s'arg ->@ t_%s'res@]) ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.connector ->@ ";
    fprintf mli "Rpc.protocol ->@ ";
    fprintf mli "Rpc.mode ->@ ";
    fprintf mli "Unixqueue.event_system ->@ ";
    fprintf mli "Rpc_server.t@]@ ";
    (* ML: *)
    fprintf f "connector@ ";
    fprintf f "protocol@ ";
    fprintf f "mode@ ";
    fprintf f "esys@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.create@   ?program_number ?version_number esys connector protocol mode _program@ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Sync { @[<v>Rpc_server.sync_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.sync_proc = (fun x -> _of_%s'res (proc_%s (_to_%s'arg x)))@]});" 
	   pvp proc.proc_symbol.ocaml_name pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  limit@]@]";

    fprintf f "@ @ ";

    (* MLI: *)
    fprintf mli "val @[<hv 4>create_async_server :@ ?limit:int ->@ ?program_number:Rtypes.uint4 ->@ ?version_number:Rtypes.uint4 ->@ ";
    (* ML: *)
    fprintf f "@[<hv 2>let create_async_server@ ";
    fprintf f "?(limit = 20)@ ";
    fprintf f "?program_number@ ";
    fprintf f "?version_number@ ";
    (* Both: *)
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf mli "proc_%s : (@[<hv>Rpc_server.session ->@ t_%s'arg ->@ (t_%s'res -> unit) ->@ unit)@] ->@ "
	   proc.proc_symbol.ocaml_name
	   pvp
	   pvp;
	 fprintf f "~proc_%s@ " proc.proc_symbol.ocaml_name)
      vers.version_def;
    (* MLI: *)
    fprintf mli "Rpc_server.connector ->@ ";
    fprintf mli "Rpc.protocol ->@ ";
    fprintf mli "Rpc.mode ->@ ";
    fprintf mli "Unixqueue.event_system ->@ ";
    fprintf mli "Rpc_server.t@]@ ";
    (* ML: *)
    fprintf f "connector@ ";
    fprintf f "protocol@ ";
    fprintf f "mode@ ";
    fprintf f "esys@ ";
    fprintf f "=@ ";
    fprintf f "  @[<hv 2>";
    fprintf f "Rpc_server.create@   ?program_number ?version_number esys connector protocol mode _program@ ";
    fprintf f "  @[<hv 2>[";
    List.iter
      (fun proc ->
	 let pvp = prog.prog_symbol.ocaml_name ^ "'" ^ 
		   vers.version_symbol.ocaml_name ^ "'" ^
		   proc.proc_symbol.ocaml_name in
	 fprintf f "@ (Rpc_server.Async { @[<v>Rpc_server.async_name = \"%s\";@ "
	   proc.proc_symbol.xdr_name;
	 fprintf f "Rpc_server.async_invoke = (fun s x -> proc_%s s (_to_%s'arg x) (fun y -> Rpc_server.reply s (_of_%s'res y)))@]});" 
	  proc.proc_symbol.ocaml_name pvp pvp;
      )
      vers.version_def;
    fprintf f "@]@   ]@ ";
    fprintf f "  limit@]@]";

    fprintf f "@ @ ";

    fprintf mli "@]end@ @]";

    fprintf f "@]end@ ";
    fprintf f "@]";

  in

  fprintf mli "@[<v>";
  fprintf f "@[<v>";

  List.iter
    (function
       | Progdef prog -> 
	   check_program prog
       | _ ->
	   ())
    dl;

  fprintf mli "@]@\n";
  fprintf f "@]@\n"
;;

(* ======================================================================
 * History:
 * 
 * $Log: generate.ml,v $
 * Revision 1.12  2003/05/23 13:57:07  gerd
 * 	Using the new "fast" XDR structures.
 *
 * Revision 1.11  2002/10/19 15:34:13  gerd
 * 	Better error message when an invalid enum value is
 * found.
 *
 * Revision 1.10  2002/09/22 22:19:44  gerd
 * 	Fix: Using the mapped name for procedure names in servers.
 *
 * Revision 1.9  2002/06/13 16:07:40  gerd
 * 	Fix: Now using `False and `True as default names for boolean
 * variants (`false and `true are rejected by recent O'Caml compilers).
 * 	Fix: Constants > 255 are handled correctly. There was an error
 * in output_int4_pattern.
 *
 * Revision 1.8  2001/12/02 17:41:12  gerd
 * 	The _clnt assertions are not generated.
 *
 * Revision 1.7  2001/12/01 19:05:20  gerd
 * 	Support for ~program_number and ~version_number.
 *
 * Revision 1.6  2001/02/13 23:46:25  gerd
 * 	Implemented individual name mapping for variants. Example:
 * union u switch (an_enum discr) {
 *   case A => This_is_a: void;
 *   case B => This_is_b: int x;
 * }
 * - This is mapped to the ocaml type
 * [`This_is_a | `This_is_b of int ] instead of
 * [`a | `b of int ]
 *
 * Revision 1.5  2001/01/14 22:15:53  gerd
 * 	Now generating MLI files as well.
 *
 * Revision 1.4  2001/01/14 18:35:28  gerd
 * 	Enumerators: Constants occuring twice in enums are stripped
 * (strip_enum_list)
 *
 * Revision 1.3  2001/01/14 18:22:00  gerd
 * 	Optimized xdrt_* type terms.
 *
 * Revision 1.2  2001/01/10 00:30:42  gerd
 * 	Continued: int mapping, C preprocessor, % lines
 *
 * Revision 1.1  2001/01/08 01:47:58  gerd
 * 	Initial revision.
 *
 * 
 *)
