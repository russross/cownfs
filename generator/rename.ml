(* $Id: rename.ml,v 1.4 2002/06/13 16:04:22 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


open Syntax;;

let name_mapping
      ~rename_constant
      ~rename_enum
      ~rename_type
      ~rename_union_component
      ~rename_struct_component
      ~rename_program
      ~rename_version
      ~rename_procedure
      dl =
  
  let rec check_type t =
    match t with
	T_option t'          -> check_type t'
      | T_array_fixed(_,t')  -> check_type t'
      | T_array(_,t')        -> check_type t'
      | T_array_unlimited t' -> check_type t'
      | T_enum l ->
	  List.iter (fun (id,_) -> rename_enum id) l
      | T_struct tdl ->
	  List.iter (fun td -> rename_struct_component td.decl_symbol) tdl;
	  List.iter (fun td -> check_type td.decl_type) tdl;
      | T_union u ->
	  let decls = 
	    List.map (fun (_,_,td) -> td) u.cases @ 
	    match u.default with Some d -> [d] | None -> [] in
	  List.iter (fun td -> rename_union_component td.decl_symbol) decls;
	  List.iter (fun td -> check_type td.decl_type) decls;
      | _ ->
	  ()
  in

  List.iter
    (function 
	 Typedef td ->
	   rename_type td.decl_symbol;
	   check_type td.decl_type
       | Constdef (id,_) ->
	   rename_constant id
       | Progdef p ->
	   rename_program p.prog_symbol;
	   List.iter
	     (fun v ->
		rename_version p.prog_symbol v.version_symbol;
		List.iter
		  (fun proc ->
		     rename_procedure 
		       p.prog_symbol v.version_symbol proc.proc_symbol;
		     List.iter check_type proc.proc_params;
		     check_type proc.proc_result
		  )
		  v.version_def
	     )
	     p.prog_def
    )
    dl
;;


let reserved =
  [ (* Reserved by ocamlrpcgen: *)
    "create_client";
    "create_portmapped_client";
    "create_server";
    "create_async_server";
    (* O'Caml keywords: *)
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "closed";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "object";
    "of";
    "open";
    "or";
    "parser";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
  ]
;;


let reserved_table =
  let t = Hashtbl.create 100 in
  List.iter
    (fun n -> Hashtbl.add t n ())
    reserved;
  t
;;


let simple_name_mapping dl =
  (* names of constants => lowercase     [value namespace]
   * names of enums => lowercase         [same namespace as constants]
   * names of types => lowercase         [type namespace]
   * names of union components => Capitalized    [no namespace]
   * names of struct components => lowercase     [component namespace]
   * names of programs and versions => Capitalized [==> modules]
   * names of procedures => lowercase    [value namespace within version module]
   *
   * names of discriminants do not occur in O'Caml
   *)

  (* Every list enumerates the names already used: *)
  let value_ns = ref [] in
  let type_ns = ref [] in
  let comp_ns = ref [] in
  let module_ns = ref [] in

  (* --- ROUND 1: Assign requested names ---------------------------------- *)
  let check_lowercase id =
    let c = id.ocaml_name.[0] in
    if c <> Char.lowercase c then
      error ("Name mapping fails for requested O'Caml name `" ^ 
	     id.ocaml_name ^ "': name must be lowercase");
  in

  let check_uppercase id =
    let c = id.ocaml_name.[0] in
    if c <> Char.uppercase c then
      error ("Name mapping fails for requested O'Caml name `" ^ 
	     id.ocaml_name ^ "': name must be uppercase");
  in

  let assign_requested_name ?(prefix = (fun s->s)) ns uc id =
    if id.ocaml_name_requested then begin
      if Hashtbl.mem reserved_table id.ocaml_name then
	error ("Name mapping fails for requested O'Caml name `" ^ 
	       id.ocaml_name ^ "': name is reserved");
      if List.mem (prefix id.ocaml_name) !ns then
	error ("Name mapping fails for requested O'Caml name `" ^ 
	       id.ocaml_name ^ "': name has been assigned twice");
      (if uc then check_uppercase else check_lowercase) id;
      ns := (prefix id.ocaml_name) :: !ns
    end
  in

  let assign_requested_version_name prog_id vers_id =
    assign_requested_name 
      ~prefix: (fun n -> prog_id.ocaml_name ^ "." ^ n)
      module_ns
      true
      vers_id
  in

  let assign_requested_procedure_name prog_id vers_id proc_id =
    assign_requested_name 
      ~prefix: (fun n -> prog_id.ocaml_name ^ "." ^ 
		         vers_id.ocaml_name ^ "." ^ n)
      module_ns
      false
      proc_id
  in

  name_mapping
    ~rename_constant:        (assign_requested_name value_ns false)
    ~rename_enum:            (assign_requested_name value_ns false)
    ~rename_type:            (assign_requested_name type_ns false)
    ~rename_union_component: (fun _ -> ())
    ~rename_struct_component:(assign_requested_name comp_ns false)
    ~rename_program:         (assign_requested_name module_ns true)
    ~rename_version:         assign_requested_version_name
    ~rename_procedure:       assign_requested_procedure_name
    dl;

  (* --- ROUND 2: Map other names somehow --------------------------------- *)

  let get_lowercase id = String.lowercase id.xdr_name in
  let get_uppercase id = String.capitalize id.xdr_name in

  let map_name ?(prefix = (fun s -> s)) ns uc id =
    if not id.ocaml_name_requested then begin
      let n = ref ((if uc then get_uppercase else get_lowercase) id) in
      let renamed = ref false in
      while List.mem (prefix !n) !ns || Hashtbl.mem reserved_table !n do
	n := !n ^ "'";
	renamed := true
      done;
      if !renamed then
	prerr_endline("Warning: Renamed \"" ^ 
		      prefix id.xdr_name ^ "\" to \"" ^ 
		      prefix !n ^ "\"");
      id.ocaml_name <- !n;
      ns := (prefix !n) :: !ns
    end
  in

  let map_version_name prog_id vers_id =
    map_name 
      ~prefix: (fun n -> prog_id.ocaml_name ^ "." ^ n)
      module_ns
      true
      vers_id
  in
  
  let map_procedure_name prog_id vers_id proc_id =
    map_name 
      ~prefix: (fun n -> prog_id.ocaml_name ^ "." ^ 
		         vers_id.ocaml_name ^ "." ^ n)
      module_ns
      false
      proc_id
  in
  
  let retain_name id =
    if not id.ocaml_name_requested then
      id.ocaml_name <- id.xdr_name
  in

  name_mapping
    ~rename_constant:        (map_name value_ns false)
    ~rename_enum:            (map_name value_ns false)
    ~rename_type:            (map_name type_ns false)
    ~rename_union_component: retain_name
    ~rename_struct_component:(map_name comp_ns false)
    ~rename_program:         (map_name module_ns true)
    ~rename_version:         map_version_name
    ~rename_procedure:       map_procedure_name
    dl;
;;

(* ======================================================================
 * History:
 * 
 * $Log: rename.ml,v $
 * Revision 1.4  2002/06/13 16:04:22  gerd
 * 	Keyword "object" was missing in table of reserved words
 *
 * Revision 1.3  2001/02/13 23:46:25  gerd
 * 	Implemented individual name mapping for variants. Example:
 * union u switch (an_enum discr) {
 *   case A => This_is_a: void;
 *   case B => This_is_b: int x;
 * }
 * - This is mapped to the ocaml type
 * [`This_is_a | `This_is_b of int ] instead of
 * [`a | `b of int ]
 *
 * Revision 1.2  2001/01/14 17:43:54  gerd
 * 	Now printing a warning when an identifier is renamed.
 * 	List of reserved identifiers.
 *
 * Revision 1.1  2001/01/08 01:47:58  gerd
 * 	Initial revision.
 *
 * 
 *)
