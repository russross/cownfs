(* $Id: syntax.ml,v 1.4 2002/06/13 16:05:19 gerd Exp $ 
 * ----------------------------------------------------------------------
 *)

open Rtypes;;

exception Error of string;;

let error s = raise(Error s);;

type xdr_id =
    { xdr_name : string;
      ocaml_name_requested : bool;
      mutable ocaml_name : string;
    }
;;

let mk_id name =
  { xdr_name = name; ocaml_name_requested = false; ocaml_name = "" }
;;

let mk_mapped_id xdrname ocamlname =
  { xdr_name = xdrname; ocaml_name_requested = true; ocaml_name = ocamlname }
;;

let mk_void_id () =
  { xdr_name = ""; ocaml_name_requested = false; ocaml_name = "" }
;;

type xdr_constant =
  | Constant of (bool * uint4)        (* sign (true=neg), absolute value *)
  | Named_constant of string
;;

let constant c =
  match c with
      Constant(sign,v) -> (sign,v)
    | Named_constant _ -> assert false
;;

type int_variant =
  | Abstract
  | INT32
  | INT64
  | Unboxed
;;

type xdr_type =
    T_opaque_fixed of xdr_constant ref
  | T_opaque of xdr_constant ref
  | T_opaque_unlimited
  | T_string  of xdr_constant ref
  | T_string_unlimited
  | T_option of xdr_type
  | T_void
  | T_array_fixed of (xdr_constant ref * xdr_type)
  | T_array of (xdr_constant ref * xdr_type)
  | T_array_unlimited of xdr_type
  | T_int of int_variant
  | T_uint of int_variant
  | T_hyper of int_variant
  | T_uhyper of int_variant
  | T_double
  | T_float
  | T_bool
  | T_refer_to of (refconstr * string ref)         (* always the XDR name! *)
  | T_enum of (xdr_id * xdr_constant ref) list
  | T_struct of xdr_decl list
  | T_union of xdr_union

and refconstr =
    R_struct
  | R_union
  | R_enum
  | R_any

and xdr_union =
    { discriminant : xdr_decl;
      cases : (xdr_constant ref * string option * xdr_decl) list;
        (* List of (c, ocaml_name, component_decl)
	 * E.g. case ENUM_CASE => ocaml_name: t x;
	 * c: The constant before ':' (ENUM_CASE)
	 * ocaml_name: The ocaml name for the polymorphic variant
	 * component_decl: The declaration after ':' (t x)
	 *)
      default : xdr_decl option;
    }

and xdr_decl =
    { decl_type : xdr_type;
      decl_symbol : xdr_id;
    }
;;

type xdr_prog =
    { prog_symbol : xdr_id;
      prog_def : xdr_version list;
      prog_number : uint4;
    }

and xdr_version =
    { version_symbol : xdr_id;
      version_def : xdr_proc list;
      version_number : uint4;
    }

and xdr_proc =
    { proc_symbol : xdr_id;
      proc_params : xdr_type list;
      proc_result : xdr_type;
      proc_number : uint4;
    }
;;


type xdr_def =
    Typedef of xdr_decl
  | Constdef of (xdr_id * (bool * uint4))
  | Progdef of xdr_prog
;;


let mk_decl id t =
  { decl_type = t;
    decl_symbol = id
  }
;;

let mk_union discr cs defl =
  { discriminant = discr;
    cases = cs;
    default = defl;
  }
;;

let mk_program symbol def number =
  { prog_symbol = symbol;
    prog_def = def;
    prog_number = number;
  }
;;

let mk_version symbol def number =
  { version_symbol = symbol;
    version_def = def;
    version_number = number;
  }
;;

let mk_procedure symbol params result number =
  { proc_symbol = symbol;
    proc_params = params;
    proc_result = result;
    proc_number = number;
  }
;;

let submax s i n =
  let l = String.length s in
  let n' = min (l-i) n in
  String.sub s i n'
;;

let constant_of_string s =
  (* s: 32 bit integer in C syntax, i.e:
   * 0x...:    hex number
   * 0...:     octal number
   * [1-9]...: decimal number
   * Returns the number as uint4.
   *)
  assert(String.length s >= 1);
  let sign, i0 = 
    match submax s 0 1 with
	"-" -> true, 1
      | "+" -> false, 1
      | _   -> false, 0
  in
  let base, i1 =
    match submax s i0 2 with
	("0x"|"0X") -> 16, i0+2
      | _ -> 
	  (match submax s i0 1 with
	       "0" -> 8, i0+1
	     | _   -> 10, i0
	  )
  in
  let m = Int64.of_string "0xffffffff" in
  let base64 = Int64.of_int base in
  let n = ref (Int64.zero) in
  for i = i1 to String.length s - 1 do
    let c = s.[i] in
    let v =
      match c with
	| '0'..'9' -> Char.code c - 48
	| 'a'..'f' -> Char.code c - Char.code 'a' + 10
	| 'A'..'F' -> Char.code c - Char.code 'A' + 10
	| _        -> assert false
    in
    assert (v < base);
    n := Int64.add (Int64.mul !n base64) (Int64.of_int v);
    if !n > m then
      error ("Number too big: " ^ s);
  done;
  (sign, uint4_of_int64 !n)
;;

let string_of_uint4 n =
  let n' = int64_of_uint4 n in
  Int64.to_string n'
;;


(**********************************************************************)

let resolve_constants dl =
  (* Iterates over all subterms of dl, and processes all constant definitions
   * (including enums).
   * Furthermore, all references to defined constants (Named_constant) are
   * replaced by the values (Constant) of the constants.
   *)

  let constmap = Hashtbl.create 100 in

  let def_constant id c =
    try
      ignore(Hashtbl.find constmap id.xdr_name);
      error("The constant `" ^ id.xdr_name ^ "' is already defined")
    with
	Not_found ->
	  Hashtbl.add constmap id.xdr_name c
  in

  let resolve_const c =
    match !c with
	Constant (_,_) -> ()
      | Named_constant n ->
	  ( try 
	      let sign,v = Hashtbl.find constmap n in
	      c := Constant(sign,v)
	    with
		Not_found ->
		  error("Unknown constant `" ^ n ^ "'")
	  )
  in

  let rec check_type t = (
    match t with
	T_opaque_fixed c    -> resolve_const c
      | T_opaque c          -> resolve_const c
      | T_string c          -> resolve_const c
      | T_option t'         -> check_type t'
      | T_array_fixed(c,t') -> resolve_const c; check_type t'
      | T_array(c,t')       -> resolve_const c; check_type t'
      | T_array_unlimited t'-> check_type t'
      | T_enum l            -> List.iter 
	                         (fun (id,c) -> 
				    resolve_const c;
				    def_constant id (constant !c);
				 ) l
      | T_struct td         -> List.iter check_type_decl td
      | T_union u           -> check_type_decl (u.discriminant);
	                       List.iter (fun (c,_,td) -> 
					    resolve_const c;
					    check_type_decl td) u.cases;
			       (match u.default with
				    Some td -> check_type_decl td
				  | None    -> ()
			       )
      | _                   -> ()
  )

  and check_type_decl td =
    try
      check_type td.decl_type
    with
	Error s ->
	  error
	    ("In the definition for `" ^ td.decl_symbol.xdr_name ^ "':\n" ^ s)
  in

  let rec check_progdef p =
    try
      List.iter check_versiondef p.prog_def
    with
	Error s ->
	  error
	    ("In the definition for `" ^ p.prog_symbol.xdr_name ^ "':\n" ^ s)

  and check_versiondef v =
    try
      List.iter check_procdef v.version_def
    with
	Error s ->
	  error
	    ("In the definition for `" ^ v.version_symbol.xdr_name ^ "':\n" ^ s)

  and check_procdef p =
    try
      List.iter check_type p.proc_params;
      check_type p.proc_result
    with
	Error s ->
	  error
	    ("In the definition for `" ^ p.proc_symbol.xdr_name ^ "':\n" ^ s)
  in

  List.iter
    (function
	 Typedef td       -> check_type_decl td
       | Constdef (id, c) -> def_constant id c
       | Progdef p        -> check_progdef p
    )
    dl
;;

(**********************************************************************)

type env = (string, xdr_type) Hashtbl.t
;;

let check_type_constraints dl =
  (* Checks that types are well-formed. Especially:
   * - If types are referred to by name, it is checked whether the name 
   *   is defined (forward references are accepted)
   * - It is checked that types have unique names
   * - It is checked that components of structs and unions have unique
   *   names 
   * - Size arguments of strings, arrays and opaque types must not be
   *   negative
   * - It is checked whether discriminants of unions have integer or
   *   enumerator type
   *)

  let typemap = (Hashtbl.create 100 : env) in

  let uniqueness_in_tdlist l =
    let rec check names l =
      match l with
	  x :: l' ->
	    let n = x.decl_symbol.xdr_name in
	    if n <> "" && List.mem n names then
	      error("Component `" ^ n ^ "' is defined twice");
	    (* n = "": used for void components *)
	    check (n::names) l'
	| [] ->
	    ()
    in
    check [] l
  in

  let unsigned_int (sign,v) =
    if sign then
      error("The numerical argument must be unsigned");
    ()
  in

  let signed_int (sign,v) =
    let (c1,c2,c3,c4) = dest_uint4 v in
    if sign then
      ( if c1 > '\128' || 
	  (c1 = '\128' && (c2 > '\000' || c3 > '\000' || c4 > '\000')) then
	    error "The numerical argument is too small"
      )
    else
      if c1 >= '\128' then
	error "The numerical argument is too big";
    ()
  in

  let rec get_type t =
    match t with
	T_refer_to (r,n) ->
	  let t' =
	    ( try get_type(Hashtbl.find typemap !n)
	      with
		  Not_found ->
		    error ("Type `" ^ !n ^ "' is unknown")
	    )
	  in
	  (match r with
	       R_any -> ()
	     | R_struct ->
		 (match t' with
		      T_struct _ -> ()
		    | _ -> 
			error("Reference `struct " ^ !n ^ "' does not refer to struct"))
	     | R_union ->
		 (match t' with
		      T_union _ -> ()
		    | _ -> 
			error("Reference `union " ^ !n ^ "' does not refer to union"))
	     | R_enum ->
		 (match t' with
		      T_enum _ -> ()
		    | _ -> 
			error("Reference `enum " ^ !n ^ "' does not refer to enum"))
	  );
	  t'
      | t -> t
  in

  let rec check_type t = (
    match t with
	T_opaque_fixed c     -> unsigned_int (constant !c)
      | T_opaque c           -> unsigned_int (constant !c)
      | T_string c           -> unsigned_int (constant !c)
      | T_option t'          -> check_type t'
      | T_array_fixed(c,t')  -> unsigned_int (constant !c); check_type t'
      | T_array(c,t')        -> unsigned_int (constant !c); check_type t'
      | T_array_unlimited t' -> check_type t'
      | T_refer_to n         -> ignore(get_type t)
      | T_enum l             -> List.iter 
	                          (fun(_,c) -> signed_int (constant !c)) l
      | T_struct td          -> uniqueness_in_tdlist td;
	                        List.iter check_type_decl td
      | T_union u            -> let defdecl =
	                          match u.default with
				      None    -> []
				    | Some td -> [td]
	                        in
	                        let casesdecls =
				  List.map (fun (_,_,td) -> td) u.cases in
	                        let alldecls =
				  ([u.discriminant] @ casesdecls @ defdecl) in
				(* Note: because union component
				 * identifiers are not used in the
				 * language mapping, it is not necessary
				 * to check their uniqueness:
				 *)
	                        (* uniqueness_in_tdlist alldecls; *)
	                        List.iter check_type_decl alldecls;
				check_cases u
      | _                    -> ()
  )
			   
  and check_cases u = (
    match get_type u.discriminant.decl_type with
	T_int _ ->
	  List.iter (fun (c,_,d) -> signed_int (constant !c)) u.cases
      | T_uint _ ->
	  List.iter (fun (c,_,d) -> unsigned_int (constant !c)) u.cases
      | T_bool ->
	  List.iter 
	    (fun (c,_,d) -> 
	       let (sign,absval) as n = constant !c in
	       unsigned_int n;
	       match string_of_uint4 absval with
		   "0" -> ()
		 | "1" -> ()
		 | s ->
		     error ("Boolean case must be 0 or 1, not " ^ s)
	    )
	    u.cases
      | T_enum l ->
	  List.iter 
	    (fun (c,_,d) -> 
	       let n = constant !c in
	       signed_int n;
	       if not (List.exists
			 (fun (_,c') -> !c' = !c)
			 l)
	       then
		 error "Bad enumerator case"
	    )
	    u.cases
      | _ ->
	  error("The discriminant of the union type is ill-typed")
  )

  and check_type_decl td =
    try
      check_type td.decl_type
    with
	Error s ->
	  error
	    ("In the definition for `" ^ td.decl_symbol.xdr_name ^ "':\n" ^ s)
  in

  let rec check_progdef p =
    try
      List.iter check_versiondef p.prog_def
    with
	Error s ->
	  error
	    ("In the definition for `" ^ p.prog_symbol.xdr_name ^ "':\n" ^ s)

  and check_versiondef v =
    try
      List.iter check_procdef v.version_def
    with
	Error s ->
	  error
	    ("In the definition for `" ^ v.version_symbol.xdr_name ^ "':\n" ^ s)

  and check_procdef p =
    try
      List.iter check_type p.proc_params;
      check_type p.proc_result
    with
	Error s ->
	  error
	    ("In the definition for `" ^ p.proc_symbol.xdr_name ^ "':\n" ^ s)
  in

  (* Form typemap: *)
  List.iter
    (function
	 Typedef td ->
	   ( let n = td.decl_symbol.xdr_name in
	     try
	       let t = Hashtbl.find typemap n in
	       (* Ignore the second type definition in some cases... *)
	       ( match td.decl_type with
		     T_refer_to (_,n') ->
		       if n <> !n' then
			 error ("Type `" ^ n ^ "' is defined twice")
		   | _ ->
			 error ("Type `" ^ n ^ "' is defined twice")
	       )
	     with
		 Not_found ->
		   Hashtbl.add typemap n td.decl_type
	   )
       | _ -> ()
    )
    dl;

  (* Now check types: *)
  List.iter
    (function
	 Typedef td -> check_type_decl td
       | Constdef _ -> ()
       | Progdef p -> check_progdef p
    )
    dl
;;

(**********************************************************************)

let check_program_definitions dl =
  (* Checks that names and numbers of programs, versions and procedures
   * are unique in program definitions
   *)

  let program_names = ref [] in
  let program_numbers = ref [] in

  let check_program p =

    let version_names = ref [] in
    let version_numbers = ref [] in
    
    let check_version v =

      let proc_names = ref [] in
      let proc_numbers = ref [] in

      let check_procedure q =
	
	let name = q.proc_symbol.xdr_name in
	let number = q.proc_number in

	if List.mem name !proc_names then
	  error("Procedure `" ^ name ^ "' is defined twice");

	if List.mem number !proc_numbers then
	  error("Procedure number " ^ string_of_uint4 number ^ " is defined twice");

	proc_names := name :: !proc_names;
	proc_numbers := number :: !proc_numbers;

      in

      let name = v.version_symbol.xdr_name in
      let number = v.version_number in

      if List.mem name !version_names then
	error("Version `" ^ name ^ "' is defined twice");

      if List.mem number !version_numbers then
	error("Version number " ^ string_of_uint4 number ^ " is defined twice");

      version_names := name :: !version_names;
      version_numbers := number :: !version_numbers;

      List.iter check_procedure v.version_def

    in

    let name = p.prog_symbol.xdr_name in
    let number = p.prog_number in

    if List.mem name !program_names then
      error("Program `" ^ name ^ "' is defined twice");

    if List.mem number !program_numbers then
      error("Program number " ^ string_of_uint4 number ^ " is defined twice");

    program_names := name :: !program_names;
    program_numbers := number :: !program_numbers;

    List.iter check_version p.prog_def
  in

  List.iter
    (function
	 Progdef p -> check_program p
       | _         -> ()
    )
    dl
;;

(* ======================================================================
 * 
 * $Log: syntax.ml,v $
 * Revision 1.4  2002/06/13 16:05:19  gerd
 * 	It is no longer checked whether several components of unions have
 * the same name. Such name clashes are not critical for the language
 * mapping.
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
 * Revision 1.2  2001/01/10 00:30:43  gerd
 * 	Continued: int mapping, C preprocessor, % lines
 *
 * Revision 1.1  2001/01/08 01:47:58  gerd
 * 	Initial revision.
 *
 *)
