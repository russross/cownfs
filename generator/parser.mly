%{

(* $Id: parser.mly,v 1.4 2002/06/11 23:07:01 gerd Exp $ 
 *)

  open Syntax

%}

%token LBRACKET RBRACKET
%token LANGLE RANGLE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token STAR COMMA SEMICOLON COLON EQUAL MAPSTO
%token <string> IDENT 
%token <string> INTLITERAL
%token K_opaque K_string K_void K_unsigned K_int K_hyper
%token K_float K_double K_quadruple K_bool
%token K_enum K_struct K_union K_switch K_case K_default
%token K_const K_typedef K_program K_version
%token K_int32 K_int64 K_unboxed K_abstract
%token IGNORE PERCENT
%token <int*int> LINEFEED 
%token <int*string> SETFILE
%token EOF

%type <Syntax.xdr_def list> specification

%start specification

%%

declaration:
  type_specifier declared_identifier
    { mk_decl $2 $1 }
| type_specifier declared_identifier LBRACKET value RBRACKET
    { mk_decl $2 (T_array_fixed($4, $1)) }
| type_specifier declared_identifier LANGLE value_opt RANGLE
    { let v_opt = $4 in
      match v_opt with
	  None   -> mk_decl $2 (T_array_unlimited $1)
	| Some v -> mk_decl $2 (T_array (v, $1))
    }
| K_opaque declared_identifier LBRACKET value RBRACKET
    { mk_decl $2 (T_opaque_fixed $4) }
| K_opaque declared_identifier LANGLE value_opt RANGLE
    { let v_opt = $4 in
      match v_opt with
	  None   -> mk_decl $2 T_opaque_unlimited
	| Some v -> mk_decl $2 (T_opaque v)
    }
| K_string declared_identifier LANGLE value_opt RANGLE
    { let v_opt = $4 in
      match v_opt with
	  None   -> mk_decl $2 (T_string_unlimited)
	| Some v -> mk_decl $2 (T_string v)
    }
| type_specifier STAR declared_identifier
    { mk_decl $3 (T_option $1) }
| K_void
    { mk_decl (mk_void_id()) T_void }
;

value_opt:
  value
    { Some $1 }
|
    { None }
;

value:
  constant
    { let (sign,absval) = $1 in ref (Constant(sign,absval)) }
| identifier
    { ref (Named_constant $1) }
;

type_specifier:
  K_unsigned int_or_hyper
    { match $2 with
	  T_int v   -> T_uint v
	| T_hyper v -> T_uhyper v
	| _         -> assert false
    }
| K_unsigned
    { T_uint !Options.default_int_variant }
| int_or_hyper
    { $1 }
| K_float
    { T_float }
| K_double
    { T_double }
| K_quadruple
    { error "Sorry, quadruple-precision floating point numbers are not supported"
    }
| K_bool
    { T_bool }
| enum_type_spec
    { $1 }
| struct_type_spec
    { $1 }
| union_type_spec
    { $1 }
| identifier
    { T_refer_to (R_any, ref $1) }
;

int_or_hyper:
  K_int
    { T_int !Options.default_int_variant }
| K_hyper
    { T_hyper !Options.default_hyper_variant }
| K_abstract K_int
    { T_int Abstract }
| K_abstract K_hyper
    { T_hyper Abstract }
| K_unboxed K_int
    { T_int Unboxed }
| K_unboxed K_hyper
    { T_hyper Unboxed }
| K_int32 K_int
    { T_int INT32 }
| K_int64 K_int
    { T_int INT64 }
| K_int64 K_hyper
    { T_hyper INT64 }
;

enum_type_spec:
  K_enum enum_body
    { T_enum $2 }
| K_enum identifier
    { T_refer_to (R_enum, ref $2) }
;

enum_body:
  LBRACE enum_body_list RBRACE
    { $2 }
;

enum_body_list:
  declared_identifier EQUAL value
    { [ $1, $3 ] }
| declared_identifier EQUAL value COMMA enum_body_list
    { ( $1, $3 ) :: $5 }
;

struct_type_spec:
  K_struct struct_body
    { T_struct $2 }
| K_struct identifier
    { T_refer_to (R_struct, ref $2) }
;

struct_body:
  LBRACE struct_body_list RBRACE
    { $2 }
;

struct_body_list:
  declaration SEMICOLON
    { [ $1 ] }
| declaration SEMICOLON struct_body_list
    { $1 :: $3 }
;

union_type_spec:
  K_union union_body
    { T_union $2 }
| K_union identifier
    { T_refer_to (R_union, ref $2) }
;

union_body:
  K_switch LPAREN declaration RPAREN 
           LBRACE union_body_list union_default_opt RBRACE
    { mk_union $3 $6 $7 }
;

/*
union_body_list:
  K_case value union_case_mapping COLON declaration SEMICOLON
    { [ $2, $3, $5 ] }
| K_case value union_case_mapping COLON declaration SEMICOLON union_body_list
    { ( $2, $3, $5 ) :: $7 }
;
*/

union_body_list:
  union_case_list declaration SEMICOLON
    { let d = $2 in List.map (fun (v,mv) -> (v,mv,d)) $1 }
| union_case_list declaration SEMICOLON union_body_list
    { let d = $2 in List.map (fun (v,mv) -> (v,mv,d)) $1 @ $4 }
;

union_case_list:
  K_case value union_case_mapping COLON
    { [ $2, $3 ] }
| K_case value union_case_mapping COLON union_case_list
    { ($2, $3) :: $5 }
;

union_case_mapping:
  MAPSTO IDENT
    { Some $2 }
|
    { None }
;

union_default_opt:
  K_default COLON declaration SEMICOLON
    { Some $3 }
|
    { None }
;

constant_def:
  K_const declared_identifier EQUAL constant SEMICOLON
    { Constdef($2, $4) }
;

type_def:
  K_typedef declaration SEMICOLON
    { Typedef $2 }
| K_enum declared_identifier enum_body SEMICOLON
    { Typedef (mk_decl $2 (T_enum $3)) }
| K_struct declared_identifier struct_body SEMICOLON
    { Typedef (mk_decl $2 (T_struct $3)) }
| K_union declared_identifier union_body SEMICOLON
    { Typedef (mk_decl $2 (T_union $3)) }
;

program_def:
  K_program declared_identifier 
            LBRACE program_def_list RBRACE EQUAL constant SEMICOLON
    { let (sign,nr) = $7 in
      if sign then error "Program numbers must not be negative";
      mk_program $2 $4 nr
    }
;

program_def_list:
  version_def
    { [ $1 ] }
| version_def program_def_list
    { $1 :: $2 }
;

version_def:
  K_version declared_identifier 
            LBRACE version_def_list RBRACE EQUAL constant SEMICOLON
    { let (sign,nr) = $7 in
      if sign then error "Version numbers must not be negative";
      mk_version $2 $4 nr
    }
;

version_def_list:
  procedure_def
    { [ $1 ] }
| procedure_def version_def_list
    { $1 :: $2 }
;

procedure_def:
  type_specifier_or_void declared_identifier 
                 LPAREN parameter_list_or_void RPAREN EQUAL constant SEMICOLON
    { let (sign,nr) = $7 in
      if sign then error "Procdure numbers must not be negative";
      mk_procedure $2 $4 $1 nr
    }
;

type_specifier_or_void:
  type_specifier
    { $1 }
| K_void
    { T_void }
;

parameter_list:
  type_specifier
    { [ $1 ] }
| type_specifier COMMA parameter_list
    { $1 :: $3 }
;

parameter_list_or_void:
  parameter_list
    { $1 }
| K_void
    { [ T_void ] }
;

definition:
  type_def
    { $1 }
| constant_def
    { $1 }
| program_def
    { Progdef $1 }
;

specification:
  definition specification
    { $1 :: $2 }
| EOF
    { [] }
;

identifier:
  IDENT
    { $1 }
;

declared_identifier:
  IDENT
    { mk_id $1 }
| IDENT MAPSTO IDENT
    { mk_mapped_id $1 $3 }
;

constant:
  INTLITERAL
    { constant_of_string $1 }
;

%%

(* ======================================================================
 * 
 * $Log: parser.mly,v $
 * Revision 1.4  2002/06/11 23:07:01  gerd
 * 	Multiple "case" lists in unions.
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
