(*
 * $Id: rtypes.mli,v 1.4 2003/05/23 13:53:49 gerd Exp $
 *)

(* 4- and 8-bytes representation of signed integers *)

type int4
type int8

(* 4- and 8-bytes representation of non-negative integers *)

type uint4
type uint8

(* Floating-point numbers of single and double precision according to IEEE *)

type fp4
type fp8

exception Cannot_represent of string
(* raised if a conversion can't be done *)

exception Out_of_range
(* raised if string position out of range *)


(* create integers from character tuples. In these tuples, the MSB
 * comes first
 *)

val mk_int4 : char * char * char * char -> int4
val mk_int8 : char * char * char * char * char * char * char * char -> int8
val mk_uint4 : char * char * char * char -> uint4
val mk_uint8 : char * char * char * char * char * char * char * char -> uint8

(* destroy integers and get tuples *)

val dest_int4 : int4 -> char * char * char * char
val dest_int8 : int8 -> char * char * char * char * char * char * char * char
val dest_uint4 : uint4 -> char * char * char * char
val dest_uint8 : uint8 -> char * char * char * char * char * char * char * char

(* read integers from strings *)

(* these functions may raise Out_of_range if the position is out of the
 * allowed range of string positions
 *)

val read_int4 : string -> int -> int4
val read_int8 : string -> int -> int8
val read_uint4 : string -> int -> uint4
val read_uint8 : string -> int -> uint8

(* write integers into strings *)

(* these functions may raise Out_of_range if the position is out of the
 * allowed range of string positions
 *)

val write_int4 : string -> int -> int4 -> unit
val write_int8 : string -> int -> int8 -> unit
val write_uint4 : string -> int -> uint4 -> unit
val write_uint8 : string -> int -> uint8 -> unit

(* these functions are unsafe and do not check the range *)

val write_int4_unsafe : string -> int -> int4 -> unit
val write_int8_unsafe : string -> int -> int8 -> unit
val write_uint4_unsafe : string -> int -> uint4 -> unit
val write_uint8_unsafe : string -> int -> uint8 -> unit

(* integers as XDR compatible strings (i.e. big endian) *)

val int4_as_string : int4 -> string
val int8_as_string : int8 -> string
val uint4_as_string : uint4 -> string
val uint8_as_string : uint8 -> string


(* conversions from int to [u]intn and vice versa.
 * On 32-bit computers, the type int can hold 31-bit signed integers
 * (including the sign, i.e. one bit cannot be used).
 * On 64-bit computers, the type int can hold 63-bit signed integers
 * (including the sign, i.e. one bit cannot be used).
 * The int_of_xxx functions raise Cannot_represent if the number to
 * convert is too big (or too small) to be represented as int. Note
 * that this depends on the word size of your architecture.
 *)

val int_of_int4  : int4  -> int
val int_of_uint4 : uint4 -> int
val int_of_int8  : int8  -> int
val int_of_uint8 : uint8 -> int

val int4_of_int  : int -> int4
val uint4_of_int : int -> uint4
val int8_of_int  : int -> int8
val uint8_of_int : int -> uint8

(* Since O'Caml 3.00, there are the types int32 and int64 representing
 * 32-bit and 64-bit signed integers on every architecture.
 *)

val int32_of_int4  : int4  -> int32
val int32_of_uint4 : uint4 -> int32
val int32_of_int8  : int8  -> int32
val int32_of_uint8 : uint8 -> int32

val int4_of_int32  : int32 -> int4
val uint4_of_int32 : int32 -> uint4
val int8_of_int32  : int32 -> int8
val uint8_of_int32 : int32 -> uint8

val int64_of_int4  : int4  -> int64
val int64_of_uint4 : uint4 -> int64
val int64_of_int8  : int8  -> int64
val int64_of_uint8 : uint8 -> int64

val int4_of_int64  : int64 -> int4
val uint4_of_int64 : int64 -> uint4
val int8_of_int64  : int64 -> int8
val uint8_of_int64 : int64 -> uint8

(* Casts from uint4/uint8 to int32/int64. Here, the sign is ignored and
 * simply considered as a bit.
 *)

val logical_uint4_of_int32 : int32 -> uint4
val logical_int32_of_uint4 : uint4 -> int32
val logical_uint8_of_int64 : int64 -> uint8
val logical_int64_of_uint8 : uint8 -> int64

(* Floating-point stuff. The following functions all assume that the
 * system represents fp number in an IEEE-compliant way.
 *)

val fp8_of_fp4 : fp4 -> fp8
val fp4_of_fp8 : fp8 -> fp4

val float_of_fp4 : fp4 -> float
val float_of_fp8 : fp8 -> float
val fp4_of_float : float -> fp4
val fp8_of_float : float -> fp8

(* mk_fp4 e m2 m1 m0:
 * e: sign + exponent
 * m2, m1, m0: normalized mantissa
 *
 * mk_fp8 e1 e0m6 m5 m4 m3 m2 m1 m0:
 * e1: sign + upper 7 bits of exponent
 * e0m6: lower 4 bits of exponent, highest 4 bits of mantissa
 * m5 to m0: lower bits of mantissa
 *)

val mk_fp4 : char * char * char * char -> fp4
val mk_fp8 : char * char * char * char * char * char * char * char -> fp8
val dest_fp4 : fp4 -> char * char * char * char
val dest_fp8 : fp8 -> char * char * char * char * char * char * char * char

val fp4_as_string : fp4 -> string
val fp8_as_string : fp8 -> string

(* ======================================================================
 *
 * $Log: rtypes.mli,v $
 * Revision 1.4  2003/05/23 13:53:49  gerd
 * 	Several new functions to improve performance.
 *
 * Revision 1.3  2001/03/05 01:26:18  gerd
 * 	Improvements.
 *
 * Revision 1.2  2000/12/12 00:54:11  gerd
 * 	Some bugfixes in existing conversion functions.
 * 	New conversions for Int32 and Int64.
 * 	Conversions for fp4 are implemented.
 *
 * Revision 1.1  1999/04/04 20:04:56  gerd
 * 	Initial release in this depot. This files have been restructured;
 * some missing features have been added (fp8 code, recursive xdr types).
 *
 *)
