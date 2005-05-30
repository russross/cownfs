(*
 * $Id: rtypes.ml,v 1.1 2003/05/23 13:53:28 gerd Exp $
 *
 *)

(* NOTE: Parts of this implementation depend very much of representation 
 * details of O'Caml 3.xx. It is not guaranteed that this works in future
 * versions of O'Caml as well.
 *)

(* representation types *)

type int4 = int32;;
type int8 = int64;;

type uint4 = int32;;
type uint8 = int64;;

type fp4 = int32 (* string;; *)    (* IEEE representation of fp numbers *)
type fp8 = int64;;

exception Cannot_represent of string;;
(* raised if numbers are too big to map them to other type *)

exception Out_of_range;;

let cannot_represent s =
  raise (Cannot_represent s);;


(**********************************************************************)
(* mk_[u]intn                                                         *)
(**********************************************************************)

(* compatibility interface *)

let mk_int4 (c3,c2,c1,c0) =
  let n3 = Int32.of_int (Char.code c3) in
  let n2 = Int32.of_int (Char.code c2) in
  let n1 = Int32.of_int (Char.code c1) in
  let n0 = Int32.of_int (Char.code c0) in

  Int32.logor
    (Int32.shift_left n3 24)
    (Int32.logor
       (Int32.shift_left n2 16)
       (Int32.logor
	  (Int32.shift_left n1 8)
	  n0))
;;

let mk_int8 (c7,c6,c5,c4,c3,c2,c1,c0) =
  let n7 = Int64.of_int (Char.code c7) in
  let n6 = Int64.of_int (Char.code c6) in
  let n5 = Int64.of_int (Char.code c5) in
  let n4 = Int64.of_int (Char.code c4) in
  let n3 = Int64.of_int (Char.code c3) in
  let n2 = Int64.of_int (Char.code c2) in
  let n1 = Int64.of_int (Char.code c1) in
  let n0 = Int64.of_int (Char.code c0) in

  Int64.logor
    (Int64.shift_left n7 56)
    (Int64.logor
       (Int64.shift_left n6 48)
       (Int64.logor
	  (Int64.shift_left n5 40)
	  (Int64.logor
	     (Int64.shift_left n4 32)
	     (Int64.logor
		(Int64.shift_left n3 24)
		(Int64.logor
		   (Int64.shift_left n2 16)
		   (Int64.logor
		      (Int64.shift_left n1 8)
		      n0))))))
;;

let mk_uint4 = mk_int4;;
let mk_uint8 = mk_int8;;

(**********************************************************************)
(* read_[u]intn                                                       *)
(**********************************************************************)

let read_int4 s pos =
  if pos < 0 || pos + 4 > String.length s then 
    raise Out_of_range;

  let n3 = Int32.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Int32.shift_left n3 24 in
  
  let n2 = Int32.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Int32.logor x (Int32.shift_left n2 16) in

  let n1 = Int32.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Int32.logor x (Int32.shift_left n1 8) in

  let n0 = Int32.of_int (Char.code (String.unsafe_get s (pos+3))) in
  Int32.logor x n0

(*
  seems to be slightly better than

  Int32.logor
    (Int32.shift_left n3 24)
    (Int32.logor
       (Int32.shift_left n2 16)
       (Int32.logor
	  (Int32.shift_left n1 8)
	  n0))
*)
;;


let read_int8 s pos =
  if pos < 0 || pos + 8 > String.length s then 
    raise Out_of_range;

  let n7 = Int64.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Int64.shift_left n7 56 in

  let n6 = Int64.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Int64.logor x (Int64.shift_left n6 48) in

  let n5 = Int64.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Int64.logor x (Int64.shift_left n5 40) in

  let n4 = Int64.of_int (Char.code (String.unsafe_get s (pos+3))) in
  let x = Int64.logor x (Int64.shift_left n4 32) in

  let n3 = Int64.of_int (Char.code (String.unsafe_get s (pos+4))) in
  let x = Int64.logor x (Int64.shift_left n3 24) in

  let n2 = Int64.of_int (Char.code (String.unsafe_get s (pos+5))) in
  let x = Int64.logor x (Int64.shift_left n2 16) in

  let n1 = Int64.of_int (Char.code (String.unsafe_get s (pos+6))) in
  let x = Int64.logor x (Int64.shift_left n1 8) in

  let n0 = Int64.of_int (Char.code (String.unsafe_get s (pos+7))) in
  Int64.logor x n0
;;

let read_uint4 = read_int4;;
let read_uint8 = read_int8;;


(**********************************************************************)
(* dest_[u]intn                                                       *)
(**********************************************************************)

(* compatibility interface *)

let c_0xff_32 = Int32.of_string "0xff" ;;
let c_0xff_64 = Int64.of_string "0xff" ;;

let dest_int4 x =
  let n3 = Int32.to_int (Int32.shift_right_logical x 24) land 0xff in
  let n2 = Int32.to_int (Int32.shift_right_logical x 16) land 0xff in
  let n1 = Int32.to_int (Int32.shift_right_logical x 8) land 0xff in
  let n0 = Int32.to_int (Int32.logand x c_0xff_32) in
  (Char.chr n3, Char.chr n2, Char.chr n1, Char.chr n0)
;;


let dest_int8 x =
  let n7 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 56) 
			   c_0xff_64) in
  let n6 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 48) 
			   c_0xff_64) in
  let n5 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 40) 
			   c_0xff_64) in
  let n4 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 32) 
			   c_0xff_64) in
  let n3 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 24) 
			   c_0xff_64) in
  let n2 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 16) 
			   c_0xff_64) in
  let n1 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 8) 
			   c_0xff_64) in
  let n0 = Int64.to_int (Int64.logand x c_0xff_64) in
  (Char.chr n7, Char.chr n6, Char.chr n5, Char.chr n4,
   Char.chr n3, Char.chr n2, Char.chr n1, Char.chr n0)
;;


let dest_uint4 = dest_int4;;
let dest_uint8 = dest_int8;;

(**********************************************************************)
(* write_[u]intn                                                      *)
(**********************************************************************)

let write_int4_unsafe s pos x =
  let n3 = Int32.to_int (Int32.shift_right_logical x 24) land 0xff in
  String.unsafe_set s pos (Char.unsafe_chr n3);
  let n2 = Int32.to_int (Int32.shift_right_logical x 16) land 0xff in
  String.unsafe_set s (pos+1) (Char.unsafe_chr n2);
  let n1 = Int32.to_int (Int32.shift_right_logical x 8) land 0xff in
  String.unsafe_set s (pos+2) (Char.unsafe_chr n1);
  let n0 = Int32.to_int (Int32.logand x c_0xff_32) in
  String.unsafe_set s (pos+3) (Char.unsafe_chr n0);
  ()
;;


let write_int4 s pos x =
  if pos < 0 || pos + 4 > String.length s then
    raise Out_of_range;
  write_int4_unsafe s pos x
;;


let write_int8_unsafe s pos x =
  let n7 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 56) 
			   c_0xff_64) in
  String.unsafe_set s pos (Char.unsafe_chr n7);

  let n6 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 48) 
			   c_0xff_64) in
  String.unsafe_set s (pos+1) (Char.unsafe_chr n6);

  let n5 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 40) 
			   c_0xff_64) in
  String.unsafe_set s (pos+2) (Char.unsafe_chr n5);

  let n4 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 32) 
			   c_0xff_64) in
  String.unsafe_set s (pos+3) (Char.unsafe_chr n4);

  let n3 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 24) 
			   c_0xff_64) in
  String.unsafe_set s (pos+4) (Char.unsafe_chr n3);

  let n2 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 16) 
			   c_0xff_64) in
  String.unsafe_set s (pos+5) (Char.unsafe_chr n2);

  let n1 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 8) 
			   c_0xff_64) in
  String.unsafe_set s (pos+6) (Char.unsafe_chr n1);

  let n0 = Int64.to_int (Int64.logand x c_0xff_64) in
  String.unsafe_set s (pos+7) (Char.unsafe_chr n0);
  ()
;;


let write_int8 s pos x =
  if pos < 0 || pos + 8 > String.length s then
    raise Out_of_range;
  write_int8_unsafe s pos x
;;


let write_uint4 = write_int4;;
let write_uint8 = write_int8;;
let write_uint4_unsafe = write_int4_unsafe;;
let write_uint8_unsafe = write_int8_unsafe;;

(**********************************************************************)
(* [u]intn_as_string                                                  *)
(**********************************************************************)

let int4_as_string x =
  let s = String.create 4 in
  write_int4 s 0 x;
  s
;;

let uint4_as_string x =
  let s = String.create 4 in
  write_uint4 s 0 x;
  s
;;

let int8_as_string x =
  let s = String.create 8 in
  write_int8 s 0 x;
  s
;;

let uint8_as_string x =
  let s = String.create 8 in
  write_int8 s 0 x;
  s
;;

(**********************************************************************)
(* int_of_[u]intn                                                     *)
(**********************************************************************)

let c_max_int_32 = Int32.of_int max_int;;  (* (1) *)
let c_min_int_32 = Int32.of_int min_int;;  (* (1) *)
let c_max_int_64 = Int64.of_int max_int;;
let c_min_int_64 = Int64.of_int min_int;;
(* (1) These constants are not meaningful if Sys.word_size = 64 *)


let int_of_int4_32 x =
  if x >= c_min_int_32 && x <= c_max_int_32 then
    Int32.to_int x
  else
    cannot_represent "int_of_int4"
;;


let int_of_int4 =
  if Sys.word_size = 32 then
    int_of_int4_32
  else if Sys.word_size = 64 then
      Int32.to_int
  else
    assert false
;;


let int_of_uint4_32 x =
  if x >= Int32.zero && x <= c_max_int_32 then
    Int32.to_int x
  else
    cannot_represent "int_of_uint4"
;;


let int_of_uint4_64 x =
  if x >= Int32.zero then
    Int32.to_int x
  else
    Int32.to_int x + (0x10000000 lsl 4)
;;


let int_of_uint4 =
  if Sys.word_size = 32 then
    int_of_uint4_32
  else if Sys.word_size = 64 then
    int_of_uint4_64
  else
    failwith "word size not supported"
;;


let int_of_int8 x =
  if x >= c_min_int_64 && x <= c_max_int_64 then
    Int64.to_int x
  else
    cannot_represent "int_of_int8"
;;


let int_of_uint8 x =
  if x >= Int64.zero && x <= c_max_int_64 then
    Int64.to_int x
  else
    cannot_represent "int_of_uint8"
;;

(**********************************************************************)
(* intn_of_int                                                        *)
(**********************************************************************)

let int4_of_int_64 i = 
  let j = i asr 31 in
  if j = 0 || j = (-1) then
    Int32.of_int i
  else
    cannot_represent "int4_of_int"
;;


let int4_of_int =
  if Sys.word_size = 32 then
    Int32.of_int
  else if Sys.word_size = 64 then
    int4_of_int_64
  else
    assert false
;;


let uint4_of_int_32 i =
  if i >= 0 then
    Int32.of_int i
  else
    cannot_represent "uint4_of_int"
;;


let uint4_of_int_64 i =
  let j = i asr 32 in
  if j = 0 then
    Int32.of_int i
  else
    cannot_represent "uint4_of_int"
;;


let uint4_of_int =
  if Sys.word_size = 32 then
    uint4_of_int_32
  else if Sys.word_size = 64 then
    uint4_of_int_64
  else
    assert false
;;


let int8_of_int = Int64.of_int ;;


let uint8_of_int i =
  if i >= 0 then
    Int64.of_int i
  else
    cannot_represent "uint8_of_int"
;;


(**********************************************************************)
(* Int32 and Int64 support: int[32|64]_of_[u]intn                     *)
(**********************************************************************)

let int32_of_int4 x = x ;;

let int32_of_uint4 x =
  if x >= Int32.zero then
    x
  else
    cannot_represent "int32_of_uint4"
;;


let c_int32_min_int_64 = Int64.of_int32 Int32.min_int ;;
let c_int32_max_int_64 = Int64.of_int32 Int32.max_int ;;

let int32_of_int8 x =
  if x >= c_int32_min_int_64 && x <= c_int32_max_int_64 then
    Int64.to_int32 x
  else
    cannot_represent "int32_of_int8"
;;


let int32_of_uint8 x =
  if x >= Int64.zero && x <= c_int32_max_int_64 then
    Int64.to_int32 x
  else
    cannot_represent "int32_of_uint8"
;;


let int64_of_int4 = Int64.of_int32 ;;


let c_100000000_64 = Int64.of_string "0x100000000";;

let int64_of_uint4 x =
  if x >= Int32.zero then
    Int64.of_int32 x
  else
    Int64.add (Int64.of_int32 x) c_100000000_64
;;

let int64_of_int8 x = x ;;


let int64_of_uint8 x =
  if x >= Int64.zero then
    x
  else
    cannot_represent "int64_of_uint8"
;;


(**********************************************************************)
(* Int32 and Int64 support: [u]intn_of_int[32|64]                     *)
(**********************************************************************)

let int4_of_int32 x = x ;;

let uint4_of_int32 i =
  if i < Int32.zero then
    cannot_represent "uint4_of_int32";
  i
;;

let int8_of_int32 i =
  Int64.of_int32 i
;;

let uint8_of_int32 i =
  if i < Int32.zero then
    cannot_represent "uint8_of_int32";
  Int64.of_int32 i
;;

let c_uint4_max_64 = Int64.of_string  "0xFFFFFFFF";;

let int4_of_int64 i =
  if i >= c_int32_min_int_64 && i <= c_int32_max_int_64 then
    Int64.to_int32 i
  else cannot_represent "int4_of_int64"
;;

let uint4_of_int64 i =
  if i < Int64.zero || i > c_uint4_max_64 then
    cannot_represent "uint4_of_int64";
  Int64.to_int32 i
;;

let int8_of_int64 i = i ;;

let uint8_of_int64 i =
  if i < Int64.zero then
    cannot_represent "uint8_of_int64";
  i
;;

(**********************************************************************)
(* logical_xxx_of_xxx                                                 *)
(**********************************************************************)

let logical_uint4_of_int32 x = x;;
let logical_int32_of_uint4 x = x;;
let logical_uint8_of_int64 x = x;;
let logical_int64_of_uint8 x = x;;

(**********************************************************************)
(* floating-point numbers                                             *)
(**********************************************************************)

let n23 = Int64.of_string "0x7fffff";;
let n32 = Int64.of_string "0x80000000";;
let n52 = Int64.of_string "0x10000000000000";;

let fp8_of_fp4_int64 x =
  let m = Int64.logand x n23 in
  let exp = Int64.to_int (Int64.shift_right_logical x 23) land 0xff in
  let s = Int64.logand x n32 in

  let x1 = Int64.shift_left s 32 in
  if exp = 255 then begin
    (* Infinity, NaN *)
    Int64.logor 
      x1
      (Int64.logor
	 (Int64.shift_left (Int64.of_int 0x7ff) 52)
	 (Int64.shift_left m 29)
      )
  end
  else begin
    let m' = ref (Int64.shift_left m 29) in
    let exp' = ref (exp + 1023 - 127) in
    if exp = 0 && !m' <> Int64.zero then begin
      (* We need normalization *)
      while Int64.logand !m' n52 = Int64.zero do
	m' := Int64.shift_left !m' 1;
	decr exp'
      done
    end;
    Int64.logor 
      x1
      (Int64.logor
	 (Int64.shift_left (Int64.of_int !exp') 52)
	 !m'
      )
  end
;;


let fp8_of_fp4 x =
  let x_int32 = x in
  let x_int64 = Int64.of_int32 x_int32 in
  let x'_int64 = fp8_of_fp4_int64 x_int64 in
  let x' = x'_int64 in
  x'
;;


let n51 = Int64.of_string "0xfffffffffffff";;
let n64 = Int64.of_string "0x8000000000000000";;
let n22 = Int64.of_string "0x400000";;

let fp4_of_fp8_int64 x =
  let m = Int64.logand x n51 in
  let exp = Int64.to_int (Int64.shift_right_logical x 52) land 0x7ff in
  let s = Int64.logand x n64 in
  
  let x1 = Int64.shift_right_logical s 32 in
  if exp = 0x7ff then begin
    (* infinity, NaN *)
    if m = Int64.zero then
      (* infinity *)
      Int64.logor 
	x1
	(Int64.shift_left (Int64.of_int 0xff) 23)
    else
      (* NaN *)
      Int64.logor 
	x1
	(Int64.logor
	   (Int64.shift_left (Int64.of_int 0xff) 23)
	   (Int64.logor
	      (Int64.shift_right_logical m 29)
	      (Int64.one)))
  end 
  else begin
    let m' = ref (Int64.shift_right_logical m 29) in
    let exp' = ref (exp - 1023 + 127) in
    if !exp' <= 0 then begin
      m' := Int64.logor (Int64.shift_right_logical !m' 1) n22;
      while !exp' < 0 do
	m' := Int64.shift_right_logical !m' 1;
	incr exp'
      done
    end;
    if !exp' >= 255 then
      cannot_represent "fp4_of_fp8";

    Int64.logor 
      x1
      (Int64.logor
	 (Int64.shift_left (Int64.of_int !exp') 23)
	 !m'
      )
  end
;;


let fp4_of_fp8 x =
  let x_int64 = x in
  let x'_int64 = fp4_of_fp8_int64 x_int64 in
  let x'_int32 = Int64.to_int32 x'_int64 in
  let x' = x'_int32 in
  x'
;;


let float_of_fp8 x =
  (* Requires O'Caml >= 3.01 *)
  Int64.float_of_bits x
;;


let float_of_fp4 x =
  float_of_fp8 (fp8_of_fp4 x)
;;


let fp8_of_float x =
  (* Requires O'Caml >= 3.01 *)
  Int64.bits_of_float x
;;


let fp4_of_float x =
  fp4_of_fp8 (fp8_of_float x)
;;

let mk_fp4 = mk_int4 ;;

let mk_fp8 = mk_int8 ;;

let dest_fp4 = dest_int4 ;;

let dest_fp8 = dest_int8 ;;


let fp4_as_string = int4_as_string;;
let fp8_as_string = int8_as_string;;

(* ======================================================================
 *
 * $Log: rtypes.ml,v $
 * Revision 1.1  2003/05/23 13:53:28  gerd
 * 	Giving up the 3.00 version of Rtypes.
 *
 * Revision 1.4  2001/03/10 10:29:24  gerd
 * 	Updated for O'Caml 3.00
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
