(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

type t = string

let hashFuncs =
  [Util.hashString 412007587;
   Util.hashString 566025871;
   Util.hashString 649717793;
   Util.hashString 581870171;
   Util.hashString 179407457]

let hashFull = Util.hashString 774945461

let (fhBits, fhHashOffset) = (223, 28)

let setPrefix h elt =
  let setBit bit =
    h.[bit/8] <- (char_of_int ((int_of_char h.[bit/8]) lor
                               (1 lsl (bit mod 8)))) in
  List.iter (function f -> setBit ((abs (f elt)) mod fhBits)) hashFuncs

let testPrefix h elt =
  let getBit bit =
    ((int_of_char h.[bit/8]) lsr (bit mod 8)) land 1 = 1 in
  List.for_all (function f -> getBit ((abs (f elt)) mod fhBits)) hashFuncs

let set h path =
  let hash = hashFull path in
  h.[fhHashOffset  ] <- (char_of_int ((hash land 0x7f000000) lsr 24));
  h.[fhHashOffset+1] <- (char_of_int ((hash land 0x00ff0000) lsr 16));
  h.[fhHashOffset+2] <- (char_of_int ((hash land 0x0000ff00) lsr 8));
  h.[fhHashOffset+3] <- (char_of_int  (hash land 0x000000ff))

let test h path =
  let hash = hashFull path in
  let target =
    ((int_of_char h.[fhHashOffset  ]) lsl 24) lor
    ((int_of_char h.[fhHashOffset+1]) lsl 16) lor
    ((int_of_char h.[fhHashOffset+2]) lsl 8)  lor
     (int_of_char h.[fhHashOffset+3]) in
  hash = target

let make pathlist =
  let fh = String.make 32 '\000' in
  let rec iter s lst =
    match lst with
    | x :: xs -> setPrefix fh s; iter (Util.concatPath s x) xs
    | []      -> setPrefix fh s; set fh s in
  iter "/" pathlist;
  fh

let ofString s =
  if String.length s = 32 then s else failwith "ofString"

let toString t = t

let root = make []
