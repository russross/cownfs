(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Nfs3_prot_caux;;
open Unix;;
open Unix.LargeFile;;

exception Invalid_path of string

let hashString seed s =
  let rec f i a =
    if i < 0
      then a
      else f (i - 1) (a * 33 + (int_of_char (s.[i])))
  in f ((String.length s) - 1) seed

let hashString64 seed s =
  let rec f i a =
    if i < 0
      then a
      else f (i - 1) (Int64.add (Int64.mul a 73L)
                                (Int64.of_int (int_of_char (s.[i]))))
  in f ((String.length s) - 1) seed

let assertPath p =
  let last = (String.length p) - 1 in
  let rec f i =
    if i >= last || p.[i] <> '/' || p.[i+1] = '/'
      then raise (Invalid_path p)
      else try f (String.index_from p (i+1) '/') with Not_found -> ()
  in if p = "/" then () else f 0

(* split a path into a list of parts *)
let split path =
  let rec f last a =
    if last <= 0 then a else
    let i = String.rindex_from path last '/' in
    f (i - 1) ((String.sub path (i + 1) (last - i)) :: a)
  in f ((String.length path) - 1) []

(* join a list of parts into a path *)
let join pathlist =
  if pathlist = [] then "/"
  else List.fold_left (fun a s -> a ^ "/" ^ s) "" pathlist

let startsWith s sub =
  let len = String.length sub in
  len = 0 || (len <= String.length s && (String.sub s 0 len = sub))

let endsWith s sub =
  let slen = String.length s in
  let sublen = String.length sub in
  slen >= sublen && String.sub s (slen - sublen) sublen = sub

let dirname = Filename.dirname

let filename = Filename.basename

let concatPath parent name =
  match name with
  | "." -> parent
  | ".." -> dirname parent
  | n -> Filename.concat parent name

let validateName name =
  name <> "" && not (String.contains name '/') &&
                not (String.contains name '\000')

external lchown : string -> int -> int -> unit = "linux_lchown"

external create : string -> int -> int -> int -> Int32.t -> Int32.t -> int
        = "linux_create_bc" "linux_create_opt"

external mknod : string -> int -> Int64.t -> int -> int -> unit = "linux_mknod"

external hash64 : string -> Rtypes.uint8 = "hash64"

let padded_length s =
  let len = String.length s + 3 in
  len - (len land 3)

let (readdir_set,
        readdir_base, readdir_entry_size,
        readdirplus_base, readdirplus_entry_size) =
  let rd_base = ref 0 in
  let rd_inc  = ref 0 in
  let pl_base = ref 0 in
  let pl_inc  = ref 0 in
  ((fun (a,b,x,y) -> rd_base := a; rd_inc := b; pl_base := x; pl_inc := y),
   (fun () ->
        if !rd_base = 0 then failwith "readdir_base not initialized"
        else !rd_base),
   (fun s ->
        if !rd_inc = 0 then failwith "readdir_entry_size not initialized"
        else !rd_inc + padded_length s),
   (fun () ->
        if !pl_base = 0 then failwith "readdirplus_base not initialized"
        else !pl_base),
   (fun s ->
        if !pl_inc = 0 then failwith "readdirplus_entry_size not initialized"
        else !pl_inc + padded_length s))
