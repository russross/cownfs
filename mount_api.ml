(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Mount_prot_caux
open Common

exception Unimplemented
exception Bad_path
exception Too_long

let mntlist = ref []

let checkPath path =
  let len = String.length path in
  if len > 127 then raise Too_long else
  let contains s sub =
    let sublen = String.length sub in
    let rec f i =
      if i + sublen > len then false else
      String.sub s i sublen = sub || f (i + 1)
    in f 0
  in
  if path = "" || path = "/" || path = "/." then "/" else
  if path.[0] <> '/' then raise Bad_path else
  if contains path "//" then raise Bad_path else
  if contains path "/./" then raise Bad_path else
  if contains path "/../" then raise Bad_path else
  if len >= 3 && String.sub path (len - 3) 3 = "/.." then raise Bad_path else
  if len >= 2 && String.sub path (len - 2) 2 = "/."
    then String.sub path 0 (len - 2)
  else if path.[len - 1] = '/' then String.sub path 0 (len - 1) else
  path

let null state session arg =
  ()

let mnt state session arg =
  let (uid, gids, hostname) = session in
(*  if uid <> 0 then `mnt3err_acces else *)
  try
    let path = checkPath arg in
    if String.length path > 127 then `mnt3err_nametoolong else
    let lst = Util.split path in
    mntlist := (hostname, path) :: !mntlist;
    `mnt3_ok { fhandle      = Fh.toString (Fh.make lst);
               auth_flavors = [| (Rtypes.int4_of_int 1) |] }
  with Too_long -> `mnt3err_nametoolong
     | _ -> `mnt3err_inval

let dump state session arg =
  let rec f lst =
    match lst with
    | [] -> None
    | (hostname, path) :: xs ->
        Some {  ml_hostname = hostname;
                ml_directory = path;
                ml_next = f xs }
  in
  f !mntlist

let umnt state session arg =
  let (uid, gids, hostname) = session in
  if uid <> 0 then () else
  try
    let path = checkPath arg in
    mntlist := List.filter (fun elt -> elt <> (hostname, path)) !mntlist
  with _ -> ()

let umntall state session arg =
  let (uid, gids, hostname) = session in
  if uid <> 0 then () else
  mntlist := List.filter (fun (hostname', _) -> hostname' <> hostname) !mntlist

let export state session arg =
  Some { ex_dir = "/"; ex_groups = None; ex_next = None }
