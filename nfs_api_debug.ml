(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Nfs3_prot_caux;;
open Nfs_api;;
open Common;;

let debug_string = prerr_string
let debug_newline = prerr_newline
let debug_endline = prerr_endline

let print_error = ref true
let print_noent = ref false
let print_okay = ref false
let dump_interval = ref 1800.0

let timer = Hashtbl.create 30
let last_dump = ref (Unix.time ())

let dump () =
  debug_string "timings:\n";
  Hashtbl.iter
    (fun name (count, time) ->
      debug_string ((Printf.sprintf "%11s" name)^
        ": count="^(Printf.sprintf "%7d" count)^
        " total="^(Printf.sprintf "%15.8f" time)^
        " avg="^(Printf.sprintf "%15.8f" (time /. (float_of_int count)))^"\n"))
    timer;
  debug_newline ()

let wrap name f state session arg =
  let start = Unix.gettimeofday () in
  let result = f state session arg in
  let stop = Unix.gettimeofday () in
  let time = stop -. start in
  begin
    try let (count, total) = Hashtbl.find timer name in
        Hashtbl.replace timer name (count + 1, total +. time)
    with Not_found -> Hashtbl.add timer name (1, time)
  end;
  if Unix.time () -. !last_dump > !dump_interval
    then (last_dump := Unix.time (); dump ());
  result

let debug s f state ses arg =
  let res = f state ses arg in
  if !print_error
    then
      begin
        match res with
        | `nfs3_ok _ -> if !print_okay
                        then debug_endline (s^" -> okay");
                        res
        | `nfs3err_perm _ ->
            debug_endline (s^" -> nfs3err_perm error"); res
        | `nfs3err_noent _ ->
            debug_endline (s^" -> nfs3err_noent error"); res
        | `nfs3err_io _ ->
            debug_endline (s^" -> nfs3err_io error"); res
        | `nfs3err_nxio _ ->
            debug_endline (s^" -> nfs3err_nxio error"); res
        | `nfs3err_acces _ ->
            debug_endline (s^" -> nfs3err_acces error"); res
        | `nfs3err_exist _ ->
            debug_endline (s^" -> nfs3err_exist error"); res
        | `nfs3err_xdev _ ->
            debug_endline (s^" -> nfs3err_xdev error"); res
        | `nfs3err_nodev _ ->
            debug_endline (s^" -> nfs3err_nodev error"); res
        | `nfs3err_notdir _ ->
            debug_endline (s^" -> nfs3err_notdir error"); res
        | `nfs3err_isdir _ ->
            debug_endline (s^" -> nfs3err_isdir error"); res
        | `nfs3err_inval _ ->
            debug_endline (s^" -> nfs3err_inval error"); res
        | `nfs3err_fbig _ ->
            debug_endline (s^" -> nfs3err_fbig error"); res
        | `nfs3err_nospc _ ->
            debug_endline (s^" -> nfs3err_nospc error"); res
        | `nfs3err_rofs _ ->
            debug_endline (s^" -> nfs3err_rofs error"); res
        | `nfs3err_mlink _ ->
            debug_endline (s^" -> nfs3err_mlink error"); res
        | `nfs3err_nametoolong _ ->
            debug_endline (s^" -> nfs3err_nametoolong error"); res
        | `nfs3err_notempty _ ->
            debug_endline (s^" -> nfs3err_notempty error"); res
        | `nfs3err_dquot _ ->
            debug_endline (s^" -> nfs3err_dquot error"); res
        | `nfs3err_stale _ ->
            debug_endline (s^" -> nfs3err_stale error"); res
        | `nfs3err_remote _ ->
            debug_endline (s^" -> nfs3err_remote error"); res
        | `nfs3err_badhandle _ ->
            debug_endline (s^" -> nfs3err_badhandle error"); res
        | `nfs3err_not_sync _ ->
            debug_endline (s^" -> nfs3err_not_sync error"); res
        | `nfs3err_bad_cookie _ ->
            debug_endline (s^" -> nfs3err_bad_cookie error"); res
        | `nfs3err_notsupp _ ->
            debug_endline (s^" -> nfs3err_notsupp error"); res
        | `nfs3err_toosmall _ ->
            debug_endline (s^" -> nfs3err_toosmall error"); res
        | `nfs3err_serverfault _ ->
            debug_endline (s^" -> nfs3err_serverfault error"); res
        | `nfs3err_badtype _ ->
            debug_endline (s^" -> nfs3err_badtype error"); res
        | `nfs3err_jukebox _ ->
            debug_endline (s^" -> nfs3err_jukebox error"); res
        | `nfs3err_fprintnotfound _ ->
            debug_endline (s^" -> nfs3err_fprintnotfound error"); res
        | `nfs3err_aborted _ ->
            debug_endline (s^" -> nfs3err_aborted error"); res
        | _ -> debug_endline (s^" -> unknown error"); res
      end
    else res

let debug1 s f state ses arg =
  let res = f state ses arg in
  if !print_error
    then
      begin
        match res with
        | `nfs3_ok _ -> if !print_okay
                        then debug_endline (s^" -> okay");
                        res
        | `nfs3err_perm ->
            debug_endline (s^" -> nfs3err_perm error"); res
        | `nfs3err_noent ->
            debug_endline (s^" -> nfs3err_noent error"); res
        | `nfs3err_io ->
            debug_endline (s^" -> nfs3err_io error"); res
        | `nfs3err_nxio ->
            debug_endline (s^" -> nfs3err_nxio error"); res
        | `nfs3err_acces ->
            debug_endline (s^" -> nfs3err_acces error"); res
        | `nfs3err_exist ->
            debug_endline (s^" -> nfs3err_exist error"); res
        | `nfs3err_xdev ->
            debug_endline (s^" -> nfs3err_xdev error"); res
        | `nfs3err_nodev ->
            debug_endline (s^" -> nfs3err_nodev error"); res
        | `nfs3err_notdir ->
            debug_endline (s^" -> nfs3err_notdir error"); res
        | `nfs3err_isdir ->
            debug_endline (s^" -> nfs3err_isdir error"); res
        | `nfs3err_inval ->
            debug_endline (s^" -> nfs3err_inval error"); res
        | `nfs3err_fbig ->
            debug_endline (s^" -> nfs3err_fbig error"); res
        | `nfs3err_nospc ->
            debug_endline (s^" -> nfs3err_nospc error"); res
        | `nfs3err_rofs ->
            debug_endline (s^" -> nfs3err_rofs error"); res
        | `nfs3err_mlink ->
            debug_endline (s^" -> nfs3err_mlink error"); res
        | `nfs3err_nametoolong ->
            debug_endline (s^" -> nfs3err_nametoolong error"); res
        | `nfs3err_notempty ->
            debug_endline (s^" -> nfs3err_notempty error"); res
        | `nfs3err_dquot ->
            debug_endline (s^" -> nfs3err_dquot error"); res
        | `nfs3err_stale ->
            debug_endline (s^" -> nfs3err_stale error"); res
        | `nfs3err_remote ->
            debug_endline (s^" -> nfs3err_remote error"); res
        | `nfs3err_badhandle ->
            debug_endline (s^" -> nfs3err_badhandle error"); res
        | `nfs3err_not_sync ->
            debug_endline (s^" -> nfs3err_not_sync error"); res
        | `nfs3err_bad_cookie ->
            debug_endline (s^" -> nfs3err_bad_cookie error"); res
        | `nfs3err_notsupp ->
            debug_endline (s^" -> nfs3err_notsupp error"); res
        | `nfs3err_toosmall ->
            debug_endline (s^" -> nfs3err_toosmall error"); res
        | `nfs3err_serverfault ->
            debug_endline (s^" -> nfs3err_serverfault error"); res
        | `nfs3err_badtype ->
            debug_endline (s^" -> nfs3err_badtype error"); res
        | `nfs3err_jukebox ->
            debug_endline (s^" -> nfs3err_jukebox error"); res
        | `nfs3err_fprintnotfound ->
            debug_endline (s^" -> nfs3err_fprintnotfound error"); res
        | `nfs3err_aborted ->
            debug_endline (s^" -> nfs3err_aborted error"); res
        | _ -> debug_endline (s^" -> unknown error"); res
      end
    else res

let fake state fh =
  try
    Lookup.fhToFake state (Fh.ofString fh)
  with e -> "(fhToFake failed in nfs_api_debug.ml)"
            

let null state ses arg =
  debug_endline ("null");
  null state ses arg

let getattr state ses arg =
  debug1 ("getattr "^(fake state arg.data))
        getattr state ses arg

let setattr state ses arg =
  debug ("setattr "^(fake state arg.object'.data))
        setattr state ses arg

let lookup state ses arg =
  if !print_noent
    then
      debug ("lookup "^(fake state arg.dir.data)^" + "^arg.name)
            lookup state ses arg
    else
      lookup state ses arg

let access state ses arg =
  debug ("access "^(fake state arg.object'''.data))
        access state ses arg

let readlink state ses arg =
  debug ("readlink "^(fake state arg.data))
        readlink state ses arg
  
let read state ses arg =
  debug ("read "^(fake state arg.file.data))
        read state ses arg

let write state ses arg =
  debug ("write "^(fake state arg.file'.data))
        write state ses arg

let create state ses arg =
  debug ("create "^(fake state arg.where.dir.data)^" + "^arg.where.name)
        create state ses arg

let mkdir state ses arg =
  debug ("mkdir "^(fake state arg.where'.dir.data)^
                " + "^arg.where'.name)
        mkdir state ses arg

let symlink state ses arg =
  debug ("symlink "^(fake state arg.where''.dir.data)^
                " + "^arg.where''.name^" --> "^arg.symlink.symlink_data)
        symlink state ses arg

let remove state ses arg =
  debug ("remove "^(fake state arg.dir.data)^" + "^arg.name)
        remove state ses arg

let rmdir state ses arg =
  debug ("rmdir "^(fake state arg.dir.data)^" + "^arg.name)
        rmdir state ses arg

let rename state ses arg =
  debug ("rename "^(fake state arg.from.dir.data)^" + "^arg.from.name^
                " --> "^(fake state arg.to'.dir.data)^" + "^arg.to'.name)
        rename state ses arg

let readdir state ses arg =
  debug ("readdir "^(fake state arg.dir'.data)^
                " #"^(string_of_int (Rtypes.int_of_uint8 arg.cookie)))
        readdir state ses arg

let readdirplus state ses arg =
  debug ("readdirplus "^(fake state arg.dir''.data)^
                " #"^(string_of_int (Rtypes.int_of_uint8 arg.cookie'')))
        readdirplus state ses arg

let fsstat state ses arg =
  debug ("fsstat "^(fake state arg.data))
        fsstat state ses arg

let fsinfo state ses arg =
  debug ("fsinfo "^(fake state arg.data))
        fsinfo state ses arg

let pathconf state ses arg =
  debug ("pathconf "^(fake state arg.data))
        pathconf state ses arg

let commit state ses arg =
  debug ("commit "^(fake state arg.file'''.data))
        commit state ses arg

let null = wrap "null" null
let getattr = wrap "getattr" getattr
let setattr = wrap "setattr" setattr
let lookup = wrap "lookup" lookup
let access = wrap "access" access
let readlink = wrap "readlink" readlink
let read = wrap "read" read
let write = wrap "write" write
let create = wrap "create" create
let mkdir = wrap "mkdir" mkdir
let symlink = wrap "symlink" symlink
let mknod = wrap "mknod" mknod
let remove = wrap "remove" remove
let rmdir = wrap "rmdir" rmdir
let rename = wrap "rename" rename
let link = wrap "link" link
let readdir = wrap "readdir" readdir
let readdirplus = wrap "readdirplus" readdirplus
let fsstat = wrap "fsstat" fsstat
let fsinfo = wrap "fsinfo" fsinfo
let pathconf = wrap "pathconf" pathconf
let commit = wrap "commit" commit
