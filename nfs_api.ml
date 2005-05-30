(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

(* TODO:
   - copy on write files should be deleted on operation failure
   - rename of directories needs to create/modify .mount file
   - remove of dirs needs to delete .~ files if the dir is otherwise empty
   - mounted paths should be constrained
   - shadow dirs should be deleted if the op fails
   - dirTree ops should respect .~ mask files
 *)

open Common;;
open Unix;;
open Unix.LargeFile;;
open Nfs3_prot_caux;;

module SSet = Set.Make (String);;
module L = Lookup;;

(* Misc utility functions *****************************************************)
let block_size = ref 8192
let buffer_size = 8192
let startup_cookie = Rtypes.fp8_as_string (Rtypes.fp8_of_float (Unix.time ()))
let file_system_id = 51423
let lchown = Util.lchown

let s2fh = Fh.ofString
let fh2s = Fh.toString
let default_perm = 0o644

let checkaccess fattr mask =
  Rtypes.uint4_of_int 0

(*
let fileids = Hashtbl.create 5000
*)

let fileid_of_name = Util.hash64
(*
  Rtypes.uint8_of_int64 (Int64.abs (Util.hashString64 Int64.zero name))
*)

(*
let fileid_of_name name =
  let res = fileid_of_name name in
  (*let res = Rtypes.uint8_of_int (abs (Util.hashString 0 name)) in*)
  try if Hashtbl.find fileids res <> name
      then
        begin
          prerr_string ("fileid_of_name: collision for id "^
              (Int64.to_string (Rtypes.int64_of_uint8 res))^
              " old="^(Hashtbl.find fileids res)^
              " new="^name);
          prerr_newline ();
        end;
      if Util.startsWith name "/local"
        then
          begin
            prerr_string ("fileid_of_name: bad name ["^name^"]");
            prerr_newline ();
            failwith "fileid_of_name"
          end;
      res
  with Not_found -> Hashtbl.add fileids res name; res
*)

let nfs_time sec =
  let (frac, whole) = modf sec in
  let nano = frac *. 1000000000.0 in
  { seconds = Rtypes.uint4_of_int32 (Int32.of_float (whole +. 0.5));
    nseconds = Rtypes.uint4_of_int32 (Int32.of_float (nano +. 0.5)) }

let float_of_nfs_time nfstime =
  match nfstime with { seconds = s; nseconds = ns } ->
    Int32.to_float (Rtypes.int32_of_uint4 s) +.
    (Int32.to_float (Rtypes.int32_of_uint4 ns) /. 1000000000.0)

let nfs_stat info fake =
  {
    type' = begin
              match info.st_kind with
              | S_REG  -> nf3reg
              | S_DIR  -> nf3dir
              | S_CHR  -> nf3chr
              | S_BLK  -> nf3blk
              | S_LNK  -> nf3lnk
              | S_FIFO -> nf3fifo
              | S_SOCK -> nf3sock
            end;
    mode = Rtypes.uint4_of_int info.st_perm;
    nlink = Rtypes.uint4_of_int info.st_nlink;
    uid = Rtypes.uint4_of_int info.st_uid;
    gid = Rtypes.uint4_of_int info.st_gid;
    size = Rtypes.uint8_of_int64 info.st_size;
    used = Rtypes.uint8_of_int64 info.st_size;
    rdev = { major = Rtypes.uint4_of_int ((info.st_rdev land 0xff00) lsr 8);
             minor = Rtypes.uint4_of_int  (info.st_rdev land 0x00ff) };
    fsid = Rtypes.uint8_of_int file_system_id;
    fileid = fileid_of_name fake;
    atime = nfs_time info.st_atime;
    mtime = nfs_time info.st_mtime;
    ctime = nfs_time info.st_ctime
  }

let s64 n = Int64.to_string (Rtypes.int64_of_uint8 n)
let s32 n = Int32.to_string (Rtypes.int32_of_uint4 n)
let ss32 n = Int32.to_string (Rtypes.int32_of_int4 n)
(*
let print_nfs_stat info =
  prerr_string ("nfs_stat:"^
        " mode="^(s32 info.mode)^
        " fileid="^(s64 info.fileid)^
        " mtime="^(s32 info.mtime.seconds)^
        " ctime="^(s32 info.ctime.seconds)^
        " rdev=(major="^(s32 info.rdev.major)^
        " minor="^(s32 info.rdev.minor)^
        " fsid="^(s64 info.fsid)^
        " type'="^(ss32 info.type'));
  prerr_newline ();
*)

(* package a Unix stat record for a pre_op_attr result *)
let pre_op_attr info =
  `True { size' = Rtypes.uint8_of_int64 info.st_size;
          mtime' = nfs_time (info.st_mtime);
          ctime' = nfs_time (info.st_ctime) }

(*
let print_pre_op_attr info =
  match info with
  | `True { size' = s;
            mtime' = { seconds = msec; nseconds = mnsec };
            ctime' = { seconds = csec; nseconds = cnsec } } ->
    begin
      prerr_string ("pre_op_attr:"^
          " size="^(s64 s)^
          " mtime="^(s32 msec)^
          " ctime="^(s32 csec));
      prerr_newline ()
    end; info
  | `False -> info
*)

(* package a Unix stat record for a post_op_attr result *)
let post_op_attr info fake =
  `True (nfs_stat info fake)

(*
let print_post_op_attr info =
  match info with
  | `True i -> ignore (print_nfs_stat i); info
  | `False -> info
*)

let file_perm_of_sattr3 attr =
  match attr.mode' with
  | `True mode -> Rtypes.int_of_uint4 mode
  | _ -> failwith "file_perm_of_sattr3 called with `False"

let string8_of_int i =
  let res = String.create 8 in
  res.[0] <- char_of_int ((i land 0x7f000000) lsr 24);
  res.[1] <- char_of_int ((i land 0x00ff0000) lsr 16);
  res.[2] <- char_of_int ((i land 0x0000ff00) lsr 8);
  res.[3] <- char_of_int  (i land 0x000000ff);
  res.[4] <- '\000';
  res.[5] <- '\000';
  res.[6] <- '\000';
  res.[7] <- '\000';
  res

let int_of_string8 s =
  ((int_of_char s.[0]) lsl 24) lor
  ((int_of_char s.[1]) lsl 16) lor
  ((int_of_char s.[2]) lsl 8)  lor
   (int_of_char s.[3])

let user session = session (*
  let (uid, gid, gid_arr, hostname) =
      Rpc_auth_sys.parse_user_name (Rpc_server.get_user session) in
  (uid, gid :: (Array.to_list gid_arr), hostname) *)

let perm (* info *) =
  let read  =  Rtypes.int_of_uint4 access3_read in
  let write = (Rtypes.int_of_uint4 access3_modify) lor
              (Rtypes.int_of_uint4 access3_extend) lor
              (Rtypes.int_of_uint4 access3_delete) in
  let exec  = (Rtypes.int_of_uint4 access3_lookup) lor
              (Rtypes.int_of_uint4 access3_execute) in
  let p info =
    let owner = (if info.st_perm land 0o400 <> 0 then read  else 0) lor
                (if info.st_perm land 0o200 <> 0 then write else 0) lor
                (if info.st_perm land 0o100 <> 0 then exec  else 0) in
    let group = (if info.st_perm land 0o040 <> 0 then read  else 0) lor
                (if info.st_perm land 0o020 <> 0 then write else 0) lor
                (if info.st_perm land 0o010 <> 0 then exec  else 0) in
    let world = (if info.st_perm land 0o004 <> 0 then read  else 0) lor
                (if info.st_perm land 0o002 <> 0 then write else 0) lor
                (if info.st_perm land 0o001 <> 0 then exec  else 0) in
    (owner, group, world) in
  p

let getperm_user user info =
  let (uid, gidlist, hostname) = user in
  let (owner, group, world) = perm info in
  if uid = info.st_uid then owner
  else if List.mem info.st_gid gidlist then group
  else world

let getperm session (* info *) = getperm_user (user session)

let checkperm_user (uid, gids, hostname) info kind =
  let kind = Rtypes.int_of_uint4 kind in
  uid = 0 || (kind land getperm_user (uid, gids, hostname) info <> 0)

let checkperm_chown (uid, _, _) info =
  uid = 0

let checkperm_mknod (uid, _, _) =
  uid = 0

let checkperm session (* info kind *) = checkperm_user (user session)

exception Not_writeable
exception Wrong_filetype

let create_shadow_dirs state user path file =
  let rec get_perm parent =
    try let info = lstat parent in
        if info.st_kind <> S_DIR then raise Wrong_filetype else info.st_perm
    with _ ->
        let perm = get_perm (Util.dirname parent) in
        mkdir parent perm;
        let (uid, gids, hostname) = user in
        chown parent uid (List.hd gids);
        perm
  in
  ignore (get_perm (Util.dirname path))

let copy_file user name_in name_out info =
  if info.st_kind <> S_REG then raise Wrong_filetype else
  let fp_in = openfile name_in [O_RDONLY] 0 in
  try
    let fp_out = openfile name_out [O_WRONLY; O_CREAT; O_EXCL] info.st_perm in
    (try
      let (uid, gids, hostname) = user in
      fchown fp_out uid (List.hd gids);
      let buff = String.create buffer_size in
      let rec copy () =
        let size = read fp_in buff 0 buffer_size in
        if size > 0 then
          begin
            if size <> (write fp_out buff 0 size) then failwith "copy_file"
            else copy ()
          end
        else ()
      in
      copy ();
      close fp_out
    with e -> close fp_out; raise e);
    close fp_in
  with e-> close fp_in; raise e

let copy_link user name_in name_out info =
  if info.st_kind <> S_LNK then raise Wrong_filetype else
  let lnk = readlink name_in in
  symlink name_out lnk;
  let (uid, gids, hostname) = user in
  lchown name_out uid (List.hd gids)

let copy_dir user name_in name_out info =
  if info.st_kind <> S_DIR then raise Wrong_filetype else
  mkdir name_out info.st_perm;
  let (uid, gids, hostname) = user in
  chown name_out uid (List.hd gids)

(* TODO: more squashing--shadow dir creation, dir permission changes, etc. *)
let copy_on_write state user file paths =
  match paths with
  | (Some (readname, readinfo), Some writename) ->
    if readname = writename then (writename, readinfo) else
      begin
        let fh = match file with FhOnly fh -> fh | FhName (fh, _) -> fh in
        L.squash state (FhOnly fh);
        match readinfo.st_kind with
        | S_REG ->
          begin
            create_shadow_dirs state user writename file;
            copy_file user readname writename readinfo;
            (writename, lstat writename)
          end
        | S_LNK ->
          begin
            create_shadow_dirs state user writename file;
            copy_link user readname writename readinfo;
            (writename, lstat writename)
          end
        | S_DIR ->
          begin
            create_shadow_dirs state user writename file;
            copy_dir user readname writename readinfo;
            (writename, lstat writename)
          end
        | _ -> raise Not_writeable
      end
  | (Some _, None) -> raise Not_writeable
  | (None, _) -> raise Not_found

let remove_mask path =
  let hidename =
      Util.concatPath (Util.dirname path) (".~"^(Util.filename path)) in
  try
    let info = lstat hidename in
    (* this gets called by create ops, so the user already has permission to
     * the directory *)
    if info.st_kind = S_REG &&
       info.st_size = 0L
      then Unix.unlink hidename
  with Unix_error _ -> ()

(* Exceptions *****************************************************************)

exception Noent
exception Notdir
exception Inval
exception Bad_cookie
exception Bad_type
exception Not_sync
exception Notsupp
exception Acces
exception Exist
exception Isdir
exception Hide_failed
exception Perm

exception Unimplemented

let unixerr err arg =
  match err with
  | EPERM                       -> `nfs3err_perm arg
  | ENOENT                      -> `nfs3err_noent arg
  | EIO                         -> `nfs3err_io arg
  | ENXIO                       -> `nfs3err_nxio arg
  | EACCES                      -> `nfs3err_acces arg
  | EEXIST                      -> `nfs3err_exist arg
  | EXDEV                       -> `nfs3err_xdev arg
  | ENODEV                      -> `nfs3err_nodev arg
  | ENOTDIR                     -> `nfs3err_notdir arg
  | EISDIR                      -> `nfs3err_isdir arg
  | EINVAL                      -> `nfs3err_inval arg
  | EFBIG                       -> `nfs3err_fbig arg
  | ENOSPC                      -> `nfs3err_nospc arg
  | EROFS                       -> `nfs3err_rofs arg
  | EMLINK                      -> `nfs3err_mlink arg
  | ENAMETOOLONG                -> `nfs3err_nametoolong arg
  | ENOTEMPTY                   -> `nfs3err_notempty arg
(*| EDQUOT                      -> `nfs3err_dquot arg           *)
(*| ESTALE                      -> `nfs3err_stale arg           *)
(*| EREMOTE                     -> `nfs3err_remote arg          *)
  | EBADF                       -> `nfs3err_badhandle arg
(*| ENOT_SYNC                   -> `nfs3err_not_sync arg        *)
(*| EBAD_COOKIE                 -> `nfs3err_bad_cookie arg      *)
(*| ENOTSUPP                    -> `nfs3err_notsupp arg         *)
(*| ETOOSMALL                   -> `nfs3err_toosmall arg        *)
(*| EBADTYPE                    -> `nfs3err_badtype arg         *)
(*| EJUKEBOX                    -> `nfs3err_jukebox arg         *)
(*| EFPRINTNOTFOUND             -> `nfs3err_fprintnotfound arg  *)
(*| EABORTED                    -> `nfs3err_aborted arg         *)
  | _                           -> `nfs3err_serverfault arg

let handle_exp exp res =
(*
  if exp <> Noent then
    begin
      prerr_endline ("*** exception "^(Printexc.to_string exp))
    end;
*)
  match exp with
  | Noent                       -> `nfs3err_noent res
  | Notdir                      -> `nfs3err_notdir res
  | Inval                       -> `nfs3err_inval res
  | Bad_cookie                  -> `nfs3err_bad_cookie res
  | Bad_type                    -> `nfs3err_badtype res
  | Not_sync                    -> `nfs3err_not_sync res
  | Notsupp                     -> `nfs3err_notsupp res
  | Acces                       -> `nfs3err_acces res
  | Exist                       -> `nfs3err_exist res
  | Isdir                       -> `nfs3err_isdir res
  | Not_writeable               -> `nfs3err_acces res
  | Wrong_filetype              -> `nfs3err_acces res
  | Unix_error (err, s1, s2)    -> unixerr err res
  | Unimplemented               -> `nfs3err_notsupp res
  | Hide_failed                 -> `nfs3err_acces res
  | Perm                        -> `nfs3err_perm res
  | Not_found                   -> `nfs3err_stale res
  | _                           -> `nfs3err_serverfault res

let hide_fh_name map session fh name =
  match L.lookupHide map fh name with
  | (true, Some path) ->
    begin
      let (uid, gids, hostname) = user session in
      create_shadow_dirs map (uid, gids, hostname) path (FhName (fh, name));
      let res = Util.create path uid (List.hd gids) default_perm false in
      if res <> 0 then raise Hide_failed
    end
  | (true, None) -> raise Hide_failed
  | (false, _) -> ()

(* Read operations ************************************************************)

let lookup map session arg name info fake =
  if info.st_kind <> S_DIR then raise Notdir else
  if not (checkperm session info access3_lookup) then raise Acces else
  if not (Util.validateName arg.name) then raise Noent else
  begin
    let dirFake = L.fhToFake map (s2fh arg.dir.data) in
    let newFake = Util.concatPath dirFake arg.name in
    let newFh = L.fakeToNewFh map newFake in
    match L.lookupRead map newFh with
    | Some (fname, finfo) ->
      let fattr = nfs_stat finfo newFake in
      `nfs3_ok {
        object'' = { data            = fh2s newFh };
                     obj_attributes' = `True fattr;
                     dir_attributes  = `True (nfs_stat info fake) }
    | None -> raise Noent
  end

let access map session arg name info fake =
  let attr = nfs_stat info fake in
  let (uid, gids, hostname) = user session in
  let perms =
    if uid = 0 then Rtypes.int_of_uint4 arg.access
    else (Rtypes.int_of_uint4 arg.access) land (getperm session info)
  in
  `nfs3_ok { obj_attributes'' = (`True attr);
             access' = Rtypes.uint4_of_int perms }

let readlink map session arg name info fake =
  if not (checkperm session info access3_read) then raise Acces
  else if info.st_kind <> S_LNK then raise Inval
  else `nfs3_ok { symlink_attributes = (`True (nfs_stat info fake));
                  data' = Unix.readlink name }

let read map session arg name info fake =
  if not (checkperm session info access3_read) then raise Acces else
  let start = Rtypes.int64_of_uint8 arg.offset in
  let len = Rtypes.int_of_uint4 arg.count in
  let buff = String.create len in
  if start >= info.st_size || len = 0
    then `nfs3_ok { file_attributes = (`True (nfs_stat info fake));
                    count' = Rtypes.uint4_of_int 0;
                    eof = start >= info.st_size;
                    data'' = "" } else
  let readres = ref 0 in
  let fp = openfile name [O_RDONLY] 0 in
  (try
    readres :=
        (let pos = lseek fp start SEEK_SET in
        Unix.read fp buff 0 len);
    close fp
  with e -> close fp; raise e);

  let count = !readres in
  let result = if count = len then buff else String.sub buff 0 count in
  let eof = count = 0 ||
            (Int64.add start (Int64.of_int count)) = info.st_size
  in
  `nfs3_ok { file_attributes = `True (nfs_stat info fake);
             count' = Rtypes.uint4_of_int count;
             eof = eof;
             data'' = result }

(* apply the given function to every file in the given directory *)
(*
let readdir_iter f dir =
  try let handle = opendir dir in
      let rec loop () =
        try f (readdir handle);
            loop ()
        with End_of_file -> closedir handle
           | e -> closedir handle; raise e
      in
      loop ()
  with Unix_error _ -> ()
*)

let readdir_iter f dir =
  try
    f "."; f "..";
    Array.iter f (Sys.readdir dir)
  with Unix_error _ -> ()
     | Sys_error _ -> ()

(* given a list of stacked directories, gather a full list of visible files *)
let readdir_gather dirlist user =
  let files = ref [] in
  let masked = ref SSet.empty in
  let scanOneDir dir =
    if try not (checkperm_user user (lstat dir) access3_read) with _ -> true
    then () else
    let newMasked = ref SSet.empty in
    let nextFile name =
      if SSet.mem name !masked then ()
      else
        begin
          files := (dir, name) :: !files;
          newMasked := SSet.add name !newMasked;
          let len = String.length name in
          if len > 2 && String.sub name 0 2 = ".~"
            then newMasked := SSet.add (String.sub name 2 (len - 2)) !newMasked
        end
    in
    readdir_iter nextFile dir;
    masked := SSet.union !masked !newMasked
  in
  List.iter scanOneDir dirlist;
  !files

(* get a list of visible files, from cache if available, else scan dirs *)
let readdir_getlist map fh info counter cookie user =
  try
    (counter, cookie, L.dirFhInfoToFileList map fh info counter cookie)
  with Not_found ->
    (0, 0, readdir_gather (L.lookupReaddir map fh) user)
  (*
    let lst = readdir_gather (L.lookupReaddir map fh) user in
    L.addDirFhInfoFileList map fh info lst;
    lst*)

type readdir_res = End | Elt of (string * string * Rtypes.uint8 * readdir_res)

exception Readdir_too_high

(* gather readdir info for the appropriate range of files using the cookie *)
(* this forms the basis for readdir and readdirplus *)
let readdir_common map user fh info cookie cookieverf max maxPlus isPlus =
  let first = Rtypes.int_of_uint8 cookie in
  let cookie = int_of_string8 cookieverf in
  let (counter, cookiestart, files) =
      readdir_getlist map fh info first cookie user in
  let hash = ref cookiestart in
  let eof = ref false in
  let rec f lst n size sizePlus =
    match lst with
    | [] -> if n < first
              then raise Readdir_too_high
              else (eof := true; L.squashDirFh map fh; End)
    | (prefix, file) :: xs ->
        let size' = size + (Util.readdir_entry_size file) in
        let sizePlus' = sizePlus + (Util.readdirplus_entry_size file) in
        if n < first then (hash := Util.hashString !hash file;
                          f xs (succ n) size sizePlus)
        (* ignore the cookie if n=0 because some clients don't supply 0 *)
        else if n = first && n <> 0 && !hash <> cookie then raise Bad_cookie
        else if size' > max || (isPlus && sizePlus' > maxPlus)
          then (L.addDirFhInfoFileList map fh info n !hash lst; End)
        else begin
               hash := Util.hashString !hash file;
               Elt (prefix, file, Rtypes.uint8_of_int (succ n),
                   f xs (succ n) size' sizePlus')
             end
  in
  let lst = f files counter (Util.readdir_base ()) (Util.readdirplus_base ())
  in (!eof, lst, !hash)

let readdir map session arg name info fake =
  let fh = s2fh arg.dir'.data in
  let maxResultSize = Rtypes.int_of_uint4 arg.count'''' in
  if not (checkperm session info access3_read) then raise Acces else
  if info.st_kind <> S_DIR then raise Notdir else
  let rec package elt =
    begin
      match elt with
      | Elt (prefix, fname, cookie, next) ->
          (* compute the fh of the entry to force it into the cache *)
          let newFake = Util.concatPath fake fname in
          let fh' = L.fakeToNewFh map newFake in
          Some { fileid' = fileid_of_name newFake;
                 name' = fname;
                 cookie' = cookie;
                 nextentry = package next }
      | End -> None
    end
  in
  let (eof, lst, hash) =
      try
        readdir_common map (user session) fh info arg.cookie arg.cookieverf
                      maxResultSize 0 false
      with Readdir_too_high ->
        readdir_common map (user session) fh info (Rtypes.uint8_of_int 0)
                      (string8_of_int 0) maxResultSize 0 false
  in
  `nfs3_ok { dir_attributes' = `True (nfs_stat info fake);
             cookieverf' = string8_of_int hash;
             reply = { entries = package lst; eof' = eof } }

let readdirplus map session arg name info fake =
  let fh = s2fh arg.dir''.data in
  let maxResultSize = Rtypes.int_of_uint4 arg.dircount in
  let maxResultPlusSize = Rtypes.int_of_uint4 arg.maxcount in
  if info.st_kind <> S_DIR then raise Notdir else
  let rec package elt =
    begin
      match elt with
      | Elt (prefix, fname, cookie, next) ->
          let newFake = Util.concatPath fake fname in
          let fh' = L.fakeToNewFh map newFake in
          let newReal = Util.concatPath prefix fname in
          Some { fileid'' = fileid_of_name newFake;
                 name'' = fname;
                 cookie''' = cookie;
                 name_attributes =
                     (try `True (nfs_stat (lstat newReal) newFake)
                      with _ -> `False);
                 name_handle = `True { data = fh2s fh' };
                 nextentry' = package next }
      | End -> None
    end
  in
  let (eof, lst, hash) =
      try
        readdir_common map (user session) fh info arg.cookie'' arg.cookieverf''
                      maxResultSize maxResultPlusSize true
      with Readdir_too_high ->
        readdir_common map (user session) fh info (Rtypes.uint8_of_int 0)
                      (string8_of_int 0) maxResultSize maxResultPlusSize true
  in
  `nfs3_ok { dir_attributes'' = `True (nfs_stat info fake);
             cookieverf''' = string8_of_int hash;
             reply' = { entries' = package lst; eof'' = eof } }

let fsstat map session arg name info fake =
  `nfs3_ok {
      obj_attributes''' = `True (nfs_stat info fake);
      tbytes = Rtypes.uint8_of_int 1000000000;
      fbytes = Rtypes.uint8_of_int 500000000;
      abytes = Rtypes.uint8_of_int 500000000;
      tfiles = Rtypes.uint8_of_int 100000;
      ffiles = Rtypes.uint8_of_int 50000;
      afiles = Rtypes.uint8_of_int 50000;
      invarsec = Rtypes.uint4_of_int 30 }

let fsinfo map session arg name info fake =
  `nfs3_ok {
      obj_attributes'''' = `True (nfs_stat info fake);
      rtmax = Rtypes.uint4_of_int 8192;
      rtpref = Rtypes.uint4_of_int 8192;
      rtmult = Rtypes.uint4_of_int 8192;
      wtmax = Rtypes.uint4_of_int 8192;
      wtpref = Rtypes.uint4_of_int 8192;
      wtmult = Rtypes.uint4_of_int 8192;
      dtpref = Rtypes.uint4_of_int 8192;
      maxfilesize = Rtypes.uint8_of_int 0x3fffffff;
      time_delta = { seconds = Rtypes.uint4_of_int 1;
                     nseconds = Rtypes.uint4_of_int 0 };
      properties = Rtypes.uint4_of_int (Rtypes.int_of_uint4 fsf3_symlink lor
                                        Rtypes.int_of_uint4 fsf3_homogeneous lor
                                        Rtypes.int_of_uint4 fsf3_cansettime) }

let pathconf map session arg name info fake =
  `nfs3_ok {
      obj_attributes''''' = `True (nfs_stat info fake);
      linkmax = Rtypes.uint4_of_int 1000;
      name_max = Rtypes.uint4_of_int 127;
      no_trunc = true;
      chown_restricted = true;
      case_insensitive = false;
      case_preserving = true }

(* Write operations ***********************************************************)

let setattr map session arg path info readinfo fake =
  begin
    match arg.guard with
    | `True time -> if time <> nfs_time readinfo.st_ctime
                    then raise Not_sync
    | _ -> ()
  end;

  let user = (user session) in

  (* mode changes *)
  begin
    if not (checkperm_user user info access3_modify) then raise Acces;

    match arg.new_attributes.mode' with
    | `True mode' -> let mode = Rtypes.int_of_uint4 mode' in
                     if mode <> info.st_perm
                       then chmod path mode
    | `False -> ()
  end;

  (* size changes *)
  begin
    if not (checkperm_user user info access3_modify) then raise Acces;

    match arg.new_attributes.size'' with
    | `True size'' -> let size = Rtypes.int64_of_uint8 size'' in
                      if size <> info.st_size
                        then truncate path size
    | `False -> ()
  end;

  (* time changes *)
  begin
    if not (checkperm_user user info access3_modify) then raise Acces;

    let currenttime = gettimeofday () in
    let atime = (match arg.new_attributes.atime' with
                 | `set_to_client_time nfstime ->
                     Some (float_of_nfs_time nfstime)
                 | `dont_change -> None
                 | `set_to_server_time -> Some currenttime) in
    let mtime = (match arg.new_attributes.mtime'' with
                 | `set_to_client_time nfstime ->
                     Some (float_of_nfs_time nfstime)
                 | `dont_change -> None
                 | `set_to_server_time -> Some currenttime) in
    match (atime, mtime) with
    | (Some atime, Some mtime) -> Unix.utimes path atime mtime
    | (Some atime, None) -> Unix.utimes path atime info.st_mtime
    | (None, Some mtime) -> Unix.utimes path info.st_atime mtime
    | (None, None) -> ()
  end;

  (* uid/gid changes *)
  begin
    let newuid = (match arg.new_attributes.uid' with
                  | `True id when Rtypes.int_of_uint4 id <> info.st_uid ->
                      Some (Rtypes.int_of_uint4 id)
                  | _ -> None) in
    let newgid = (match arg.new_attributes.gid' with
                  | `True id when Rtypes.int_of_uint4 id <> info.st_gid ->
                      Some (Rtypes.int_of_uint4 id)
                  | _ -> None) in
    let checkgid gidopt (_, gids, _) =
      match gidopt with None -> false | Some gid -> not (List.mem gid gids) in
    if (newuid <> None || (checkgid newgid user)) &&
        not (checkperm_chown user info)
      then raise Perm
      else
        match (newuid, newgid) with
        | (Some uid, Some gid) -> lchown path uid gid
        | (Some uid, None) -> lchown path uid info.st_gid
        | (None, Some gid) -> lchown path info.st_uid gid
        | (None, None) -> ()
  end;

  match L.lookupRead map (s2fh arg.object'.data) with
  | Some (postname, postinfo) ->
    `nfs3_ok { before = pre_op_attr readinfo;
               after = post_op_attr postinfo fake }
  | None -> raise Not_sync

let write map session arg name info readinfo fake =
  if not (checkperm session info access3_modify) then raise Acces;
  let len = Rtypes.int_of_uint4 arg.count'' in

  let writeres = ref 0 in
  let fp = openfile name [O_WRONLY] 0 in
  (try
    writeres :=
      (let pos = lseek fp (Rtypes.int64_of_uint8 arg.offset') SEEK_SET in
      Unix.write fp arg.data''' 0 len);
    close fp
  with e -> close fp; raise e);

  let count = !writeres in
  match L.lookupRead map (s2fh arg.file'.data) with
  | Some (postname, postinfo) ->
    `nfs3_ok { file_wcc = { before = pre_op_attr readinfo;
                            after = post_op_attr postinfo fake };
               count''' = Rtypes.uint4_of_int count;
               committed = file_sync;
               verf = startup_cookie }
  | None -> raise Not_sync

let commit map session arg name info readinfo = raise Unimplemented

(* Create operations **********************************************************)

let create map session arg path old wcc =
  (* permissions check happens in the wrapper *)
  if not (Util.validateName arg.where.name) then raise Acces else
  let (guarded, attrs) =
    (match arg.how with
    | `guarded a -> (true, a)
    | `unchecked a -> (false, a)
    | _ -> raise Notsupp)
  in
  begin
    match old with
    | Some (oldpath, oldinfo) ->
      if path = oldpath && (guarded || oldinfo.st_kind <> S_REG)
        then raise Exist
    | None -> ()
  end;
  let (uid, gid) =
    (match (user session) with
    | (uid, gid :: _, _) -> (uid, gid)
    | _ -> raise Perm)
  in
  let res = Util.create path uid gid (file_perm_of_sattr3 attrs) guarded in
  if res <> 0 then raise Perm else
  remove_mask path;
  let dirFake = L.fhToFake map (s2fh arg.where.dir.data) in
  let newFake = Util.concatPath dirFake arg.where.name in
  let newFh = L.fakeToNewFh map newFake in
  match L.lookupRead map newFh with
  | Some (_, info) ->
    `nfs3_ok { obj = `True { data = fh2s newFh };
               obj_attributes = post_op_attr info newFake;
               dir_wcc = wcc () }
  | None -> raise Not_sync

let mkdir map session arg path old wcc =
  (* permissions check happens in the wrapper *)
  if not (Util.validateName arg.where'.name) then raise Acces else
  begin
    match old with
    | Some (oldpath, oldinfo) ->
      if path = oldpath then raise Exist
    | None -> ()
  end;
  Unix.mkdir path (file_perm_of_sattr3 arg.attributes);
  begin
    match (user session) with
    | (uid, gid :: _, hostname) -> chown path uid gid
    | _ -> raise Perm
  end;
  remove_mask path;
  let dirFake = L.fhToFake map (s2fh arg.where'.dir.data) in
  let newFake = Util.concatPath dirFake arg.where'.name in
  let newFh = L.fakeToNewFh map newFake in
  match L.lookupRead map newFh with
  | Some (_, info) ->
    `nfs3_ok { obj = `True { data = fh2s newFh };
               obj_attributes = post_op_attr info newFake;
               dir_wcc = wcc () }
  | None -> raise Not_sync

let symlink map session arg path old wcc =
  (* permissions check happens in the wrapper *)
  if not (Util.validateName arg.where''.name) then raise Acces else
  begin
    match old with
    | Some (oldpath, oldinfo) ->
      if path = oldpath then raise Exist
    | None -> ()
  end;
  Unix.symlink arg.symlink.symlink_data path;
  begin
    match (user session) with
    | (uid, gid :: _, hostname) -> lchown path uid gid
    | _ -> raise Perm
  end;
  remove_mask path;
  let dirFake = L.fhToFake map (s2fh arg.where''.dir.data) in
  let newFake = Util.concatPath dirFake arg.where''.name in
  let newFh = L.fakeToNewFh map newFake in
  match L.lookupRead map newFh with
  | Some (_, info) ->
    `nfs3_ok { obj = `True { data = fh2s newFh };
               obj_attributes = post_op_attr info newFake;
               dir_wcc = wcc () }
  | None -> raise Not_sync

let mknod map session arg path old wcc =
  if not (Util.validateName arg.where'''.name) then raise Acces else
  begin
    match old with
    | Some (oldpath, oldinfo) ->
      if path = oldpath then raise Exist
    | None -> ()
  end;
  begin
    let decode_sattr3 user attr =
      let getint default set_uint32 =
        match set_uint32 with
        | `True u32 -> Rtypes.int_of_uint4 u32
        | `False -> default
      in
      let (uid, gids, _) = user in
      let uid = getint uid attr.uid' in
      let gid = getint (List.hd gids) attr.gid' in
      let mode = getint default_perm attr.mode' in
      (uid, gid, mode)
    in
    let decode_dev { major = major; minor = minor } =
      Int64.logor
        (Int64.shift_left (Rtypes.int64_of_uint4 major) 8)
        (Rtypes.int64_of_uint4 minor)
    in
    let user = user session in
    match arg.what with
    | `nf3chr device ->
      begin
        if not (checkperm_mknod user) then raise Perm else
        let (uid, gid, mode) = decode_sattr3 user device.dev_attributes in
        let dev = decode_dev device.spec in
        try
          Util.mknod path (mode lor 0o020000) dev uid gid
        with Failure _ -> raise Acces
      end
    | `nf3blk device ->
      begin
        if not (checkperm_mknod user) then raise Perm else
        let (uid, gid, mode) = decode_sattr3 user device.dev_attributes in
        let dev = decode_dev device.spec in
        try
          Util.mknod path (mode lor 0o060000) dev uid gid
        with Failure _ -> raise Acces
      end
    | `nf3fifo attr ->
      begin
        let (uid, gid, mode) = decode_sattr3 user attr in
        try
          Util.mknod path (mode lor 0o10000) Int64.zero uid gid
        with Failure _ -> raise Acces
      end
    | `nf3sock _ -> raise Notsupp
    | _ -> raise Bad_type
  end;
  remove_mask path;
  let dirFake = L.fhToFake map (s2fh arg.where'''.dir.data) in
  let newFake = Util.concatPath dirFake arg.where'''.name in
  let newFh = L.fakeToNewFh map newFake in
  match L.lookupRead map newFh with
  | Some (_, info) ->
    `nfs3_ok { obj = `True { data = fh2s newFh };
               obj_attributes = post_op_attr info newFake;
               dir_wcc = wcc () }
  | None -> raise Not_sync

(* Remove operations **********************************************************)

let remove map session arg readname readinfo writename wcc =
  if readinfo.st_kind = S_DIR then raise Isdir else
  if readname = writename then unlink writename;
  hide_fh_name map session (s2fh arg.dir.data) arg.name;
  `nfs3_ok (wcc ())

let rmdir map session arg readname readinfo writename wcc =
  if readinfo.st_kind <> S_DIR then raise Notdir else
  if readname = writename then rmdir writename;
  hide_fh_name map session (s2fh arg.dir.data) arg.name;
  `nfs3_ok (wcc ())

(* Special case operations ****************************************************)

let rename map session arg =
  (* first examine the directories and prepare return values *)
  let fromfh = s2fh arg.from.dir.data in
  let tofh = s2fh arg.to'.dir.data in
  let fromfake = L.fhToFake map fromfh in
  let tofake = L.fhToFake map tofh in
  let pre_from = L.lookupRead map fromfh in
  let pre_to = L.lookupRead map tofh in
  if pre_from = None then `nfs3err_stale { fromdir_wcc = { before = `False;
                                                           after = `False };
                                           todir_wcc =   { before = `False;
                                                           after = `False } }
  else
  if pre_to = None then `nfs3err_stale { fromdir_wcc = { before = `False;
                                                         after = `False };
                                         todir_wcc =   { before = `False;
                                                         after = `False } }
  else

  (* package a function to gather the return result *)
  let wcc () =
    let wcc_from () =
      { before = (match pre_from with
                  | Some (_, info) -> pre_op_attr info
                  | None -> `False);
        after =  (match L.lookupRead map fromfh with
                  | Some (_, info) -> post_op_attr info fromfake
                  | None -> `False) }
    in
    let wcc_to () =
      { before = (match pre_to with
                  | Some (_, info) -> pre_op_attr info
                  | None -> `False);
        after =  (match L.lookupRead map tofh with
                  | Some (_, info) -> post_op_attr info tofake
                  | None -> `False) }
    in
    { fromdir_wcc = wcc_from (); todir_wcc = wcc_to () }
  in

  (* do the actual rename operation *)
  match L.lookupWrite map (FhName (fromfh, arg.from.name)) with
  | (_, None) -> `nfs3err_acces (wcc ())
  | (None, _) -> `nfs3err_noent (wcc ())
  | (Some (readname, readinfo), Some writename) ->
    begin
      match L.lookupCreate map tofh arg.to'.name with
      | (None, _) -> `nfs3err_acces (wcc ())
      | (Some newpath, _) ->
        begin
          L.mountFileCheck map newpath;
          try
            if readname = writename
              then
                (* we have write permission so do a normal rename *)
                begin
                  L.mountFileCheck map writename;
                  create_shadow_dirs map (user session) newpath (FhOnly tofh);
                  Unix.rename writename newpath;
                  hide_fh_name map session fromfh arg.from.name
                end
              else
                (* the source is read-only so make a copy *)
                begin
                  ignore (copy_on_write map (user session) (FhOnly tofh)
                            (Some (readname, readinfo), Some newpath));
                  hide_fh_name map session fromfh arg.from.name
                end;
            `nfs3_ok (wcc ())
          with e -> handle_exp e (wcc ())
        end
    end

let null map session arg =
  ()

let getattr map session arg =
  let objfh = s2fh arg.data in
  let fake = L.fhToFake map objfh in
  match L.lookupRead map objfh with
  | Some (name, info) -> `nfs3_ok (nfs_stat info fake)
  | None -> `nfs3err_stale

let link map session arg =
  let objfh = s2fh arg.file''.data in
  let dirfh = s2fh arg.link.dir.data in
  let dirFake = L.fhToFake map dirfh in
  let linkname = arg.link.name in
  let newFake = Util.concatPath dirFake linkname in
  let newFh = L.fakeToNewFh map newFake in
  match (L.lookupRead map objfh, L.lookupRead map dirfh) with
  | (None, Some (dirname, dirinfo)) ->
      `nfs3err_stale  { file_attributes' = `False;
                        linkdir_wcc = { before = pre_op_attr dirinfo;
                                        after = post_op_attr dirinfo dirFake } }
  | (None, _) | (_, None) ->
      `nfs3err_stale  { file_attributes' = `False;
                        linkdir_wcc = { before = `False; after = `False } }
  | (Some (name, info), Some (dirname, dirinfo)) ->
      let wcc () =
        { file_attributes' =
            (match L.lookupRead map newFh with
            | Some (_, info) -> post_op_attr info newFake
            | None -> `False);
          linkdir_wcc =
            { before = pre_op_attr dirinfo;
              after = (match L.lookupRead map dirfh with
                      | Some (_, info) -> post_op_attr info dirFake
                      | None -> `False) } }
      in
      if dirinfo.st_kind <> S_DIR then `nfs3err_notdir (wcc ())
      else
        begin
          match L.lookupCreate map dirfh linkname with
          | (None, _) -> `nfs3err_acces (wcc ())
          | (Some path, old) ->
            try
              create_shadow_dirs map (user session) path
                  (FhName (dirfh, linkname));
              let createdirinfo = lstat (Util.dirname path) in
              if not (checkperm session createdirinfo access3_modify &&
                      Util.validateName linkname)
                then raise Acces;
              L.mountFileCheck map path;
              (match old with
              | Some (oldpath, _) when oldpath = path -> raise Exist
              | _ -> ());
              Unix.link name path;
              `nfs3_ok (wcc ())
            with e -> handle_exp e (wcc ())
        end


(* Wrappers *******************************************************************)

(* Wrap the read operations ***************************************************)

let wrap_readop name f map session arg objfh =
  try
    let objfh = s2fh objfh in
    let fake = L.fhToFake map objfh in
    match L.lookupRead map objfh with
    | Some (name, info) ->
      begin
        let postopattr = `True (nfs_stat info fake) in
        try f map session arg name info fake
        with e -> handle_exp e postopattr
      end
    | None -> `nfs3err_stale `False
  with e ->
(*    prerr_endline ("exception "^(Printexc.to_string e)^" in "^name^"\n"); *)
    handle_exp e `False

let lookup map session arg = wrap_readop "lookup"
    lookup map session arg arg.dir.data
let access map session arg = wrap_readop "access"
    access map session arg arg.object'''.data
let readlink map session arg = wrap_readop "readlink"
    readlink map session arg arg.data
let read map session arg = wrap_readop "read"
    read map session arg arg.file.data
let readdir map session arg = wrap_readop "readdir"
    readdir map session arg arg.dir'.data
let readdirplus map session arg = wrap_readop "readdirplus"
    readdirplus map session arg arg.dir''.data
let fsstat map session arg = wrap_readop "fsstat"
    fsstat map session arg arg.data
let fsinfo map session arg = wrap_readop "fsinfo"
    fsinfo map session arg arg.data
let pathconf map session arg = wrap_readop "pathconf"
    pathconf map session arg arg.data

(* Wrap the write operations **************************************************)

let wrap_writeop name f map session arg objfh =
  try
    let objfh = s2fh objfh in
    let fake = L.fhToFake map objfh in
    match L.lookupWrite map (FhOnly objfh) with
    | (Some (readname, readinfo), Some writename) as paths ->
      begin
        let (path, info) = if readname = writename
                           then (readname, readinfo)
                           else copy_on_write map (user session)
                                    (FhOnly objfh) paths
        in
        let wcc () =
          { before = pre_op_attr readinfo;
            after = (match L.lookupRead map objfh with
                     | Some (_, info) -> post_op_attr info fake
                     | None -> `False) }
        in
        try
          L.mountFileCheck map path;
          let res = f map session arg path info readinfo fake in
          res
        with e -> handle_exp e (wcc ())
      end
    | (Some (readname, readinfo), None) ->
      `nfs3err_acces { before = pre_op_attr readinfo;
                       after = post_op_attr readinfo fake }
    | (None, _) ->
      `nfs3err_stale { before = `False; after = `False }
  with e ->
(*    prerr_endline ("exception "^(Printexc.to_string e)^" in "^name^"\n");*)
    handle_exp e { before = `False; after = `False }

let setattr map session arg = wrap_writeop "setattr"
    setattr map session arg arg.object'.data
let write map session arg = wrap_writeop "write"
    write map session arg arg.file'.data
let commit map session arg = wrap_writeop "commit"
    commit map session arg arg.file'''.data

(* Wrap the create operations *************************************************)

let wrap_createop opname f map session arg dirfh name =
  try
    let dirfh = s2fh dirfh in
    let dirFake = L.fhToFake map dirfh in
    match L.lookupRead map dirfh with
    | None -> `nfs3err_stale { before = `False; after = `False }
    | Some (dirname, dirinfo) ->
      let wcc () =
        { before = pre_op_attr dirinfo;
          after = (match L.lookupRead map dirfh with
                   | Some (_, info) -> post_op_attr info dirFake
                   | None -> `False) }
      in
      if dirinfo.st_kind <> S_DIR then `nfs3err_notdir (wcc ())
      else
        begin
          match L.lookupCreate map dirfh name with
          | (Some path, old) ->
            begin
              try create_shadow_dirs map (user session) path
                      (FhName (dirfh, name));
                  let createdirinfo = lstat (Util.dirname path) in
                  if not (checkperm session createdirinfo access3_modify)
                    then raise Acces;
                  L.mountFileCheck map path;
                  f map session arg path old wcc
              with e -> handle_exp e (wcc ())
            end
          | _ -> `nfs3err_acces (wcc ())
        end
  with e ->
(*    prerr_endline ("exception "^(Printexc.to_string e)^" in "^opname^"\n");*)
    handle_exp e { before = `False; after = `False }

let create map session arg = wrap_createop "create"
    create map session arg arg.where.dir.data arg.where.name
let mkdir map session arg = wrap_createop "mkdir"
    mkdir map session arg arg.where'.dir.data arg.where'.name
let symlink map session arg = wrap_createop "symlink"
    symlink map session arg arg.where''.dir.data arg.where''.name
let mknod map session arg = wrap_createop "mknod"
    mknod map session arg arg.where'''.dir.data arg.where'''.name

(* Wrap the remove operations *************************************************)

let wrap_removeop opname f map session arg dirfh name =
  try
    let dirfh = s2fh dirfh in
    let dirFake = L.fhToFake map dirfh in
    match L.lookupRead map dirfh with
    | Some (dirname, dirinfo) ->
      let wcc () =
        { before = pre_op_attr dirinfo;
          after = (match L.lookupRead map dirfh with
                   | Some (_, info) -> post_op_attr info dirFake
                   | None -> `False) }
      in
      if dirinfo.st_kind <> S_DIR then `nfs3err_notdir (wcc ())
      else
        begin
          match L.lookupWrite map (FhName (dirfh, name)) with
          | (Some (readname, readinfo), Some writename) ->
            begin
              try
                L.mountFileCheck map writename;
                let res = f map session arg readname readinfo writename wcc in
                res
              with e -> handle_exp e (wcc ())
            end
          | (Some _, None) -> `nfs3err_acces (wcc ())
          | (None, _) -> `nfs3err_noent (wcc ())
        end
    | None -> `nfs3err_stale { before = `False; after = `False }
  with e -> 
(*    prerr_endline ("exception "^(Printexc.to_string e)^" in "^opname^"\n");*)
    handle_exp e { before = `False; after = `False }

let remove map session arg = wrap_removeop "remove"
    remove map session arg arg.dir.data arg.name
let rmdir map session arg = wrap_removeop "rmdir"
    rmdir map session arg arg.dir.data arg.name

(* Wrap the special case operations *******************************************)

(* no return data *)
let err0 () = `nfs3err_serverfault

(* post_op_attr = { `True of fattr3 = { many fields... } } *)
let err1 () = `nfs3err_serverfault `False

(* wcc_data = { before = pre_op_attr; after = post_op_attr } *)
let err2 () = `nfs3err_serverfault { before = `False; after = `False }

(* link3wcc = { file_attributes' = post_op_attr; linkdir_wcc = wcc_data } *)
let err3 () = `nfs3err_serverfault {
    file_attributes' = `False;
    linkdir_wcc = { before = `False; after = `False } }

(* rename3wcc = { fromdir_wcc = wcc_data; todir_wcc = wcc_data } *)
let err4 () = `nfs3err_serverfault {
    fromdir_wcc = { before = `False; after = `False };
    todir_wcc   = { before = `False; after = `False } }

let wrap name f map session arg res =
  try f map session arg
  with | Unix_error (e,s1,s2) ->
          prerr_endline ("exception Unix_error\n  ("^
              (error_message e)^", "^s1^", "^s2^")\n  in "^name^"\n");
          res
       | e ->
          prerr_endline ("exception "^(Printexc.to_string e)^" in "^ name^"\n");
          res

let getattr map session arg =
  try getattr map session arg
  with _ -> `nfs3err_stale

let rename map session arg = wrap "rename"
    rename map session arg (err4 ())

let link map session arg = wrap "link"
    link map session arg (err3 ())

