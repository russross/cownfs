(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Unix;;
open Unix.LargeFile;;
open Nfs3_prot_caux;;
open Common;;

type t = { tree: DirTree.t;
           fhcache: FhCache.t;
           readdircache: ReaddirCache.t }

let hide_name name = ".~" ^ name
let exists name = try ignore (lstat name); true with _ -> false
let keep old n = match old with None -> Some n | _ -> old
let max3 a b c = max (max a b) c

let make root fhcache_size readdircache_size =
  { tree = DirTree.make root;
    fhcache = FhCache.make fhcache_size;
    readdircache = ReaddirCache.make readdircache_size }

(* TODO: pay attention to credentials, or should we just ignore them here? *)
let dir_stat_combine (uid, gids, hostname) main next =
  let mtime = max main.st_mtime next.st_mtime in
  let ctime = max main.st_ctime next.st_ctime in
  let atime = max main.st_atime next.st_atime in
  let nlink = main.st_nlink + next.st_nlink - 2 in
  { main with st_mtime = mtime; st_ctime = ctime; st_atime = atime;
    st_nlink = nlink }

(******************************************************************************)

let fhToFake state fh =
  DirTree.fhToFake state.fhcache state.tree fh

let fhToRealList state fh =
  DirTree.fhToRealList state.fhcache state.tree fh

let fakeAndRealToNewFh state fake real =
  DirTree.fakeAndRealToNewFh state.fhcache fake real

let fakeToNewFh state fake =
  DirTree.fakeToNewFh state.fhcache fake

let fakeToRealList state fake =
  DirTree.fakeToRealList state.tree fake

let lookupChild state lst name =
  DirTree.lookupChild state.tree lst name

let squash state file =
  FhCache.squash state.fhcache file

let dirFhInfoToFileList state fh info counter cookie =
  ReaddirCache.dirFhToFileList state.readdircache fh info counter cookie

let squashDirFh state fh =
  ReaddirCache.remove state.readdircache fh

let addDirFhInfoFileList state fh info lst counter cookie =
  ReaddirCache.add state.readdircache fh info lst counter cookie

(* check if a file being changed is a .mount file, and clear the lookup
 * cache if appropriate *)
let mountFileCheck state real =
  if Util.filename real = mountFileName
  then
    begin
      let dir = Util.dirname real in
      FhCache.mountChange state.fhcache;
      DirTree.mountChange state.tree dir;
      let now = Unix.time () in
      Unix.utimes dir now now
    end

(* Lookup for read operations *************************************************)

let rec lookup_read_dir nameinfo paths =
  match paths with
  | [] -> nameinfo
  | (name, _) :: xs ->
    try
      let info = lstat name in
      match (info.st_kind, nameinfo) with
      | (S_DIR, Some (name', info')) ->
          (* TODO: use real credentials *)
          let combined = dir_stat_combine (0,[0],"") info' info in
          lookup_read_dir (Some (name', combined)) xs
      | (S_DIR, None) ->
          lookup_read_dir (Some (name, info)) xs
      | _ ->
          lookup_read_dir nameinfo xs
    with _ ->
      lookup_read_dir nameinfo xs

let rec lookup_read_cache_miss state paths stamp file =
  match paths with
  | [] -> None
  | (prefix, _) :: xs ->
      let name = Util.concatPath prefix file in
      let hidename = Util.concatPath prefix (hide_name file) in
      let continue () = xs <> [] && not (exists hidename) in
      try
        let info = lstat name in
        if info.st_kind = S_DIR
          then
            begin
              let (lst, stamp') = lookupChild state paths file in
              match lookup_read_dir None lst with
              | None -> None
              | Some (name, info) ->
                  Some (name, { info with
                                st_ctime = max3 info.st_ctime stamp stamp';
                                st_mtime = max3 info.st_mtime stamp stamp' })
            end
          else Some (name, info)
      with _ ->
        if continue ()
          then lookup_read_cache_miss state xs stamp file
          else None

let lookupRead state fh =
  let cachemiss () =
    try
      squash state (FhOnly fh);
      (* look up the filename and lstat it *)
      let fake = fhToFake state fh in
      let (lst, stamp) = fakeToRealList state (Util.dirname fake) in
      let res = lookup_read_cache_miss state lst stamp (Util.filename fake) in
      (* update the cache with the result *)
      begin
        match res with
        (* add a non-directory readable entry *)
        | Some (real, info) when info.st_kind <> S_DIR ->
            FhCache.add state.fhcache fh fake real
        (* it's already been squashed *)
        | _ -> ()
      end;
      res
    with Not_found -> None
  in
  try
    (* first check the cache *)
    let real = FhCache.fhToReal state.fhcache fh in
    (* cache hit, now check if the file is still there *)
    (* note: if there's a cache hit then it's not a directory, so a simple *)
    (*   lstat is sufficient *)
    let info = lstat real in
    if info.st_kind = S_DIR then failwith ("dir found in fh cache: "^real) else
    Some (real, info)
  (* cache miss or cached name has disappeared, fall back to regular lookup *)
  with _ -> cachemiss ()

(* Lookup for write operations ************************************************)

let rec lookup_write_cache_miss state paths stamp file copy =
  match paths with
  | [] -> (None, None)
  | (prefix, pri) :: xs ->
    begin
      let real = Util.concatPath prefix file in
      let hidename = Util.concatPath prefix (hide_name file) in
      let continue () = xs <> [] && not (exists hidename) in
      try
        (* see if there's a match at this real path *)
        let info = lstat real in
        let info' =
          (* in the case of directories we need to look at all stacked  *)
          (* directories with the same fake name to get the stats       *)
          if info.st_kind = S_DIR && continue ()
            then
              begin
                let (lst, stamp') = lookupChild state paths file in
                match lookup_read_dir None lst with
                | None -> failwith "bug: lookup_read_dir returned None"
                | Some (real', info') ->
                    { info' with
                      st_ctime = max3 info.st_ctime stamp stamp';
                      st_mtime = max3 info.st_mtime stamp stamp'}
              end
            else info
        in
        if pri = Read
          (* too late to find a writeable copy if we don't alreay have one *)
          then (Some (real, info'), copy)
          (* writeable--make changes directly with no copy-on-write *)
          else (Some (real, info'), Some real)
      with _ ->
        let copy' = if pri = Read then copy else (keep copy real) in
        if continue ()
          then lookup_write_cache_miss state xs stamp file copy'
          else (None, None)
    end

(* when we have a cached readable name, we just need to scan the list of
 * directories to see if there is also a writeable name.  we assume that
 * nothing has changed and this file hasn't been masked--no lstats required *)
let rec lookup_write_cache_hit paths file read =
  match paths with
  | [] -> (read, None)
  | (prefix, Read) :: xs ->
    begin
      let fullpath = Util.concatPath prefix file in
      (* if this matches the readonly name then there is no writeable name *)
      match read with
      | Some (real, info) ->
          if fullpath = real
            then (read, None)
            (* no match so keep looking *)
            else lookup_write_cache_hit xs file read
      | None -> failwith "bug: lookup_write_cache_hit called with None"
    end
  | (prefix, _) :: _ -> (read, Some (Util.concatPath prefix file))

(* check the cache for a hit and dispatch accordingly *)
let lookup_write_cache state fh paths stamp file =
  try
    let real = FhCache.fhToReal state.fhcache fh in
    let info = lstat real in
    (* a cache hit and it still exists, so check for a mutable path *)
    lookup_write_cache_hit paths file (Some (real, info))
  with _ ->
    (* either a cache miss or it's out-of-date *)
    lookup_write_cache_miss state paths stamp file None

(* find a pair of paths for an existing file.
 * the first name is readable, the second is the same if it is also
 * writeable, otherwise the second is the path where the copy-on-write
 * should be copied to.
 *)
let lookupWrite state file =
  let (fake, res) =
    begin
      match file with
      | FhOnly fh ->
          let fake = fhToFake state fh in
          let name = Util.filename fake in
          let (lst, stamp) = fakeToRealList state (Util.dirname fake) in
          let res = lookup_write_cache state fh lst stamp name in
          (fake, res)
      | FhName (fh, name) ->
          let fake = Util.concatPath (fhToFake state fh) name in
          let (lst, stamp) = fhToRealList state fh in
          let res = lookup_write_cache state fh lst stamp name in
          (fake, res)
    end
  in
  begin
    match res with
    | (Some (real, info), _) when info.st_kind <> S_DIR ->
        ignore (fakeAndRealToNewFh state fake real)
    | (Some _, _) -> ()
    | _ -> squash state file
  end;
  res

(* Lookup for create operations ***********************************************)

let rec lookup_create_cache_miss paths file create old =
  if create <> None && old <> None then (create, old) else
  match paths with
  | [] -> (create, old)
  | (prefix, pri) :: xs ->
    begin
      let real = Util.concatPath prefix file in
      let hidename = Util.concatPath prefix (hide_name file) in
      let create' = if pri = Read then create else keep create real in
      let continue () = xs <> [] && not (exists hidename) in
      let next old' =
        if (create' = None || old' = None) && continue ()
          then lookup_create_cache_miss xs file create' old'
          else (create', old')
      in
      if old <> None then next old
      else
        try let info = lstat real in
            let old' = Some (real, info) in
            next old'
        with _ -> next old
    end

let rec lookup_create_cache_hit paths file old =
  match paths with
  | [] -> (None, Some old)
  | (prefix, Read) :: xs ->
      let real = Util.concatPath prefix file in
      let (name, info) = old in
      if real = name
        then (None, Some old)
        else lookup_create_cache_hit xs file old
  | (prefix, _) :: _ -> (Some (Util.concatPath prefix file), Some old)

(* find the path for a file to be created: fh is for the enclosing directory *)
let lookupCreate state fh name =
  try
    let newFake = Util.concatPath (fhToFake state fh) name in
    let newFh = fakeToNewFh state newFake in
    let res =
      begin
        try
          let real = FhCache.fhToReal state.fhcache newFh in
          let info = lstat real in
          let (lst, _) = fhToRealList state fh in
          (* a cache hit and it still exists, so check for a create path *)
          lookup_create_cache_hit lst name (real, info)
        with _ ->
          (* either a cache miss or it's out-of-date *)
          let (lst, _) = fhToRealList state fh in
          lookup_create_cache_miss lst name None None
      end
    in
    (* now update the cache based on what was found, not what will be created *)
    begin
      match res with
      | (_, Some (real, info)) when info.st_kind <> S_DIR ->
          FhCache.add state.fhcache newFh newFake real
      | (_, Some (_, info)) when info.st_kind = S_DIR -> ()
      | _ ->
          squash state (FhOnly newFh)
    end;
    res
  (* if we get a Not_found here it means there was a fhToFake failure *)
  with Not_found -> (None, None)

(* Lookup for readdir operations **********************************************)

let lookupReaddir state fh =
  try
    List.map (fun (path, _) -> path) (fst (fhToRealList state fh))
  with _ -> []

(* Lookup for hide operations *************************************************)

(* returns (visible, hidepath option) where
 * visible <=> the file needs hiding and
 * hidepath gives the name of the file to create to hide the (visible) file.
 * Note that hidepath is always None when visible is false, but
 * visible = true and hidepath = None means it can't be hidden *)
   
let rec lookup_hide paths file create =
  match paths with
  | (prefix, pri) :: xs ->
    begin
      let hidden = Util.concatPath prefix (hide_name file) in
      try ignore (lstat (Util.concatPath prefix file));
          (true, create)
      with _ ->
        try ignore (lstat hidden);
            (false, None)
        with _ -> if pri = Read
                    then lookup_hide xs file create
                    else lookup_hide xs file (keep create hidden)
    end
  | [] -> (false, None)

let lookupHide state fh name =
  squash state (FhName (fh, name));
  lookup_hide (fst (fhToRealList state fh)) name None

