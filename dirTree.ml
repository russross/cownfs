(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

(* TODO: honor .~ mask files when following recursive mounts *)
open Unix;;
open Unix.LargeFile;;
open Common;;
open Util;;

let mountRefreshTime = 10.0
let mountExpireTime = 300.0
let defaultMountCacheSize = 256
let maxlinkdepth = 5

let rec back lst =
  match lst with
    | x :: ((y :: ys) as xs) -> x :: back xs
      | _ -> []

type t = {
  root: string;
  (* mounts: (real -> (mount entry list, fileTimeStamp, lastCheckTime)) *)
  mounts: (string, (string * priority) list * float * float) Hashtbl.t;
  expire: (float * string) Queue.t
}

let expire t =
  let time = Unix.time () in
  let rec f () =
    if Queue.is_empty t.expire ||
       time -. fst (Queue.peek t.expire) < mountExpireTime
      then ()
      else
        begin
          let (oldtime, real) = Queue.pop t.expire in
          try
            let (_, _, refresh) = Hashtbl.find t.mounts real in
            if time -. refresh < mountExpireTime
              then Queue.add (time, real) t.expire
              else Hashtbl.remove t.mounts real;
            f ()
          with Not_found -> f ()
        end
  in f ()

(*
 * follow all symlinks and subpaths from base to ext and return the
 * full path name
 * TODO: this is the place to implement path restriction.  if it resolves to
 *       an illegal path, raise an exception
 *)
let resolvePathsImp followsym base ext =
  let rec f base ext depth =
    if depth < 0 then raise (Unix_error (ELOOP, "resolvePaths", ext)) else
    let i = ref 0 in
    while (!i < String.length ext && ext.[!i] =  '/') do i := !i + 1 done;
    let j = ref !i in
    while (!j < String.length ext && ext.[!j] <> '/') do j := !j + 1 done;
    let k = ref !j in
    while (!k < String.length ext && ext.[!k] =  '/') do k := !k + 1 done;
    if !i = !j then base else
    let root = if !i = 0 then base else "/" in
    let newbase = match (String.sub ext !i (!j - !i)) with
                  | "."  -> root
                  | ".." -> dirname root
                  | name -> concatPath root name in
    let newext = String.sub ext !k ((String.length ext) - !k) in
    let info = lstat newbase in
    match info.st_kind with
    | S_DIR -> f newbase newext maxlinkdepth
    | S_LNK when followsym ->
               f root (concatPath (readlink newbase) newext) (depth - 1)
    | _     -> raise (Unix_error (ENOTDIR, "resolvePaths", ext))
  in f base ext maxlinkdepth

let resolvePaths = resolvePathsImp true

let resolvePathsNoSymlinks = resolvePathsImp false 

let make root =
  { root = resolvePaths (resolvePaths "/" (Sys.getcwd ())) root;
    mounts = Hashtbl.create defaultMountCacheSize;
    expire = Queue.create () }

(*****************************************************************************)

(*
 * read a .mount file in the given directory and return a list
 * of mount directives in reverse order
 *)
let tokenList = [ "read"; "write"; "link" ]
let readMountFile real =
  let rec read fp a =
    try
      let tokens =
          Genlex.make_lexer tokenList (Stream.of_string (input_line fp))
      in
      match Stream.npeek 3 tokens with
      | [Genlex.Kwd "read"; Genlex.String path] -> read fp ((path, Read) :: a)
      | [Genlex.Kwd "write"; Genlex.String path] -> read fp ((path, Write) :: a)
      | [Genlex.Kwd "link"; Genlex.String path] -> (path, Link) :: a
      | _ -> read fp a
    with Stream.Failure -> read fp a
       | End_of_file -> a
  in
  try
    let file = open_in (concatPath real mountFileName) in
    let res = read file [] in
    close_in file;
    res
  with Sys_error _ -> []

(* check the timestamp on the .mount file and reread it if necessary *)
let checkMountFile real old =
  let exists = try Some (lstat real) with _ -> None in
  match (old, exists) with
  | (_, None) -> ([], 0.0)
  | (None, Some info) -> (readMountFile real, info.st_mtime)
  | (Some (_, stamp), Some info) when stamp < info.st_mtime ->
      (readMountFile real, info.st_mtime)
  | (Some (_ as res), _) -> res

(* get the mount file from the cache (possibly reread it and update cache ) *)
let getMount t real =
  let time = Unix.time () in
  let refresh old =
    let (lst, stamp) = checkMountFile real old in
    if Hashtbl.mem t.mounts real then () else Queue.add (time, real) t.expire;
    Hashtbl.replace t.mounts real (lst, stamp, time);
    (lst, stamp)
  in
  try
    let (lst, stamp, exp) = Hashtbl.find t.mounts real in
    (* have we checked the mount file recently? *)
    if time -. exp > mountRefreshTime
      then refresh (Some (lst, stamp))
      else (lst, stamp)
  with Not_found -> refresh None

(*
 * given a real path name and its priority, read a mount file (if any)
 * and expand it into a list of paths and priorities.  note that this
 * expansion is recursive and we guard against loops
 *)
let expandMount t real pri =
  let min_pri a b =
    match (a, b) with
    | (Read, _)  | (_, Read)  -> Read
    | _                       -> Write
  in
  let seen = ref [] in
  let stamp = ref 0.0 in
  let rec expand path' pri' =
    let (lst, stamp') = getMount t path' in
    stamp := max !stamp stamp';
    let link = match lst with (_, Link) :: _ -> true | _ -> false in
    let mapped = List.fold_right
          (fun (path'', pri'') a ->
            try (resolvePaths path' path'', min_pri pri' pri'') :: a
            with _ -> a)
          lst []
    in
    let exploded = List.flatten
      (List.map
        (function (path'', pri'') ->
          if List.mem path'' !seen
          then []
          else (seen := path'' :: !seen; expand path'' pri''))
        mapped)
    in
    if link then exploded else (path', pri') :: exploded
  in
  let lst = expand real pri in
  (lst, !stamp)

(*
 * get the child (path, priority) list from a parent direntry by following a
 * single subdirectory, including .mount expansion, etc.
 *
 * Note: we must look up the full path each time to prevent security leaks.
 * otherwise, you could swap in a symlink where a real directory used to be
 * and get access to any path on the host instead of just the exported paths.
 *)
let lookupChild t parent name =
  let hasWrite lst =
    List.exists (function (_, Write) -> true | _ -> false) lst
  in
  let stamp = ref 0.0 in
  let rec cd lst first =
    match lst with
    | [] -> []
    | (prefix, pri) :: xs ->
      try
        let full = resolvePathsNoSymlinks prefix name in
        let info = lstat full in
        match (info.st_kind, pri) with
        | (S_DIR, _) ->
            let (lst, stamp') = expandMount t full pri in
            stamp := max !stamp stamp'; 
            (* when we've found a writable name we don't need to keep any *)
            (* more names that we know don't exist. *)
            (* Note--the newly expanded list may have names that don't exist *)
            (* but we haven't wasted an lstat on them yet so we don't know *)
            let first' = first && not (hasWrite lst) in
            lst :: (cd xs first')
        (* what to do?  a file (or symlink) with the same name as a directory *)
        | _ -> cd xs first
      with Unix_error _ ->
        (* this directory doesn't exist, but keep it in the list if it's the *)
        (* first writable name we've found *)
        match (first, pri) with
        | (true, Write) -> [(concatPath prefix name, pri)] :: (cd xs false)
        | (true, _) -> cd xs true
        | (false, _) -> cd xs false
  in
  let paths = List.flatten (cd parent true) in
  if paths = [] then raise Not_found else (paths, !stamp)

let rootDirs t = expandMount t t.root Write

(*
 * lookup a direntry by starting from a base entry and following a list
 * of paths.  this does .mount expansion, etc., and modifies the cache
 * as necessary.
 *)
let lookupList t lst =
  expire t;
  List.fold_left
    (fun (res, stamp) name ->
      let (res', stamp') = lookupChild t res name in
      (res', max stamp stamp'))
    (rootDirs t)
    lst

(*
 * return a list of subdir names (could be symlinks) s.t. (filter name) = true
 *)
(*
let filterFiles filter parent =
  try
    let dirhandle = opendir parent in
    try
      let rec f a =
        try
          let file = readdir dirhandle in
          if file = "." || file = ".." then f a
          else if filter file then f (file :: a) else f a
        with Unix_error _ -> f a
           | End_of_file  -> (closedir dirhandle; a)
      in f []
    with e -> (closedir dirhandle; raise e)
  with Unix_error _ -> []
*)

let filterFiles filter parent =
  try
    let lst = Sys.readdir parent in
    Array.fold_right (fun x xs -> if filter x then x :: xs else xs) lst []
  (* the directory might not exist--it could be a shadow dir *)
  with Unix_error _ -> []
     | Sys_error _ -> []

exception Success of string list

let lookup_fake_from_fh t fh =
  if fh = Fh.root then "/" else
  let rec search prefix paths lst =
    let get_subdirs path =
      let is_subdir name =
        let full = concatPath prefix name in
        let real = concatPath path name in
        let lst' = lst @ [name] in
        if Fh.test fh full && Fh.make lst' = fh then raise (Success lst')
        else try Fh.testPrefix fh full && (stat real).st_kind = S_DIR
             with _ -> false
      in
      filterFiles is_subdir path
    in
    let try_subdir name = search (concatPath prefix name)
                                 (fst (lookupChild t paths name))
                                 (lst @ [name])
    in
    let try_dir (path, pri) =
      List.iter try_subdir (get_subdirs path)
    in
    List.iter try_dir paths
  in
  try
    search "/" (fst (rootDirs t)) [];
    raise Not_found
  with Success res -> join res

(*
let lookup_fake_from_fh t fh =
  try lookup_fake_from_fh t fh
  with e ->
      prerr_endline ("lookup_fake_from_fh: failed");
      raise e
*)

(* public API ****************************************************************)

let fhToFake fhcache t fh =
  try FhCache.fhToFake fhcache fh
  with Not_found ->
      let fake = lookup_fake_from_fh t fh in
      FhCache.add fhcache fh fake "";
      fake

let fakeToRealList t fake =
  lookupList t (split fake)

let fhToRealList fhcache t fh =
  fakeToRealList t (fhToFake fhcache t fh)

let fakeAndRealToNewFh fhcache fake real =
  let fh = Fh.make (split fake) in
  FhCache.add fhcache fh fake real;
  fh

let fakeToNewFh fhcache fake =
  fakeAndRealToNewFh fhcache fake ""

let mountChange t real =
  if Hashtbl.mem t.mounts real
    then Hashtbl.replace t.mounts real ([], 0.0, 0.0)
    else ()
