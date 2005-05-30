(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

let cacheLifetime = 10.0

type dirHandle = { fh : Fh.t;
                   info : Unix.LargeFile.stats;
                   counter : int;
                   cookie : int;
                   fileList : (string * string) list;
                   stamp : float }

module DirStampSet = Set.Make
  (struct
    type t = dirHandle
    let compare a b =
    (* sort by timestamp, with fh as a tie-breaker *)
    let res = Pervasives.compare a.stamp b.stamp in
    if res = 0 then Pervasives.compare a.fh b.fh else res
  end)

module Hash = Hashtbl.Make
  (struct
    type t = string
    let equal = (=)
    let hash = Util.hashString 0
  end)

type t = {
  dirFhToHandleHash: dirHandle Hash.t;
  mutable stampQueue: DirStampSet.t
}

let make size =
  { dirFhToHandleHash = Hash.create size;
    stampQueue = DirStampSet.empty
  }

let remove t fh =
  try
    let handle = Hash.find t.dirFhToHandleHash fh in
    Hash.remove t.dirFhToHandleHash fh;
    t.stampQueue <- DirStampSet.remove handle t.stampQueue
  with Not_found -> ()

let removeHandle t handle = remove t handle.fh

let expire t =
  let now = Unix.time () in
  try
    let rec f () =
      let handle = DirStampSet.min_elt t.stampQueue in
      if now -. handle.stamp > cacheLifetime
        then (removeHandle t handle; f ())
        else ()
    in f ()
  with Not_found -> ()

let add t fh info counter cookie fileList =
  remove t fh;

  let handle = {
    fh = fh;
    info = info;
    counter = counter;
    cookie = cookie;
    fileList = fileList;
    stamp = Unix.time () }
  in

  (* dirFhToHandleHash *)
  Hash.add t.dirFhToHandleHash fh handle;

  (* stampQueue *)
  t.stampQueue <- DirStampSet.add handle t.stampQueue;

  expire t
  
;; open Unix.LargeFile;;
let dirFhToFileList t fh info counter cookie =
  expire t;

  let handle = Hash.find t.dirFhToHandleHash fh in
  if  handle.info.st_mtime <> info.st_mtime ||
      handle.info.st_ctime <> info.st_ctime ||
      handle.counter <> counter ||
      handle.cookie <> cookie
    then
    begin
      remove t fh;
(*
      print_string ("readdircache: stale data\n");
      if handle.info.st_ctime <> info.st_ctime then
        print_string ("  ctime mismatch\n");
      if handle.info.st_mtime <> info.st_mtime then
        print_string ("  mtime mismatch\n");
      if handle.info.st_atime <> info.st_atime then
        print_string ("  atime mismatch\n");
      if handle.counter <> counter then
        print_string ("  counter mismatch\n");
      if handle.cookie <> cookie then
        print_string ("  cookie mismatch\n");
      print_newline ();
*)
      raise Not_found
    end else
  add t handle.fh handle.info counter cookie handle.fileList;
  handle.fileList
