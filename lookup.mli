(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

type t

val make : string -> int -> int -> t

(*****************************************************************************)
val fhToFake : t -> Fh.t -> string
val fhToRealList : t -> Fh.t -> (string * priority) list * float
val fakeToRealList : t -> string -> (string * priority) list * float
val fakeToNewFh : t -> string -> Fh.t
val fakeAndRealToNewFh : t -> string -> string -> Fh.t

val squash : t -> file -> unit
val mountFileCheck : t -> string -> unit
val dirFhInfoToFileList : t -> Fh.t -> Unix.LargeFile.stats -> int -> int ->
    (string * string) list
val squashDirFh : t -> Fh.t -> unit
val addDirFhInfoFileList :
    t -> Fh.t -> Unix.LargeFile.stats -> int -> int -> (string * string) list ->
    unit

(*****************************************************************************)

(* returns the file name and stats for a readable target.  Returns None if
 * the target isn't readable *)
val lookupRead : t -> Fh.t -> (string * Unix.LargeFile.stats) option

(* returns a, b where a is the existing file and b is the target for
 * copy-on-write.  If a and b have the same filename then the file is
 * writeable as-is.  If b is None then there is no writeable target *)
val lookupWrite : t -> file ->
    (string * Unix.LargeFile.stats) option * string option

(* returns a, b where a is the filename to be created and b is an existing
 * file (if any) with the same logical name.  It is possible that a and b
 * have the same name. *)
val lookupCreate : t -> Fh.t -> string ->
    string option * (string * Unix.LargeFile.stats) option

(* returns a list of directory names.  directories in the list may not exist *)
val lookupReaddir : t -> Fh.t -> string list

(* returns (visible, hidepath option) where
 * visible <=> the file needs hiding and
 * hidepath gives the name of the file to create to hide the (visible) file.
 * Note that hidepath is always None when visible is false, but
 * visible = true and hidepath = None means it can't be hidden *)
val lookupHide : t -> Fh.t -> string -> bool * string option
