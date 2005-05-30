(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

type t

val make : int -> t

val remove : t -> Fh.t -> unit
val add : t -> Fh.t -> Unix.LargeFile.stats -> int -> int ->
    (string * string) list -> unit
val dirFhToFileList : t -> Fh.t -> Unix.LargeFile.stats -> int -> int ->
    (string * string) list
