(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

exception Invalid_path of string

val hashString : int -> string -> int
val hashString64 : int64 -> string -> int64

val assertPath : string -> unit
val split : string -> string list
val join : string list -> string
val startsWith : string -> string -> bool
val endsWith : string -> string -> bool
val dirname : string -> string
val filename : string -> string
val concatPath : string -> string -> string
val validateName : string -> bool
external lchown : string -> int -> int -> unit = "linux_lchown"
external create : string -> int -> int -> int -> bool -> int = "linux_create"
external mknod : string -> int -> int64 -> int -> int -> unit = "linux_mknod"
external hash64 : string -> Rtypes.uint8 = "hash64"
val readdir_set : int * int * int * int -> unit
val readdir_base : unit -> int
val readdir_entry_size : string -> int
val readdirplus_base : unit -> int
val readdirplus_entry_size : string -> int

