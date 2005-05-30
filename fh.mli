(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

type t = string
val make : string list -> t
val testPrefix : t -> string -> bool
val test : t -> string -> bool
val ofString : string -> t
val toString : t -> string
val root : t
