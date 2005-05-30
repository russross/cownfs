(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

type priority = Read | Write | Link

type file = FhOnly of Fh.t | FhName of Fh.t * string

val mountFileName : string

type session = int * int list * string
