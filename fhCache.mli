(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

type t

val make : int -> t
val add : t -> Fh.t -> string -> string -> unit
val fhToFake : t -> Fh.t -> string
val fhToReal : t -> Fh.t -> string
val squash : t -> file -> unit
val mountChange : t -> unit
