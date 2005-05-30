(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

type t

val make : string -> t

val lookupChild : t -> (string * priority) list -> string ->
    (string * priority) list * float
val fhToFake : FhCache.t -> t -> Fh.t -> string
val fhToRealList : FhCache.t -> t -> Fh.t -> (string * priority) list * float
val fakeToRealList : t -> string -> (string * priority) list * float
val fakeToNewFh : FhCache.t -> string -> Fh.t
val fakeAndRealToNewFh : FhCache.t -> string -> string -> Fh.t
val mountChange : t -> string -> unit
