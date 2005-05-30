(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Nfs3_prot_caux;;

val init : Lookup.t -> bool -> unit
external main_loop : unit -> unit = "main_loop"
