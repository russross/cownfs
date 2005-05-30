(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

(*
Gc.set { (Gc.get ()) with
    Gc.minor_heap_size = 524288;
    Gc.major_heap_increment = 491520 };;
*)

let basedir = ref "."
let fhcache_size = ref 8192
let readdircache_size = ref 16
let debug = ref false
;;

Arg.parse [
    ("-block_size", Arg.Set_int Nfs_api.block_size,
        "<size>: set the recommended block size (default "^
        (string_of_int !Nfs_api.block_size)^")");
    ("-fhcache", Arg.Set_int fhcache_size,
        "<entries>: set the size of the file handle cache (default "^
        (string_of_int !fhcache_size)^")");
    ("-rdcache", Arg.Set_int readdircache_size,
        "<entries>: set the size of the readdir cache (default "^
        (string_of_int !readdircache_size)^")");
    ("-debug", Arg.Set debug,
        ": enable debugging output");
    ("-debug_error",
        Arg.Bool (fun v -> Nfs_api_debug.print_error := v),
        "<true|false>: when debug is set, output calls with error results\n   "^
        "(default "^(string_of_bool !Nfs_api_debug.print_error)^")");
    ("-debug_noent",
        Arg.Bool (fun v -> Nfs_api_debug.print_noent := v),
        "<true|false>: when debug is set, output NFS3ERR_NOENT error\n   "^
        "results (default "^(string_of_bool !Nfs_api_debug.print_noent)^")");
    ("-debug_okay", Arg.Bool (fun v -> Nfs_api_debug.print_okay := v),
        "<true|false>: when debug is set, output calls with okay results\n   "^
        "(default "^(string_of_bool !Nfs_api_debug.print_okay)^")");
    ("-debug_interval",
        Arg.Set_float Nfs_api_debug.dump_interval,
        "<seconds>: when debug is set, dump timing data\n   "^
        "every <seconds> seconds (default "^
        (string_of_float !Nfs_api_debug.dump_interval)^")");
    ]
  (fun s -> basedir := s)
  "usage: cownfsd [options] <root dir> &";;

ignore (Unix.umask 0);;

if Unix.geteuid () <> 0 then prerr_endline ("warning: cownfsd not running "^
    "as root, expect things to break...");;

let state = Lookup.make !basedir !fhcache_size !readdircache_size;;
Csrv.init state !debug;;
Csrv.main_loop ();;
