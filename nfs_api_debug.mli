(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Nfs3_prot_caux
open Common

val dump : unit -> unit
val print_error : bool ref
val print_noent : bool ref
val print_okay : bool ref
val dump_interval : float ref

val null :          Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_null'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_null'res
val getattr :       Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_getattr'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_getattr'res
val setattr :       Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_setattr'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_setattr'res
val lookup :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_lookup'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_lookup'res
val access :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_access'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_access'res
val readlink :      Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readlink'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readlink'res
val read :          Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_read'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_read'res
val write :         Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_write'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_write'res
val create :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_create'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_create'res
val mkdir :         Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_mkdir'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_mkdir'res
val symlink :       Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_symlink'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_symlink'res
val mknod :         Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_mknod'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_mknod'res
val remove :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_remove'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_remove'res
val rmdir :         Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_rmdir'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_rmdir'res
val rename :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_rename'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_rename'res
val link :          Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_link'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_link'res
val readdir :       Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readdir'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readdir'res
val readdirplus :   Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readdirplus'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_readdirplus'res
val fsstat :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_fsstat'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_fsstat'res
val fsinfo :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_fsinfo'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_fsinfo'res
val pathconf :      Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_pathconf'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_pathconf'res
val commit :        Lookup.t ->
                    session ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_commit'arg ->
                    t_NFS_PROGRAM'NFS_V3'nfsproc3_commit'res
