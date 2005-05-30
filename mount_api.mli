(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Mount_prot_caux
open Common

val null :              Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_null'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_null'res

val mnt :               Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_mnt'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_mnt'res

val dump :              Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_dump'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_dump'res
           
val umnt :              Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_umnt'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_umnt'res

val umntall :           Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_umntall'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_umntall'res

val export :            Lookup.t ->
                        session ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_export'arg ->
                        t_MOUNT_PROGRAM'MOUNT_V3'mountproc3_export'res
