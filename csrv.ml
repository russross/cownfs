(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Mount_prot_caux;;
open Nfs3_prot_caux;;
module Nfs = Nfs_api;;
module DNfs = Nfs_api_debug;;
module Mount = Mount_api;;

external mount_prot_init :
    mountres3_ok *
    mountbody *
    groupnode *
    exportnode
  -> unit = "mount_prot_init"

external nfs3_prot_init :
    specdata3 *
    nfs_fh3 *
    nfstime3 *
    fattr3 * 
    wcc_attr *
    wcc_data *
    sattr3 *
    diropargs3 *
    diropres3ok *
    setattr3args *
    lookup3resok *
    access3args *
    access3resok *
    readlink3resok *
    read3args *
    read3resok *
    write3args *
    write3resok *
    create3args *
    mkdir3args *
    symlinkdata3 *
    symlink3args *
    devicedata3 *
    mknod3args *
    rename3args *
    rename3wcc *
    link3args *
    link3wcc *
    readdir3args *
    entry3 *
    dirlist3 *
    readdir3resok *
    readdirplus3args *
    entryplus3 *
    dirlistplus3 *
    readdirplus3resok *
    fsstat3resok *
    fsinfo3resok *
    pathconf3resok *
    commit3args *
    commit3resok
  -> unit = "nfs3_prot_init"

external main_loop : unit -> unit = "main_loop"

external readdir3resok_length : readdir3resok
  -> int = "readdir3resok_length"
external readdirplus3resok_length : readdirplus3resok
  -> int = "readdirplus3resok_length"

let init_readdir_length () =
  let int4 = Rtypes.int4_of_int 0 in
  let uint32 = Rtypes.uint4_of_int 0 in
  let uint64 = Rtypes.uint8_of_int 0 in
  let cookieverf3 = "12345678" in
  let ftype3 = int4 in
  let specdata3 = 
        { 
          major = uint32;
          minor = uint32;
        } in
  let nfstime3 = 
        { 
          seconds = uint32;
          nseconds = uint32;
        } in
  let fattr3 = 
        { 
          type' = ftype3;
          mode = uint32;
          nlink = uint32;
          uid = uint32;
          gid = uint32;
          size = uint64;
          used = uint64;
          rdev = specdata3;
          fsid = uint64;
          fileid = uint64;
          atime = nfstime3;
          mtime = nfstime3;
          ctime = nfstime3;
        } in
  let post_op_attr = `True fattr3 in
  let entry = 
        { 
          fileid' = uint64;
          name' = "";
          cookie' = uint64;
          nextentry = None;
        } in
  let dirlist3None = 
        { 
          entries = None;
          eof' = true;
        } in
  let dirlist3One = 
        { 
          entries = Some entry;
          eof' = false;
        } in
  let readdir3resokNone = 
        { 
          dir_attributes' = post_op_attr;
          cookieverf' = cookieverf3;
          reply = dirlist3None;
        } in
  let readdir3resokOne = 
        { 
          dir_attributes' = post_op_attr;
          cookieverf' = cookieverf3;
          reply = dirlist3One;
        } in
  let nfs_fh3 =
        {
          data = Fh.root;
        } in
  let post_op_fh3 = `True nfs_fh3 in
  let entryplus = 
        { 
          fileid'' = uint64;
          name'' = "";
          cookie''' = uint64;
          name_attributes = post_op_attr;
          name_handle = post_op_fh3;
          nextentry' = None;
        } in
  let dirlistplusNone = 
        { 
          entries' = None;
          eof'' = true;
        } in
  let dirlistplusOne = 
        { 
          entries' = Some entryplus;
          eof'' = false;
        } in
  let readdirplus3resokNone = 
        { 
          dir_attributes'' = post_op_attr;
          cookieverf''' = cookieverf3;
          reply' = dirlistplusNone;
        } in
  let readdirplus3resokOne = 
        { 
          dir_attributes'' = post_op_attr;
          cookieverf''' = cookieverf3;
          reply' = dirlistplusOne;
        } in

  let lengthNone = readdir3resok_length readdir3resokNone in
  let lengthOne  = readdir3resok_length readdir3resokOne in

  let plusNone = readdirplus3resok_length readdirplus3resokNone in
  let plusOne  = readdirplus3resok_length readdirplus3resokOne in

  Util.readdir_set (lengthNone, lengthOne, plusNone, plusOne)

let init state debug =
  (* first register callback functions *)
  let _ = Callback.register "mountproc3_null"      (Mount.null state) in
  let _ = Callback.register "mountproc3_mnt"       (Mount.mnt state) in
  let _ = Callback.register "mountproc3_dump"      (Mount.dump state) in
  let _ = Callback.register "mountproc3_umnt"      (Mount.umnt state) in
  let _ = Callback.register "mountproc3_umntall"   (Mount.umntall state) in
  let _ = Callback.register "mountproc3_export"    (Mount.export state) in

  if debug then (
    let _ = Callback.register "nfsproc3_null"        (DNfs.null state) in
    let _ = Callback.register "nfsproc3_getattr"     (DNfs.getattr state) in
    let _ = Callback.register "nfsproc3_setattr"     (DNfs.setattr state) in
    let _ = Callback.register "nfsproc3_lookup"      (DNfs.lookup state) in
    let _ = Callback.register "nfsproc3_access"      (DNfs.access state) in
    let _ = Callback.register "nfsproc3_readlink"    (DNfs.readlink state) in
    let _ = Callback.register "nfsproc3_read"        (DNfs.read state) in
    let _ = Callback.register "nfsproc3_write"       (DNfs.write state) in
    let _ = Callback.register "nfsproc3_create"      (DNfs.create state) in
    let _ = Callback.register "nfsproc3_mkdir"       (DNfs.mkdir state) in
    let _ = Callback.register "nfsproc3_symlink"     (DNfs.symlink state) in
    let _ = Callback.register "nfsproc3_mknod"       (DNfs.mknod state) in
    let _ = Callback.register "nfsproc3_remove"      (DNfs.remove state) in
    let _ = Callback.register "nfsproc3_rmdir"       (DNfs.rmdir state) in
    let _ = Callback.register "nfsproc3_rename"      (DNfs.rename state) in
    let _ = Callback.register "nfsproc3_link"        (DNfs.link state) in
    let _ = Callback.register "nfsproc3_readdir"     (DNfs.readdir state) in
    let _ = Callback.register "nfsproc3_readdirplus" (DNfs.readdirplus state) in
    let _ = Callback.register "nfsproc3_fsstat"      (DNfs.fsstat state) in
    let _ = Callback.register "nfsproc3_fsinfo"      (DNfs.fsinfo state) in
    let _ = Callback.register "nfsproc3_pathconf"    (DNfs.pathconf state) in
    let _ = Callback.register "nfsproc3_commit"      (DNfs.commit state) in
    ()
  ) else (
    let _ = Callback.register "nfsproc3_null"        (Nfs.null state) in
    let _ = Callback.register "nfsproc3_getattr"     (Nfs.getattr state) in
    let _ = Callback.register "nfsproc3_setattr"     (Nfs.setattr state) in
    let _ = Callback.register "nfsproc3_lookup"      (Nfs.lookup state) in
    let _ = Callback.register "nfsproc3_access"      (Nfs.access state) in
    let _ = Callback.register "nfsproc3_readlink"    (Nfs.readlink state) in
    let _ = Callback.register "nfsproc3_read"        (Nfs.read state) in
    let _ = Callback.register "nfsproc3_write"       (Nfs.write state) in
    let _ = Callback.register "nfsproc3_create"      (Nfs.create state) in
    let _ = Callback.register "nfsproc3_mkdir"       (Nfs.mkdir state) in
    let _ = Callback.register "nfsproc3_symlink"     (Nfs.symlink state) in
    let _ = Callback.register "nfsproc3_mknod"       (Nfs.mknod state) in
    let _ = Callback.register "nfsproc3_remove"      (Nfs.remove state) in
    let _ = Callback.register "nfsproc3_rmdir"       (Nfs.rmdir state) in
    let _ = Callback.register "nfsproc3_rename"      (Nfs.rename state) in
    let _ = Callback.register "nfsproc3_link"        (Nfs.link state) in
    let _ = Callback.register "nfsproc3_readdir"     (Nfs.readdir state) in
    let _ = Callback.register "nfsproc3_readdirplus" (Nfs.readdirplus state) in
    let _ = Callback.register "nfsproc3_fsstat"      (Nfs.fsstat state) in
    let _ = Callback.register "nfsproc3_fsinfo"      (Nfs.fsinfo state) in
    let _ = Callback.register "nfsproc3_pathconf"    (Nfs.pathconf state) in
    let _ = Callback.register "nfsproc3_commit"      (Nfs.commit state) in
    ()
  );

  (*
   * now create an object of every struct type that needs to be created
   * by the C code.  We pass a list of objects to the C code and it grabs
   * the type annotations from them.  Ugly, but I couldn't see a better
   * way to do it.
   *)
  let str = "" in
  let boolean = false in
  let int4 = Rtypes.int4_of_int 0 in
  let uint64 = Rtypes.uint8_of_int 0 in
(*  let int64 = Rtypes.int8_of_int 0 in *)
  let uint32 = Rtypes.uint4_of_int 0 in
(*  let int32 = Rtypes.int4_of_int 0 in *)
  let filename3 = str in
  let nfspath3 = str in
  let cookieverf3 = str in
(*  let createverf3 = str in *)
  let writeverf3 = str in
(*  let nfsstat3 = int4 in *)
  let ftype3 = int4 in
  let specdata3 = 
        { 
          major = uint32;
          minor = uint32;
        } in
  let nfs_fh3 = 
        { 
          data = str;
        } in
  let nfstime3 = 
        { 
          seconds = uint32;
          nseconds = uint32;
        } in
  let fattr3 = 
        { 
          type' = ftype3;
          mode = uint32;
          nlink = uint32;
          uid = uint32;
          gid = uint32;
          size = uint64;
          used = uint64;
          rdev = specdata3;
          fsid = uint64;
          fileid = uint64;
          atime = nfstime3;
          mtime = nfstime3;
          ctime = nfstime3;
        } in
  let post_op_attr = `False in
  let wcc_attr = 
        { 
          size' = uint64;
          mtime' = nfstime3;
          ctime' = nfstime3;
        } in
  let pre_op_attr = `False in
  let wcc_data = 
        { 
          before = pre_op_attr;
          after = post_op_attr;
        } in
  let post_op_fh3 = `False in
  let set_uint32 = `False in
  let set_uint64 = `False in
(*  let time_how = int4 in *)
  let set_time = `dont_change in
  let sattr3 = 
        { 
          mode' = set_uint32;
          uid' = set_uint32;
          gid' = set_uint32;
          size'' = set_uint64;
          atime' = set_time;
          mtime'' = set_time;
        } in
  let diropargs3 = 
        { 
          dir = nfs_fh3;
          name = filename3;
        } in
  let diropres3ok = 
        { 
          obj = post_op_fh3;
          obj_attributes = post_op_attr;
          dir_wcc = wcc_data;
        } in
(*  let diropres3 = `nfs3_ok diropres3ok in *)
(*  let wccstat3 = `nfs3_neg_one in *)
(*  let getattr3res = `nfs3_neg_one  in *)
  let sattrguard3 = `False in
  let setattr3args = 
        { 
          object' = nfs_fh3;
          new_attributes = sattr3;
          guard = sattrguard3;
        } in
  let lookup3resok = 
        { 
          object'' = nfs_fh3;
          obj_attributes' = post_op_attr;
          dir_attributes = post_op_attr;
        } in
(*  let lookup3res = `nfs3_ok lookup3resok in *)
  let access3args = 
        { 
          object''' = nfs_fh3;
          access = uint32;
        } in
  let access3resok = 
        { 
          obj_attributes'' = post_op_attr;
          access' = uint32;
        } in
(*  let access3res = `nfs3_ok access3resok in *)
  let readlink3resok = 
        { 
          symlink_attributes = post_op_attr;
          data' = nfspath3;
        } in
(*  let readlink3res = `nfs3_ok readlink3resok in *)
  let read3args = 
        { 
          file = nfs_fh3;
          offset = uint64;
          count = uint32;
        } in
  let read3resok = 
        { 
          file_attributes = post_op_attr;
          count' = uint32;
          eof = boolean;
          data'' = str;
        } in
(*  let read3res = `nfs3_ok read3resok in *)
  let stable_how = 
        int4 in
  let write3args = 
        { 
          file' = nfs_fh3;
          offset' = uint64;
          count'' = uint32;
          stable = stable_how;
          data''' = str;
        } in
  let write3resok = 
        { 
          file_wcc = wcc_data;
          count''' = uint32;
          committed = stable_how;
          verf = writeverf3;
        } in
(*  let write3res = `nfs3_ok write3resok in *)
(*  let createmode3 = int4 in *)
  let createhow3 = `unchecked sattr3 in
  let create3args = 
        { 
          where = diropargs3;
          how = createhow3;
        } in
  let mkdir3args = 
        { 
          where' = diropargs3;
          attributes = sattr3;
        } in
  let symlinkdata3 = 
        { 
          symlink_attributes' = sattr3;
          symlink_data = nfspath3;
        } in
  let symlink3args = 
        { 
          where'' = diropargs3;
          symlink = symlinkdata3;
        } in
  let devicedata3 = 
        { 
          dev_attributes = sattr3;
          spec = specdata3;
        } in
  let mknoddata3 = `nf3reg  in
  let mknod3args = 
        { 
          where''' = diropargs3;
          what = mknoddata3;
        } in
  let rename3args = 
        { 
          from = diropargs3;
          to' = diropargs3;
        } in
  let rename3wcc = 
        { 
          fromdir_wcc = wcc_data;
          todir_wcc = wcc_data;
        } in
(*  let rename3res = `nfs3_neg_one in *)
  let link3args = 
        { 
          file'' = nfs_fh3;
          link = diropargs3;
        } in
  let link3wcc = 
        { 
          file_attributes' = post_op_attr;
          linkdir_wcc = wcc_data;
        } in
(*  let link3res = `nfs3_neg_one  in *)
  let readdir3args = 
        { 
          dir' = nfs_fh3;
          cookie = uint64;
          cookieverf = cookieverf3;
          count'''' = uint32;
        } in
  let entry3 = 
        { 
          fileid' = uint64;
          name' = filename3;
          cookie' = uint64;
          nextentry = None;
        } in
  let dirlist3 = 
        { 
          entries = None;
          eof' = boolean;
        } in
  let readdir3resok = 
        { 
          dir_attributes' = post_op_attr;
          cookieverf' = cookieverf3;
          reply = dirlist3;
        } in
(*  let readdir3res = `nfs3_ok readdir3resok in *)
  let readdirplus3args = 
        { 
          dir'' = nfs_fh3;
          cookie'' = uint64;
          cookieverf'' = cookieverf3;
          dircount = uint32;
          maxcount = uint32;
        } in
  let entryplus3 = 
        { 
          fileid'' = uint64;
          name'' = filename3;
          cookie''' = uint64;
          name_attributes = post_op_attr;
          name_handle = post_op_fh3;
          nextentry' = None;
        } in
  let dirlistplus3 = 
        { 
          entries' = None;
          eof'' = boolean;
        } in
  let readdirplus3resok = 
        { 
          dir_attributes'' = post_op_attr;
          cookieverf''' = cookieverf3;
          reply' = dirlistplus3;
        } in
(*  let readdirplus3res = `nfs3_ok readdirplus3resok in *)
  let fsstat3resok = 
        { 
          obj_attributes''' = post_op_attr;
          tbytes = uint64;
          fbytes = uint64;
          abytes = uint64;
          tfiles = uint64;
          ffiles = uint64;
          afiles = uint64;
          invarsec = uint32;
        } in
(*  let fsstat3res = `nfs3_ok fsstat3resok in *)
  let fsinfo3resok = 
        { 
          obj_attributes'''' = post_op_attr;
          rtmax = uint32;
          rtpref = uint32;
          rtmult = uint32;
          wtmax = uint32;
          wtpref = uint32;
          wtmult = uint32;
          dtpref = uint32;
          maxfilesize = uint64;
          time_delta = nfstime3;
          properties = uint32;
        } in
(*  let fsinfo3res = `nfs3_ok fsinfo3resok in *)
  let pathconf3resok = 
        { 
          obj_attributes''''' = post_op_attr;
          linkmax = uint32;
          name_max = uint32;
          no_trunc = boolean;
          chown_restricted = boolean;
          case_insensitive = boolean;
          case_preserving = boolean;
        } in
(*  let pathconf3res = `nfs3_ok pathconf3resok in *)
  let commit3args = 
        { 
          file''' = nfs_fh3;
          offset'' = uint64;
          count''''' = uint32;
        } in
  let commit3resok = 
        { 
          file_wcc' = wcc_data;
          verf' = writeverf3;
        } in
(*  let commit3res = `nfs3_ok commit3resok in *)

  (* mount_prot types *)
(*  let fhandle2 = str in *)
  let fhandle3 = str in
  let dirpath = str in
  let name = str in
(*  let fhstatus = `_0 fhandle2 in *)
(*  let mountstat3 = int4 in *)
  let mountres3_ok = 
        { 
          fhandle = fhandle3;
          auth_flavors = [||];
        } in
(*  let mountres3 = `mnt3err_perm in *)
  let mountlist = None in
  let mountbody = 
        { 
          ml_hostname = name;
          ml_directory = dirpath;
          ml_next = mountlist;
        } in
  let groups = None in
  let groupnode = 
        { 
          gr_name = name;
          gr_next = groups;
        } in
  let exports = None in
  let exportnode = 
        { 
          ex_dir = dirpath;
          ex_groups = groups;
          ex_next = exports;
        }
  in

  mount_prot_init (
      mountres3_ok,
      mountbody,
      groupnode,
      exportnode
  );

  nfs3_prot_init (
      specdata3,
      nfs_fh3,
      nfstime3,
      fattr3, 
      wcc_attr,
      wcc_data,
      sattr3,
      diropargs3,
      diropres3ok,
      setattr3args,
      lookup3resok,
      access3args,
      access3resok,
      readlink3resok,
      read3args,
      read3resok,
      write3args,
      write3resok,
      create3args,
      mkdir3args,
      symlinkdata3,
      symlink3args,
      devicedata3,
      mknod3args,
      rename3args,
      rename3wcc,
      link3args,
      link3wcc,
      readdir3args,
      entry3,
      dirlist3,
      readdir3resok,
      readdirplus3args,
      entryplus3,
      dirlistplus3,
      readdirplus3resok,
      fsstat3resok,
      fsinfo3resok,
      pathconf3resok,
      commit3args,
      commit3resok
  );

  init_readdir_length ()
