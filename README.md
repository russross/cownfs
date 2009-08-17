Copy-on-write NFS server
========================

This is a copy-on-write NFS server designed to allow dynamic
stacking of filesystem hierarchies.  In a typical use, a user will
mount a read-only base image (such as a standard Linux distribution
installation) and a private, writable overlay.  Initially, the
stacked filesystem will appear to be the same as the base image, but
any changes made are captured in the private overlay, allowing the
user to get the benefits of a private filesystem without having to
start from scratch.

This was designed for use with the XenoServers public computing
platform, particularly to allow multiple virtual machines using the
Xen hypervisor to run independently while sharing a common base
filesystem.  This makes it easy and fast to launch a new virtual
machine from an existing filesystem without worrying about write
conflicts.

The source code is written in Objective Caml (OCaml), which can be
obtained from:

        http://caml.inria.fr/

or it is available with most Linux distributions.  To build it, use
make.  First change `OCAMLINCLUDE` in the `Makefile` to point to your
OCaml library directory.  To build a native executable (the default)
use:

        make opt

To build a bytecode executable use:

        make bc

In each case, the executable `cownfsd` is built, which can then be
copied to any directory (there are no other dependencies in the
build tree).


This daemon uses the standard C `rpcgen` and its accompanying
libraries and generates code to interface between the RPC code and
the OCaml code.  This generator is derived from the `ocamlrpcgen`
library written by Gerd Stolpmann.  Unfortunately, `ocamlrpcgen` was a
huge CPU hog that destroyed peformance and crashed regularly, so I
don't use the library itself, but I did extend the code generator to
produce my interface code.  The `ocamlrpcgen` library is also
distributed under the GPL and can be found here:

        http://www.ocaml-programming.de/packages/documentation/rpc/

This code is Copyright 2004, 2005 by Russ Ross and the latest
release is available at the site referenced above.  It is
distributed under the GPL; full details are available in the file
COPYING.

I can be contacted by the email address: russ at russross dot com.

Note: this software was written for research purposes and has not
been designed with rigorous security in mind.  It should only be
used in trusted environments.


HOWTO
=====

I wrote a copy-on-write NFS daemon for use with the [XenoServers](http://www.cl.cam.ac.uk/Research/SRG/netos/xeno/) project. It's a general purpose userspace NFS daemon with the ability to dynamically stack file hierarchies over each other. Different layers can be configured as writable or as read-only, and in the latter case changes are made to a writable overlay in a copy-on-write fashion.

Some more information is available in a [paper from WORLDS'04](http://www.cl.cam.ac.uk/Research/SRG/netos/papers/2004-deploy-worlds.pdf) (PDF format).


Example
-------

A simple example illustrates the basic idea.

* Install a pristine Linux kernel source tree in `/usr/src/linux`
* Create an empty directory `/home/russ/testpatches`
* Start `cownfsd` with `/home/russ/testpatches` as the root exported directory and mount it at `/mnt/cow`
* Note: the path you give to `mount` is relative to the path you specify to `cownfsd`, so in this example I would have issued these two commands, where `localhost:/` refers to the root exported directory (`/home/russ/testpatches`) and `/mnt/cow` is the mount point:
    1. `cownfsd /home/russ/testpatches &`
    2. `mount -t nfs localhost:/ /mnt/cow`
* Create a subdirectory `/home/russ/testpatches/linux1`
* Create a file called `.mount` in the `linux1` directory with the single line:

    `read "/usr/src/linux"`
* The directory `/mnt/cow/linux1` is immediately populated with the complete contents of the `/usr/src/linux` directory.
* Make changes within `/mnt/cow/linux1`, say running `make config`
* All new and modified files are updated in a sparsely created directory structure rooted at `/home/russ/testpatches/linux1`. A deleted file _foo_ is marked by the creation of an empty file called `.~~`_foo_

Similarly, you could create a new directory called `/home/russ/testpatches/linux2` which would be visible as `/mnt/cow/linux2` and could mount the same read-only base image (`/usr/src/linux`). Changes made in the two directories are completely isolated from each other.

This was written originally for use with [Xen](http://www.cl.cam.ac.uk/Research/SRG/netos/xen/index.html). Multiple domains can use overlays to create custom versions of a common root filesystem, which they can then use for booting. This lets multiple concurrent domains share the same root image by booting over NFS with customized overlays.


Basic rules
-----------

* Mounts can be many levels deep, and each layer can be read-only or writable
* The top layer must always be writable (though file permissions may still prevent writes from succeeding), and files in higher levels _mask_ files in lower levels.
* A file called `.~~`_name_ in a higher level hides a file called _name_ in a lower level.
* Every file is _visible_ from exactly one layer (copy-on-write works at the whole-file level)
* If the visible instance of a file is in a writeable layer, then all writes are made directly to it. If the visible layer is readonly, then the first write to the file triggers a copy to the top level which then becomes the visible instance of the file.
* New files are always created in the top writable directory
* Entries in `.mount` files are one per line. A `.mount` file can exist in any directory, and affects all subdirectories recursively.
* `.mount` file entries can be one of three types:
*# `read "`_/readonly/path_`"`
*# `write "`_/writeable/path_`"`
*# `replace "`_/writable/path_`"`
* Later entries in a `.mount` file are layered on top of earlier entries. The directory containing the `.mount` file is always layered on top as writeable, except when the last entry uses `replace`.


Common problems
===============

The most common problem people have reported to me is a failure to mount properly.  The main symptom is a bunch of errors having to do with stale file handles showing up either at mount time or immediately after.

The solution to this problem is usually to kill any other `mountd` processes running on the same machine.  `cownfsd` provides its own Mount service, and the NFS service only works with it.

Mount and NFS services are closely tied together (NFSv4 combines them into a single service).  The Mount service returns a file handle identifying the root directory of the mount point, which is later passed to the NFS service as a reference point for future operations.  The structure of the file handle is unique to a particular NFS implementation so the services responding to Mount and NFS requests must be matched.  In particular, the standard kernel NFS implementation uses a file's `inode` number as the main part of its file handle, which doesn't really work with a userspace daemon like `cownfsd`.

The version problem also manifests itself when booting over an NFS-mounted file system (a very useful thing to do with `cownfsd` in a Xen environment).  If you are planning to boot Linux from a `cownfsd` volume, be sure to add `,v3` to the end of the `nfsroot` line in your boot parameter list.


Getting CoWNFSd
===============

The server is written in OCaml and the source is available under the GPL.  It is currently housed on github, after being converted from an old Bazaar/Arch repository.


Authors
=======

Russ Ross (russ at russross dot com)


Download
========

You can download this project in either [zip](http://github.com/russross/cownfs/zipball/master) or [tar](http://github.com/russross/cownfs/tarball/master) formats.

You can also clone the project with [Git](http://git-scm.com/) by running:

> $ git clone git://github.com/russross/cownfs
