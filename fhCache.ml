(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)

open Common;;

(* handles including the real and fake pathnames plus a timestamp
   note: paths follow a canonical form:
    - every path starts with a /, 
    - no path ends with a / except the path "/",
    - no double /es are permitted
   an empty string (for real only) indicates the value is unknown
*)
type handle = { fh : Fh.t;
                fake : string;
                real : string;
                stamp : float }

let expireTime = 10.0
let maxEntries = ref 10000

(* a type for handle sets ordered by timestamp *)
module StampSet = Set.Make
  (struct
    type t = handle
    let compare a b =
      (* sort by timestamp, with fh as a tie-breaker *)
      let res = Pervasives.compare a.stamp b.stamp in
      if res = 0 then Pervasives.compare a.fh b.fh else res
  end)

module Hash = Hashtbl.Make
  (struct
     type t = string
     let equal = (=)
     let hash = Util.hashString 0
  end)

(* the main cache type
     - fh -> handle
     - set of all handles ordered by timestamp
*)
type t = {
  fhToHandleHash:    handle Hash.t;
  mutable stampQueue:  StampSet.t;
  mutable count: int;
  mutable mountChange: float
}

let make size =
  maxEntries := size;
  { fhToHandleHash = Hash.create ((size * 3) / 2);
    stampQueue = StampSet.empty;
    count = 0;
    mountChange = 0.0
  }

let remove t fh =
  try
    let handle = Hash.find t.fhToHandleHash fh in

    (* fhToHandleHash *)
    Hash.remove t.fhToHandleHash fh;

    (* stampQueue *)
    t.stampQueue <- StampSet.remove handle t.stampQueue;

    (* update the counter *)
    t.count <- t.count - 1

  with Not_found -> ()

let removeHandle t handle = remove t handle.fh

let rec expire t =
  while t.count > !maxEntries
    do
      removeHandle t (StampSet.min_elt t.stampQueue)
    done

let add t fh fake real =
  Util.assertPath fake;
  if real = "" then () else Util.assertPath real;

  remove t fh;

  let handle = { fh = fh; fake = fake; real = real; stamp = Unix.time () } in

  (* fhToHandleHash *)
  Hash.add t.fhToHandleHash fh handle;

  (* stampQueue *)
  t.stampQueue <- StampSet.add handle t.stampQueue;

  (* update the counter *)
  t.count <- t.count + 1;

  (* make sure we haven't grown too big *)
  expire t

let squashHandle t handle =
  removeHandle t handle;
  add t handle.fh handle.fake ""

let fhToHandle t fh =
  let handle = Hash.find t.fhToHandleHash fh in
  let time = Unix.time () in
  let timeout = min expireTime (time -. t.mountChange) in
  if handle.real <> "" && time -. handle.stamp >= timeout
    then (squashHandle t handle; Hash.find t.fhToHandleHash fh)
    else handle

let fhToFake t fh =
  (fhToHandle t fh).fake

let fhToReal t fh =
  let real = (fhToHandle t fh).real in
  if real = "" then raise Not_found else real

let squash t file =
  try
    match file with 
    | FhOnly fh -> squashHandle t (fhToHandle t fh)
    | FhName (fh, name) ->
        let fakeList = Util.split (fhToFake t fh) @ [name] in
        squashHandle t (fhToHandle t (Fh.make fakeList))
  with Not_found -> ()

let setReal t handle real =
  add t handle.fh handle.fake real

let mountChange t =
  t.mountChange <- Unix.time ()
