(* Copyright 2004, 2005 Russ Ross
 * See the file COPYING for information about licensing and distribution.  *)


(* DEBUG *
let lookup_read_fh_name state fh name =
  let res = lookup_read_fh_name state fh name in
  print_string ("lookup_read_fh_name "^(string_of_fh state fh)^" "^name^" -> ");
  begin
    match res with
    | Some (path, info) -> print_string path
    | None -> print_string "(None)"
  end;
  print_newline ();
  res
* /DEBUG *)

(* DEBUG *
let lookup_read_fh state fh =
  let res = lookup_read_fh state fh in
  begin
    try
      print_string ("lookup_read_fh "^(string_of_fh state fh)^" -> ");
    with Not_found -> print_string ("lookup_read_fh with invalid handle -> ");
  end;
  begin
    match res with
    | Some (path, info) -> print_string path
    | None -> print_string "(None)"
  end;
  print_newline ();
  res
* /DEBUG *)

(* DEBUG *
let lookup_write_fh state fh =
  let res = lookup_write_fh state fh in
  print_string "lookup_write_fh -> ";
  begin
    match res with
    | (Some (readname, readinfo), Some writename) ->
      print_string ("read "^readname^", write "^writename)
    | (Some (readname, readinfo), None) ->
      print_string ("read "^readname^", no write")
    | (None, Some writename) ->
      print_string ("no read, write "^writename)
    | (None, None) ->
      print_string "no read, no write"
  end;
  print_newline ();
  res
* /DEBUG *)

(* DEBUG *
let lookup_write_fh_name state fh name =
  let res = lookup_write_fh_name state fh name in
  print_string "lookup_write_fh_name -> ";
  begin
    match res with
    | (Some (readname, readinfo), Some writename) ->
      print_string ("read "^readname^", write "^writename)
    | (Some (readname, readinfo), None) ->
      print_string ("read "^readname^", no write")
    | (None, Some writename) ->
      print_string ("no read, write "^writename)
    | (None, None) ->
      print_string "no read, no write"
  end;
  print_newline ();
  res
* /DEBUG *)

(* DEBUG *
let lookup_create_fh_name state fh name =
  let res = lookup_create_fh_name state fh name in
  print_string "lookup_create_fh_name -> ";
  begin
    match res with
    | (Some name, Some (oldname, oldinfo)) ->
      print_string ("name "^name^", old "^oldname)
    | (Some name, None) ->
      print_string ("name "^name^", no old")
    | (None, Some (oldname, oldinfo)) ->
      print_string ("no name, old "^oldname)
    | (None, None) ->
      print_string "no name, no old"
  end;
  print_newline ();
  res
* /DEBUG *)

