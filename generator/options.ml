(* $Id: options.ml,v 1.1 2001/03/05 01:27:46 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)


let default_int_variant = ref Syntax.Abstract;;
  (* The int variant chosen by default (i.e. int without
   * preceding _abstract, _int32, _int64, or _unboxed keyword)
   *)

let default_hyper_variant = ref Syntax.Abstract;;

(* ======================================================================
 * History:
 * 
 * $Log: options.ml,v $
 * Revision 1.1  2001/03/05 01:27:46  gerd
 * 	Initial revision.
 *
 * 
 *)
