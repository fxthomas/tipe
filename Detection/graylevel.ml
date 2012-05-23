(* Module Graylevel : Graylevel conversion / management *)

(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "progressgraph.cmo";;
  #load "colorspace.cmo";;
  #load "channel.cmo";;
*)

(* Functions :

  ## PUBLIC ##
    of_image img : converts Graphics.color matrix to float graylevel matrix
    to_image chan : converts channel <chan> to graylevel Graphics.image

*)

open Graphics;;
open Progressgraph;;
open Colorspace;;
open Channel;;

let of_int l =
  rgb l l l
;;

let of_float f =
  let i = int_of_float f in of_int i
;;

let of_image = Channel.extract (Colorspace.Luma);;
let to_image chan = make_image (Array.map (fun t -> Array.map of_float t) chan);;
let draw_channel chan = draw_image (to_image chan);;
