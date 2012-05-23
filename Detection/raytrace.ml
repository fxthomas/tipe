(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "progressgraph.cmo";;
  #load "unix.cma";;
  #load "util.cmo";;
  #load "bitmap.cmo";;
  #load "colorspace.cmo";;
  #load "channel.cmo";;
  #load "graylevel.cmo";;
  #load "convolution.cmo";;
  #load "intensitymap.cmo";;
  #load "edgedetect.cmo";;
*)

(* Module Raytrace : circle detection by tracing rays from the edge directions *)

let trace canny = (* Traces edge rays from canny edge detector output *)
  let edges,ga = canny in
  let h = Array.length edges and w = Array.length edges.(0) in
  let imap = Array.make_matrix h w 0 in
  for i=0 to h-1 do
    for j=0 to w-1 do
      let _,_,theta = ga.(i).(j) in
      if edges.(i).(j) = 255. then Intensitymap.line imap i j theta
    done
  done;
  imap
;;
