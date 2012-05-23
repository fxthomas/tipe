(*
#directory "/usr/lib/ocaml/";;
#directory "/usr/lib/ocaml/stublibs/";;
#load "graphics.cma";;
#load "unix.cma";;
#load "bitmap.cmo";;
#load "util.cmo";;
#load "colorspace.cmo";;
#load "progressgraph.cmo";;
#load "channel.cmo";;
#load "graylevel.cmo";;
*)

open Graphics;;
open Util;;
open Bitmap;;
open Colorspace;;
open Progressgraph;;
open Channel;;
open Graylevel;;

let substract img0 img1 = (* returns the eucl. distance btw. img0 & img1 *)
  let h = Array.length img0 and w = Array.length img0.(0) in
  let ret = Array.make_matrix h w 0. and mx = ref 0. in
  for i=0 to h-1 do
    for j=0 to w-1 do
      let c0 = rgbf_of_color img0.(i).(j)
      and c1 = rgbf_of_color img1.(i).(j) in
      let d = distance c0 c1 in if d > !mx then mx := d; ret.(i).(j) <- d
    done
  done;
  for i=0 to h-1 do
    for j=0 to w-1 do
      ret.(i).(j) <- ret.(i).(j) /. !mx *. 255.
    done
  done;
  ret
;;

let speed_filter img0 threshold ri rj img1 = (* filters img0 & img1 pixels *)
  let rh = Array.length img0 and rw = Array.length img0.(0) in
  let h  = Array.length img1 and w  = Array.length img1.(0) in
  let resultat = Array.make_matrix h w blue in
  let ret = Array.make_matrix h w 0. and mx = ref 0. in
  for i=0 to h-1 do
    for j=0 to w-1 do
      if i+ri < rh && j+rj < rw then (
        let c0 = rgbf_of_color img0.(i+ri).(j+rj)
        and c1 = rgbf_of_color img1.(i).(j) in
        let d = distance c0 c1 in if d > !mx then mx := d; ret.(i).(j) <- d
      )
    done
  done;
  for i=0 to h-1 do
    for j=0 to w-1 do
      let p = ret.(i).(j) /. !mx in if p < threshold then () else resultat.(i).(j) <- img1.(i).(j)
    done
  done;
  resultat
;;

(*let bmp0 = read_file "../movtest/mov2-0.bmp" and
    bmp1 = read_file "../movtest/mov2-2.bmp";;
let r0,r1 = speed_filter bmp0.data 0.15 bmp1.data;;
gopen_graph bmp0.h bmp0.w;;
draw_image (make_image r0) 0 0;;
draw_image (make_image r1) 0 0;;*)
