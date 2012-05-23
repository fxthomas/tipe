(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "sortarray.cmo";;
  #load "progressgraph.cmo";;
  #load "unix.cma";;
  #load "util.cmo";;
  #load "bitmap.cmo";;
  #load "colorspace.cmo";;
  #load "channel.cmo";;
  #load "graylevel.cmo";;
  #load "convolution.cmo";;
  #load "intensitymap.cmo";;
  #load "raytrace.cmo";;
  #load "edgedetect.cmo";;
  #load "cclabel.cmo";;
*)
(* Module Ball_detector : Performs ball detection on an image *)

open Graphics;;
open Sortarray;;
open Progressgraph;;
open Util;;
open Bitmap;;
open Colorspace;;
open Channel;;
open Graylevel;;
open Convolution;;
open Intensitymap;;
open Raytrace;;
open Edgedetect;;
open Cclabel;;

let draw_cross h i j =
  let x0 = j and y0 = h - i  in
  set_color green;
  moveto (x0 + 5) y0;
  lineto (x0 - 5) y0;
  moveto x0 (y0 + 5);
  lineto x0 (y0 - 5);
;;

(* ===== *)
(* Tests *)
(* ===== *)

let bmp = Bitmap.read_file "../movtest/mov-2.bmp";;
gopen_graph bmp.h bmp.w;;
draw_bitmap bmp 0 0;;
let gray = Channel.extract_range Luma (191,182,110) (57,52,35) bmp.data;;
let gray = Channel.extract Luma bmp.data;;
draw_channel gray 0 0;;
let canny = let th_down = 20. and th_up = 50. in (channel_canny th_down th_up Extend gray);;
let rays = trace canny;;
draw_image (make_image (to_yuvmap rays)) 0 0;;
let _,i,j =max rays in draw_cross bmp.h i j;;
draw_channel gray 0 0;;
let _,i,j =max rays in draw_cross bmp.h i j;;

set_color black;;
fill_rect 0 0 bmp.w bmp.h;;
for i=0 to 500 do
  set_color (intensity 255 i);
  fill_rect 0 i 30 1
done;;


(* Test Results :
BALL_01 : FAILED (w && w/o tresholding -> plyr = pbm)
BALL_02 : SUCEEDED (w tresholding -> plyr = pbm)
BALL_03 : SUCEEDED (w/o tresholding)
BALL_04 : FAILED (w && w/o tresholding -> face = pbm)
BALL_05 : FAILED (w && w/o tresholding -> )
BALL_06 : SUCEEDED (w/o tresholding)
BALL_07 : FAILED (pbm = noise)
*)

let bmp = Bitmap.read_file "../movtest/mov2-3.bmp";;
gopen_graph bmp.h bmp.w;;
draw_bitmap bmp 0 0;;
let pts = findpoints bmp.data (100.,89.,35.) 30;;
let cc = cclabel_multilist bmp.data pts 40.;;
let pc = findcc cc (100.,89.,39.) 30;;
set_color black; fill_rect 0 0 bmp.w bmp.h;;
for i=0 to 29 do set_color (rand_color ()); display bmp.h (let _,c = cc.(pc.(i)) in c) done;;
let i,j = let _,l = cc.(pc.(0)) in gravitycenter l in draw_cross bmp.h i j;;
(* Test Results :
1 : FAILED
2 : SUCCEEDED (0)
3 : SUCCEEDED (0)
4 : FAILED
5 : SUCCEEDED (3)
6 : FAILED
7 : 
*)
let hh = bmp.h;;
