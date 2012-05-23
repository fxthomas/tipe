(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "unix.cma";;
  #load "bitmap.cmo";;
  #load "locate_cclabel.cma";;
*)

open Graphics;;
open Bitmap;;
open Progressgraph;;
open Locator;;

let bmp = read_file "ball_07.bmp";;
gopen_graph bmp.h bmp.w;;
locate bmp 35. 55 55 50. 30 30 26 (250., 250., 70.);;

(*
1:ok
2:ok
3:ok
4:echec
5:
*)
