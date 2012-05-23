(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "unix.cma";;
  #load "graphics.cma";;
  #load "progressgraph.cmo";;
  #load "util.cmo";;
  #load "bitmap.cmo";;
  #load "colorspace.cmo";;
*)

open Colorspace;;
open Graphics;;
open Bitmap;;
open Util;;

let maple_of_color c =
  let r,g,b = rgb_of_color c in
  if not (test_gray r g b) then (string_of_int r)^"\t"^(string_of_int g)^"\t"^(string_of_int b)^"\n" else ""
;;

let maple_of_img img fd =
  let p = ref 0. and n = 100./.(float_of_int (Array.length img))in
  Array.iter (fun r -> p := !p +. n; Printf.printf "Converting : %f%%\n" !p; flush stdout; Array.iter (fun c -> output_string fd (maple_of_color c)) r) img
;;

let convert img_path img_dest =
  let img = let f = Bitmap.read_file img_path in f.data in
  let fd = open_out img_dest in
  maple_of_img img fd;
  close_out fd
;;

convert "colorball_01.bmp" "colorball_01_ng.mwd";;
convert "colorball_02.bmp" "colorball_02_ng.mwd";;
convert "colorball_03.bmp" "colorball_03_ng.mwd";;
convert "colorball_04.bmp" "colorball_04_ng.mwd";;
convert "colorball_05.bmp" "colorball_05_ng.mwd";;
convert "colorball_06.bmp" "colorball_06_ng.mwd";;
convert "colorball_07.bmp" "colorball_07_ng.mwd";;
