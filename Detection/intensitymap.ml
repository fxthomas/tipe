(* Module Intensitymap : Intensity map management and basic graphic operations *)

(*
 #directory "/usr/lib/ocaml/";;
 #directory "/usr/lib/ocaml/stublibs/";;
 #load "graphics.cma";;
 #load "colorspace.cmo";;
 #load "channel.cmo";;
*)

open Graphics;;
open Channel;;
open Colorspace;;

type imap = int array array;;

let plot map x y =
  map.(x).(y) <- map.(x).(y) + 1
;;

let line_1stO map x0 y0 theta = (* Draws a 'line' on *map* ; angle theta / (Ox) when displayed *)
  let h = Array.length map and w = Array.length map.(0) in
  let e10 = tan (theta/.180.*.3.141592) and e01 = -1.0 in
  let e = ref 0. in
  let x = ref x0 and y = ref y0 in
  while (!x >= 0 && !y < w) do
    plot map !x !y;
    e := !e +. e10;
    if !e >= 0.5 then (decr x; e := !e +. e01);
    incr y;
  done;
  x := x0; y := y0; e := 0.;
  while (!x < h && !y >= 0) do
    plot map !x !y;
    e := !e -. e10;
    if !e <= -0.5 then (incr x; e := !e -. e01);
    decr y;
  done
;;

let line_2ndO map x0 y0 theta = (* Draws a 'line' on *map* ; angle theta / (Ox) when displayed *)
  let h = Array.length map and w = Array.length map.(0) in
  let e10 = tan ((90. -. theta)/.180.*.3.141592) and e01 = -1.0 in
  let e = ref 0. in
  let x = ref y0 and y = ref x0 in
  while (!x >= 0 && !y < h) do
    plot map !y !x;
    e := !e +. e10;
    if !e >= 0.5 then (decr x; e := !e +. e01);
    incr y;
  done;
  x := y0; y := x0; e := 0.;
  while (!x < w && !y >= 0) do
    plot map !y !x;
    e := !e -. e10;
    if !e <= -0.5 then (incr x; e := !e -. e01);
    decr y;
  done
;;

let line_8thO map x0 y0 theta = (* Draws a 'line' on *map* ; angle theta / (Ox) when displayed *)
  let h = Array.length map and w = Array.length map.(0) in
  let e10 = tan (theta/.180.*.3.141592) and e01 = 1.0 in
  let e = ref 0. in
  let x = ref x0 and y = ref y0 in
  while (!x < h && !y < w) do
    plot map !x !y;
    e := !e +. e10;
    if !e <= -0.5 then (incr x; e := !e +. e01);
    incr y;
  done;
  x := x0; y := y0; e := 0.;
  while (!x >= 0 && !y >= 0) do
    plot map !x !y;
    e := !e -. e10;
    if !e >= 0.5 then (decr x; e := !e -. e01);
    decr y;
  done
;;

let line_7thO map x0 y0 theta = (* Draws a 'line' on *map* ; angle theta / (Ox) when displayed *)
  let h = Array.length map and w = Array.length map.(0) in
  let e10 = tan ((-90. -. theta)/.180.*.3.141592) and e01 = 1.0 in
  let e = ref 0. in
  let x = ref y0 and y = ref x0 in
  while (!x < w && !y < h) do
    plot map !y !x;
    e := !e +. e10;
    if !e <= -0.5 then (incr x; e := !e +. e01);
    incr y;
  done;
  x := y0; y := x0; e := 0.;
  while (!x >= 0 && !y >= 0) do
    plot map !y !x;
    e := !e -. e10;
    if !e >= 0.5 then (decr x; e := !e -. e01);
    decr y;
  done
;;

let line map x0 y0 theta0 =
  let theta = if theta0 >= -90. && theta0 < 90. then theta0
    else if theta0 >= 90. && theta0 <= 180. then theta0 -. 180.
    else if theta0 < -90. && theta0 >= -180. then theta0 +. 180. else 0. in
  if theta >= 0. && theta < 45. then line_1stO map x0 y0 theta
  else if theta >= 45. && theta < 90. then line_2ndO map x0 y0 theta
  else if theta < 0. && theta >= -45. then line_8thO map x0 y0 theta
  else if theta < -45. && theta >= -90. then line_7thO map x0 y0 theta
;;

let max map =
  let max = ref 0 and im = ref 0 and jm = ref 0 in
  for i=0 to (Array.length map - 1) do
    for j=0 to (Array.length map.(0) - 1) do
      if map.(i).(j) > !max then (max := map.(i).(j); im := i; jm := j)
    done
  done;
  (!max, !im, !jm)
;;

let to_channel (map : imap) =
  let mx = 255. /. (float_of_int (let t,_,_ = max map in if t = 0 then 1 else t)) in
  let ret = Array.make_matrix (Array.length map) (Array.length map.(0)) 0. in
  for i=0 to (Array.length map - 1) do
    for j=0 to (Array.length map.(0) - 1) do
      ret.(i).(j) <- (float_of_int map.(i).(j)) *. mx
    done
  done;
  ret
;;

let to_yuvmap (map : imap) =
  Array.map (fun r -> Array.map (intensity (let c,_,_ = max map in c)) r) map
;;
