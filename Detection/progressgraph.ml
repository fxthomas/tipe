(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "bitmap.cmo";;
*)

open Graphics;;
open Bitmap;;

let status_size = 100;;

(* Graphics overrides *)

let gopen_graph h w (* Main space size *) =
  Graphics.open_graph (" "^(string_of_int w)^"x"^(string_of_int (h + status_size)))
;;
let plot x y =
  Graphics.plot x (y + status_size)
;;
let moveto x y =
  Graphics.moveto x (y + status_size)
;;
let lineto x y =
  Graphics.lineto x (y + status_size)
;;
let draw_rect x y w h =
  Graphics.draw_rect x (y + status_size) w h
;;
let fill_rect x y w h =
  Graphics.fill_rect x (y + status_size) w h
;;
let draw_image img x y =
  Graphics.draw_image img x (y + status_size)
;;
let get_image x y w h =
  Graphics.get_image x (y + status_size) w h
;;
let blit_image img x y =
  Graphics.blit_image img x (y + status_size)
;;
let draw_bitmap bmp x y =
  let i = Graphics.make_image bmp.data in draw_image i x y
;;

(* Progress bar *)

let progress = ref 0;;
let progress_color = black;;
let progress_size = 20;;

let get_progress () = !progress;;
let set_progress p (* 0 < p < 100 *) = progress := if p < 0 then 0 else if p > 100 then 100 else p;;
let incr_progress p = let pp = !progress + p in set_progress pp;;
let update_progress () =
  let w = Graphics.size_x () in
    Graphics.set_color black;
    Graphics.draw_rect 1 (status_size - progress_size - 2) (w-3) progress_size;
    Graphics.set_color white;
    Graphics.fill_rect 2 (status_size - progress_size - 1) (w - 5) (progress_size - 2);
    Graphics.set_color progress_color;
    Graphics.fill_rect 3 (status_size - progress_size) ((w - 7) * !progress / 100) (progress_size - 4);
;;

(* Text bar *)

let text_area_size_y () = status_size - progress_size - 3 and text_area_size_x () = size_x () - 3;;
let text_color = black;;

let print_status s =
  let rec print_r s y =
    if s = "" || y + (let _,m = text_size s in m) >= text_area_size_y () then [],[],y - 2
    else (
      let i = ref 1 in
        while (!i < String.length s && (let m,_ = text_size (String.sub s 0 !i) in m < text_area_size_x ())) do
          incr i
        done;
        Graphics.moveto 1 y;
        let p1,p2 = if !i = String.length s then s,"" else (String.sub s 0 (!i-1)),(String.sub s (!i-1) (String.length s - !i + 1)) in
        let str,ly,max = print_r p2 (y + (let _,m = text_size p1 in m + 2)) in
          p1::str,y::ly,max
    ) in
  let rec draw_r ls ly =
    match ls,ly with
      | [],[] | _,[] | [],_ -> ()
      | (st::ts),(yn::ty) -> Graphics.moveto 1 yn; Graphics.draw_string st; draw_r ts ty in
  let ls,lly,max = print_r s 1 in
  let ly = List.rev lly in
  let buf = Graphics.get_image 1 1 (text_area_size_x ()) (text_area_size_y () - max - 1) in
    Graphics.set_color white; Graphics.fill_rect 1 1 (text_area_size_x ()) (max + 2); Graphics.set_color text_color;
    draw_r ls ly;
    Graphics.draw_image buf 1 (max + 2);
;;
