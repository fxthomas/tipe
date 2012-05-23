(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "unix.cma";;
  #load "bitmap.cmo";;
  #load "grid.cmo";;
  #load "sortarray.cmo";;
  #load "progressgraph.cmo";;
  #load "unix.cma";;
  #load "util.cmo";;
  #load "bitmap.cmo";;
  #load "colorspace.cmo";;
  #load "channel.cmo";;
  #load "cclabel.cmo";;
  #load "speed_filter.cmo";;
*)
(* Module Locator : Performs ball detection on an image *)

open Graphics;;
open Bitmap;;
open Grid;;
open Sortarray;;
open Util;;
open Bitmap;;
open Progressgraph;;
open Colorspace;;
open Channel;;
open Cclabel;;
open Speed_filter

let draw_cross h i j =
  let x0 = j and y0 = h - i  in
  set_color green;
  moveto (x0 + 5) y0;
  lineto (x0 - 5) y0;
  moveto x0 (y0 + 5);
  lineto x0 (y0 - 5);
;;

let locate bmp dmaxy gh gw dmaxc hc wc dc gamma = (* essaie de trouver la position de la balle dans une image *)
  set_progress 0; update_progress ();
  print_status "Loading image...";
  draw_bitmap bmp 0 0;
  set_progress 30; update_progress ();
  print_status ("Projecting yellow colors [DMAX_Y = "^(string_of_float dmaxy)^"]...");
  let yellow = project_image_yellow bmp.data dmaxy in
  draw_image (make_image yellow) 0 0;
  set_progress 60; update_progress ();
  print_status ("Creating grid ["^(string_of_int gh)^", "^(string_of_int gw)^"]...");
  let pts = create_grid bmp.h bmp.w gh gw in
  for i=0 to (gh*gw - 1) do
    let j,k = pts.(i) in draw_cross bmp.h j k
  done;
  set_progress 100; update_progress ();
  print_status ("Done!");
  let cc = cclabel_multilist yellow pts dmaxc in
  let ec,nc = elimcc cc hc wc dc in
  let pc = findcc ec gamma 1 in
  draw_bitmap bmp 0 0;
  let _,c = ec.(pc.(0)) in
  set_color (rand_color ()); display bmp.h c;
  let i,j = gravitycenter c in
  draw_cross bmp.h i j;
  (i,j,c)
;;


exception Break;;
let locate_follow bmp_prefix bmp_nb sth dmaxy gh gw dmaxc hc wc dc gamma = (* suit la balle pendant son mouvement *)
  let ball_pos  = Array.make (bmp_nb-1) (0,0,0,0,-1,-1) (*pos, size, speed*) in
  let prev_pos  = (fun ni -> let i,j,_,_,_,_ = ball_pos.(ni-2) in (i,j)) in

  (* INITIALISATION *)

  set_progress 0; update_progress ();
  print_status "Initializing...";
  print_status ("Loading reference image...");
  let bmp = Bitmap.read_file (bmp_prefix^"0.bmp") in let reference = bmp.data in
  draw_bitmap bmp 0 0;

  (* DETECTION STEPS *)

  let load_image     = (fun i -> print_status ("Loading image "^(string_of_int i)^"...");
                                 let bmp = Bitmap.read_file (bmp_prefix^(string_of_int i)^".bmp") in
                                 (draw_bitmap bmp 0 0; bmp.data)
                       ) in
  let filter_speed   = (fun (i,ri,rj) -> print_status ("Filtering unchanged pixels...");
                                 let r = speed_filter reference sth ri rj i in 
                                 (draw_image (make_image r) 0 0; r)
                       ) in
  let filter_project = (fun i -> print_status ("Projecting yellow colors [DMAX_Y = "^(string_of_float dmaxy)^"]...");
                                 let r = project_image_yellow i dmaxy in
                                 (draw_image (make_image r) 0 0; r)
                       ) in
  let filter_grid    = (fun i -> let h = Array.length i and w = Array.length i.(0) in
                                 print_status ("Creating grid ["^(string_of_int gh)^", "^(string_of_int gw)^"]...");
                                 let pts = create_grid h w gh gw in
                                 for ii=0 to (Array.length pts - 1) do
                                   let j,k = pts.(ii) in draw_cross h j k
                                 done;
                                 let cc = cclabel_multilist i pts dmaxc in
                                 let ec,nc = elimcc cc hc wc dc in
                                 let pc = findcc ec gamma 1 in
                                 let cc = ec.(pc.(0)) in
                                 set_color (rand_color ()); display h (let _,c = cc in c);
                                 let i,j = gravitycenter (let _,c = cc in c) and sh,sw = size cc hc wc dc in
                                 draw_cross h i j;
                                 (i,j,sh,sw)                                 
                       ) in
  set_progress 20; update_progress ();

  (* PROCESS FIRST IMAGE *)

  let img         = load_image     1         in set_progress 40; update_progress ();
  let img         = filter_speed   (img,0,0) in set_progress 60; update_progress ();
  let img         = filter_project img       in set_progress 80; update_progress ();
  let (i,j,sh,sw) = filter_grid    img       in
  ball_pos.(0) <- (i,j,sh,sw,-1,-1);
  print_status "Done!";
  set_progress 100; update_progress ();

  (* PROCESS SECOND IMAGE *)

  let img         = load_image     2         in set_progress 25; update_progress ();
  let img         = filter_speed   (img,0,0) in set_progress 50; update_progress ();
  let img         = filter_project img       in set_progress 75; update_progress ();
  let (i,j,sh,sw) = filter_grid    img       in
  let (vi,vj)     = let pi,pj = prev_pos 2 in (i-pi,j-pj) in
  ball_pos.(1) <- (i,j,sh,sw,vi,vj);
  print_status "Done!";
  set_progress 100; update_progress ();
  
  (* PROCESS ALL REMAINING IMAGES *)
  
  (try for ni=3 to (bmp_nb-1) do
    set_progress 0; update_progress ();
    let img0 = load_image ni in set_progress 25; update_progress ();
    try (
      let (pi,pj,ph,pw,pvi,pvj) = ball_pos.(ni-2)  in
      let img                   = subpicture     img0 (pi+pvi) (pj+pvj) (2 * (abs pvi + abs ph)) (2 * (abs pvj + abs pw)) in (set_color black; fill_rect 0 0 (Array.length img.(0)-1) (Array.length img-1); draw_image (make_image img) 0 0);
      let img                   = filter_speed   (img,(let r=pi+pvi-(2 * (abs pvi + abs ph)) in if r > 0 then r else raise Break),(let r=pj+pvj-(2 * (abs pvj + abs pw)) in if r > 0 then r else 0)) in set_progress 50; update_progress ();
      let img                   = filter_project img in set_progress 75; update_progress ();
      let (gi,gj,sh,sw)         = filter_grid    img in
      let (i,j)                 = ((gi + pi + pvi - 2*(abs pvi + abs ph),(gj + pj + pvj - 2*(abs pvj + abs pw)))) in
      let (vi,vj)               = (i-pi,j-pj)        in
      ball_pos.(ni-1) <- (i,j,sh,sw,vi,vj);
      print_status "Done!";
      set_progress 100; update_progress ();
      if i + vi >= bmp.h || j + vj >= bmp.w || i + vi < 0 || j + vj < 0 then raise Break
    ) with Invalid_argument _ -> (
        set_progress 25; update_progress ();
        set_color red; fill_rect 0 0 bmp.h bmp.w;
        print_status "Not found! Falling back to full image search!";
        let img         = filter_speed   (img0,0,0) in set_progress 50; update_progress ();
        let img         = filter_project img        in set_progress 75; update_progress ();
        let (i,j,sh,sw) = filter_grid    img        in
        let (vi,vj)     = let pi,pj = prev_pos ni in (i-pi,j-pj) in
        ball_pos.(ni-1) <- (i,j,sh,sw,vi,vj);
        print_status "Done!";
        set_progress 100; update_progress ();
    )
  done
  
  with Break -> ());

  (* RETURNS BALL POSITION *)
  
  ball_pos
;;

let display_pos h pos =
  for k=0 to (Array.length pos - 1) do
    let (i,j,_,_,_,_) = pos.(k) in draw_cross h i j
  done
;;

let bmp = Bitmap.read_file "../../Tests/Videos/3/CAM2/video-0.bmp";;
gopen_graph bmp.h bmp.w;;
let pos = locate_follow "../../Tests/Videos/3/CAM2/video-" 25 0.15 50. 5 5 42. 15 15 12 (250.,250.,70.);;
draw_bitmap bmp 0 0;;
display_pos bmp.h pos;;
