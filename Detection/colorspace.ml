(* Module Colorspace : Colorspaces conversion and extraction *)

(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
*)

(* Functions :

  ## PUBLIC ##
    rgb_of_color c : converts Graphics.color to RGB format
    yuv_of_color c : converts Graphics.color to YUV format
    rgb_of_yuv (y,u,v) : converts YUV to RGB format
    color_of_yuv (y,u,v) : converts YUV to Graphics.color format
    yuv_of_rgb (r,g,b) : converts RGB to YUV format

    extract chan c : extracts <chan> channel information from Graphics.color

*)

open Graphics;;
type space = Red | Green | Blue | Luma | Chroma_U | Chroma_V;;

let rgb_of_color (c : color) =
  let r = c lsr 16 in
  let g = (c-(r lsl 16)) lsr 8 in
  let b = (c-(r lsl 16)-(g lsl 8)) in
    (r,g,b)
;;

let rgbf_of_color (c : color) =
  let r,g,b = rgb_of_color c in
  (float_of_int r),(float_of_int g),(float_of_int b)
;;

let rgb_of_yuv (y,u,v) =
  let yf,uf,vf = float_of_int y,float_of_int u,float_of_int v in
    (
      int_of_float (1.*.yf                 +.  1.13983*.vf),
      int_of_float (1.*.yf +. -0.39465*.uf +. -0.58060*.vf),
      int_of_float (1.*.yf +.  2.03211*.uf                )
    )
;;

let color_of_yuv (y,u,v) =
  let r,g,b = rgb_of_yuv (y,u,v) in
    rgb r g b
;;

let yuv_of_rgb (r,g,b) = 
  let rf,gf,bf = float_of_int r,float_of_int g,float_of_int b in
    (
      0.299  *.rf +.  0.587  *.gf +. 0.114  *.bf,
      -0.14713*.rf +. -0.28886*.gf +. 0.436  *.bf,
      0.615  *.rf +. -0.51499*.gf +. 0.10001*.bf
    )
;;

let yuv_of_color (c : color) =
  yuv_of_rgb (rgb_of_color c)
;;

let extract chan (c : color) =
  match chan with
    | Red -> float_of_int (c lsr 16)
    | Green -> float_of_int ((c - ((c lsr 16) lsl 16)) lsr 8)
    | Blue -> float_of_int ((c - (((c - ((c lsr 16) lsl 16)) lsr 8) lsl 8) - ((c lsr 16) lsl 16)))
    | Luma -> let y,_,_ = yuv_of_color c in y
    | Chroma_U -> let _,u,_ = yuv_of_color c in u
    | Chroma_V -> let _,_,v = yuv_of_color c in v
;;

let extract_range chan (rr,gr,br) (rw,gw,bw) (c : color) =
  let r,g,b = rgb_of_color c in
  if abs (rr - r) > rw || abs (gr - g) > gw || abs (br -b) > bw then 0.
  else match chan with
    | Red -> float_of_int r
    | Green -> float_of_int g
    | Blue -> float_of_int b
    | Luma -> let y,_,_ = yuv_of_rgb (r,g,b) in y
    | Chroma_U -> let _,u,_ = yuv_of_rgb (r,g,b) in u
    | Chroma_V -> let _,_,v = yuv_of_rgb (r,g,b) in v
;;

let distance (ir,ig,ib) (r,g,b) = (* calcule le carré de la distance euclidienne dans l'espace (r,g,b) entre une couleur de référence iC et une autre couleur *)
  let dr = ir -. r and
      dg = ig -. g and
      db = ib -. b in
  dr*.dr +. dg*.dg +. db*.db
;;

let distance_yellow (r,g,b) = (* renvoie un couple contenant la distance de la couleur au plan jaune et `true` si la couleur, projetée, est dans le triangle jaune, `false` sinon *)
  let nd = abs_float (r *. 0.71 -. g *. 0.71) in (* distance entre la couleur (r,g,b) et le plan *)
  let ns = (r -. 0.71 *. nd) +. (g +. 0.71 *. nd) -. 2.*.b in (* produit scalaire indiquant si le point appartient bien au triangle jaune *)
  (nd,ns >= 0.)
;;

let project_yellow (r,g,b) = (* projette la couleur (r,g,b) sur le plan jaune *)
  let pj = (0.71 *. r -. 0.71 *. g) in
  (r -. 0.71*.pj , g +. 0.71*.pj , b)
;;

let project_image_yellow img threshold = (* projette l'image sur le plan jaune, en éliminant (couleur bleue 0x0000ff) les points carréments pas sympas *)
  let h = Array.length img and w = Array.length img.(0) in
  let dist = Array.make_matrix h w blue in
  for i=0 to h-1 do
  for j=0 to w-1 do
    let c = rgbf_of_color img.(i).(j) in
    let d,ok = distance_yellow c in
      if ok && d <= threshold then dist.(i).(j) <- let pr,pg,pb = project_yellow (rgbf_of_color img.(i).(j)) in rgb (int_of_float pr) (int_of_float pg) (int_of_float pb)
  done
done;
dist
;;

let cleq cI ca cb = (* vrai si ca est plus proche de la couleur cI que cb *)
  (distance cI ca) <= (distance cI cb)
;;

let tcleq cI ca cb = (* de même, avec la prise en compte des information de position dans l'image *)
  let cca,_,_ = ca and ccb,_,_ = cb in
  cleq cI cca ccb
;;

let rand_color () =
  let r = 50 + Random.int 200 and g = 50 + Random.int 200 and b = 50 + Random.int 200 in
  rgb r g b
;;

let intensity mx i = (* relie une valeur entière d'intensité à une couleur chaude/froide RGB *)
  let fi = float_of_int i and fmx = float_of_int mx in
  let y = int_of_float (0.15 *. 255.) and
      u = int_of_float (fi/.fmx *. 222. -. 111.) and
      v = int_of_float (fi/.fmx *. 313. -. 156.) in
  color_of_yuv (y,u,v)
;;
