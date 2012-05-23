(* Module Cclabel : Connected Components Labeling and color segmentation *)

(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "util.cmo";;
  #load "colorspace.cmo";;
  #load "progressgraph.cmo";;
  #load "sortarray.cmo";;
*)

open Graphics;;
open Sortarray;;
open Util;;
open Colorspace;;
open Progressgraph;;

let cclabel img si sj threshold =
  let s = Stack.create () and h = Array.length img and w = Array.length img.(0) in let a = Array.make_matrix h w 0. and np = ref 1. and  xxr,xxb,xxg = rgb_of_color img.(si).(sj) in let xr,xb,xg = ref (float_of_int xxr),ref (float_of_int xxg), ref (float_of_int xxb) in
  Stack.push (si,sj) s; a.(si).(sj) <- 255.;
  while (not (Stack.is_empty s)) do
    let ci,cj = Stack.pop s in
    (* regarde les voisins du point courant pour savoir s'ils appartiennent à la même composante connexe *)
    for i=(-1) to 1 do
      for j=(-1) to 1 do
        if (i = 0 && j = 0) || i+ci < 0 || i+ci >= h || j+cj < 0 || j + cj >= w || a.(i+ci).(j+cj) = 255.then () else (
          (* regarder si le point est suffisament proche de la composante connexe actuelle *)
          let cr,cg,cb = rgb_of_color img.(i+ci).(j+cj) in
          if abs_float (!xr -. float_of_int cr) > threshold || abs_float (!xg -. float_of_int cg) > threshold || abs_float (!xb -. float_of_int cb) > threshold then () else (Stack.push (i+ci,j+cj) s; a.(i+ci).(j+cj) <- 255.; xr := (!np *. !xr +. float_of_int cr)/.(!np +. 1.); xg := (!np *. !xg +. float_of_int cg)/.(!np +. 1.); xb := (!np *. !xb +. float_of_int cb)/.(!np +. 1.); np := !np +. 1.)
        )
      done
    done
  done;
  set_progress 100; update_progress ();
  a
;;

let cclabel_multilist img points threshold =
  let s = Stack.create () and h = Array.length img and w = Array.length img.(0) and n = Array.length points in
  let a = Array.make_matrix h w 0 in
  let components = Array.make n ((900.,900.,900.),[]) in
  let np = ref 0. in
  let xr,xb,xg = ref 0.,ref 0.,ref 0. in
  print_status ("Labeling connected components [THRESHOLD="^(string_of_float threshold)^"]...");
  for k=0 to n-1 do
    let si,sj = points.(k) in
    if a.(si).(sj) = 0 then (
      set_progress ((k+1)*100/n); update_progress ();
      let ccolor = rand_color () in
      Stack.push points.(k) s;
      a.(si).(sj) <- ccolor;
      np := 1.;
      let r,g,b = rgbf_of_color img.(si).(sj) in ( xr := r ; xg := g ; xb := b );
      while (not (Stack.is_empty s)) do
        let ci,cj = Stack.pop s in
        (* regarde les voisins du point courant pour savoir s'ils appartiennent à la même composante connexe *)
        for i=(-1) to 1 do
          for j=(-1) to 1 do
            if (i = 0 && j = 0) || i+ci < 0 || i+ci >= h || j+cj < 0 || j + cj >= w || a.(i+ci).(j+cj) <> 0 then () else (
              (* regarder si le point est suffisament proche de la composante connexe actuelle *)
              let cr,cg,cb = rgbf_of_color img.(i+ci).(j+cj) in
              if (cr = 0. && cg = 0. && cb = 255.) || abs_float (!xr -. cr) > threshold || abs_float (!xg -. cg) > threshold || abs_float (!xb -. cb) > threshold then ()
              else (
                Stack.push (i+ci,j+cj) s;
                a.(i+ci).(j+cj) <- ccolor;
                let _,c = components.(k) in components.(k) <- (0.,0.,0.),((i+ci,j+cj)::c);
                xr := (!np *. !xr +. cr)/.(!np +. 1.);
                xg := (!np *. !xg +. cg)/.(!np +. 1.);
                xb := (!np *. !xb +. cb)/.(!np +. 1.);
                np := !np +. 1.
              )
            )
          done
        done
      done;
      let _,c = components.(k) in components.(k) <- (!xr,!xg,!xb),c
    )
    else ()
  done;
  set_progress 100; update_progress ();
  components
;;

let findpoints_bycolor img (ir,ig,ib) nb = (* trouve les 'nb' premiers points les plus proches de la couleur (ir,ig,ib) dans l'image img *)
  let h = Array.length img and w = Array.length img.(0) in
  let t = Sortarray.create (tcleq (ir,ig,ib)) nb ((1000.,1000.,1000.),0,0) in
  (* parcours de l'image pour trier les points par couleur *)
  print_status "Finding probable points...";
  for i=0 to h-1 do
    set_progress ((w*i*100)/(w*h)); update_progress ();
    for j=0 to w-1 do
      Sortarray.insert ((rgbf_of_color img.(i).(j)),i,j) t
    done
  done;
  let a = Array.make nb (0,0) in
  for k=0 to nb-1 do
    let _,i,j = t.tab.(k) in a.(k) <- (i,j)
  done;
  set_progress 100; update_progress ();
  a
;;

let display h l =
  let rec d l =
    match l with
      | [] -> ()
      | (i,j)::t -> Progressgraph.plot j (h - i);d t in
  d l
;;

let findcc comp colr nb = (* trouve les 'nb' premières composantes connexes les plus proches en moyenne de (ir,ig,ib) dans l'image img *)
  let nc = Array.length comp in
  let t = Sortarray.create (fun (a,_) -> fun (b,_) -> distance colr a <= distance colr b) nb ((1000.,1000.,1000.),0) in
  let (cr,cg,cb) = colr in
  (* parcours de l'image pour trier les points par couleur *)
  print_status ("Finding probable connected components [GAMMA=("^(string_of_float cr)^", "^(string_of_float cg)^", "^(string_of_float cb)^")]...");
  for i=0 to nc-1 do
    let c,_ = comp.(i) in
    set_progress (i*nc/100); update_progress ();
    Sortarray.insert (c,i) t
  done;
  let a = Array.make nb 0 in
  for k=0 to nb-1 do
    let _,i = t.tab.(k) in a.(k) <- i
  done;
  set_progress 100; update_progress ();
  a
;;

let size cc bh bw bd = (* calcule la taille vert. & horz. (en pixels) de la composante *)
  let rec sizeR c u d l r = (* calcule les pixels extrémaux de la composante *)
    match c with
      | [] -> u,d,l,r
      | (i,j)::t -> let uu,dd,ll,rr = (if i < u then i else u),(if i > d then i else d),(if j < l then j else l),(if j > r then j else r) in sizeR t uu dd ll rr in
  let u,d,l,r = (let _,ccc = cc in
                 let u0,d0,l0,r0 = match ccc with 
                       | [] -> raise Not_found
                       | (i,j)::_ -> i,i,j,j
                 in sizeR ccc u0 d0 l0 r0) in
  (d - u),(r - l)
;;

let elimcc comp bh bw bd = (* élimine toutes les composantes qui sont trop grandes ou trop petites par rapport à la taille de la balle *)
  let n = Array.length comp in
  let s = Stack.create () in
  let nn = ref 0 in
  print_status ("Removing bad components [SIZE=("^(string_of_int bh)^", "^(string_of_int bw)^")/"^(string_of_int bd)^"]...");
  for i=0 to n-1 do
    set_progress (i*100/n); update_progress ();
    try
      let sh,sw = size comp.(i) bh bw bd in
      if abs (sh - bh) <= bd && abs (sw - bw) <= bd (* si la taille est trop grande, on élimine *)
         (* sh <= 2*sw && 2*sw <= 4*sh (* si la composante est trop allongée, on élimine *) => NON, IMPRATICABLE POUR LES VIDEOS *)
         then (Stack.push comp.(i) s; incr nn)
    with Not_found -> ()
  done;
  let t = Array.make !nn ((0.,0.,0.),[]) in
  let i = ref 0 in
  while not (Stack.is_empty s) do
    t.(!i) <- Stack.pop s;
    incr i
  done;
  set_progress 100; update_progress ();
  t,!nn
;;
