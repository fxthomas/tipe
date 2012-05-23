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
*)

(* Module Edgedetect : Implementation of Canny Edge Detection Algorithm *)

(* Functions :
  
  ## PUBLIC ##
    channel_sobel mode threshold image : Performs Sobel Edge Detection on <image>, with thresholding (if threshold = 0., then no thresholding)
    channel_sobel_angle mode image : Performs Sobel Edge Detection on <image>, and returns both gradients and approx. angles
    
    channel_edgetrace th_down th_up sobel_angle img_dest : Performs Edge Tracing for hysteresis threshold
    channel_nonmax sobel_angle img_dest : Performs Non-maximum Suppression


    channel_canny th_down th_up mode image : Performs actual Canny Algorithm on <image>

*)

open Graphics;;
open Util;;
open Bitmap;;
open Progressgraph;;
open Colorspace;;
open Channel;;
open Graylevel;;
open Convolution;;
open Intensitymap;;

let channel_sobel mode th (* threshold parameter *) img =
  print_status "Sobel mask..."; 
  let k_sobel_x = ([| [| -1. ; 0. ; 1. |] ; [| -2. ; 0. ; 2. |] ; [| -1. ; 0. ; 1.  |] |] : kernel) in
  let k_sobel_y = ([| [| 1. ; 2. ; 1. |] ; [| 0. ; 0. ; 0. |] ; [| -1. ; -2. ; -1.  |] |] : kernel) in
  let h = Array.length img and w = Array.length img.(0) in
  let ret = Array.make_matrix h w 0. in
  let o = 1 in
  let mx = ref 0. and my = ref 0. in
    for i = 0 to h-1 do
      set_progress ((w*i*100)/(w*h)); update_progress ();
      for j = 0 to w-1 do
        (* For each channel pixel, apply sobel edge detection, |G| = |Gx| + |Gy| *)
        mx := 0.;
        my := 0.;
        for ki = -o to o do
          for kj = -o to o do
            mx := !mx +. k_sobel_x.(ki+o).(kj+o)*.(Channel.get mode img (i+ki) (j+kj));
            my :=  !my +. k_sobel_y.(ki+o).(kj+o)*.(Channel.get mode img (i+ki) (j+kj));
          done
        done;
        ret.(i).(j) <- (let r = abs_float !mx +. abs_float !my in if th > 0. then threshold th r else r)
      done
    done;
    ret
;;

(* ============= *)
(* Useful convolution kernels *)
(* ============= *)

let k_blur = ([| [| 2. ; 2. ; 2. |] ; [| 2. ; 1. ; 2. |] ; [| 2. ; 2. ; 2. |] |] : kernel);;
let k_hpass = ([| [| 1. ; -2. ; 1. |] ; [| -2. ; 5. ; -2. |] ; [| 1. ; -2. ; 1. |] |] : kernel);;

(* =========== *)
(* Canny Edge Detection *)
(* =========== *)

let channel_threshold th img_grad_angle img_dest =
  let h = Array.length img_dest and w = Array.length img_dest.(0) in
  print_status "Thresholding...";
  for i=0 to h-1 do
    set_progress ((w*i*100)/(w*h)); update_progress ();
    for j=0 to w-1 do
      if (let g,_,_ = img_grad_angle.(i).(j) in g < th) then img_dest.(i).(j) <- 0. else img_dest.(i).(j) <- 255.
    done
  done
;;

let channel_sobel_angle mode img = (* Performs sobel edge detection, returns edge angles for canny algorithm *)
  print_status "Sobel mask...";
  let k_sobel_x = ([| [| 1. ; 0. ; -1. |] ; [| 2. ; 0. ; -2. |] ; [| 1. ; 0. ; -1.  |] |] : kernel) in
  let k_sobel_y = ([| [| -1. ; -2. ; -1. |] ; [| 0. ; 0. ; 0. |] ; [| 1. ; 2. ; 1.  |] |] : kernel) in
  let h = Array.length img and w = Array.length img.(0) in
  let ret = Array.make_matrix h w (0.,0,0.) in
  let o = 1 in
  let mx = ref 0. and my = ref 0. in
    for i = 0 to h-1 do
      set_progress ((w*i*100)/(w*h)); update_progress ();
      for j = 0 to w-1 do
        (* For each channel pixel, apply sobel edge detection, |G| = sqrt(|Gx|^2 + |Gy|^2) *)
        mx := 0.;
        my := 0.;
        for ki = -o to o do
          for kj = -o to o do
            mx := !mx +. k_sobel_x.(ki+o).(kj+o)*.(Channel.get mode img (i+ki) (j+kj));
            my :=  !my +. k_sobel_y.(ki+o).(kj+o)*.(Channel.get mode img (i+ki) (j+kj));
          done
        done;
        let theta = (atan2 !my !mx)/.3.14159*.180.0 in
          ret.(i).(j) <- (
            sqrt (!mx*. !mx +. !my*. !my), (* Gradient *)
            map_range [(-190.,-157.5,0);
                       (-157.5,-112.5,45);
                       (-112.5,-67.5,90);
                       (-67.5,-22.5,135);
                       (-22.5,22.5,0);
                       (22.5,67.5,45);
                       (67.5,112.5,90);
                       (112.5,157.5,135);
                       (157.5,190.,0)] theta (* Edge direction *), theta
          )
      done
    done;
    ret
;;

let channel_edgetrace (th_down : float) (th_up : float) (img_grad_angle : (float*int*float) array array) img_dest = (* performs hysteresis threshold on edges *)
  print_status "Edge tracing...";
  let h = Array.length img_grad_angle and w = Array.length img_grad_angle.(0) in
  let findedge row col dir =
    let iRowShift,iColShift = match dir with
      | 0 -> 1,0
      | 45 -> 1,1
      | 90 -> 0,1
      | 135 -> -1,1
      | _ -> raise (Invalid_arg 0.) in
      let i = ref (row + iRowShift) and j = ref (col + iColShift) in
      try
        while (let g,a,_ = img_grad_angle.(!i).(!j) in
                 a = dir && g > th_down) do
          img_dest.(!i).(!j) <- 255.;
          i := !i + iRowShift;
          j := !j + iColShift;
        done
      with _ -> img_dest.(!i - iRowShift).(!j - iColShift) <- 0.;
      i := row - iRowShift; j := col - iColShift;
      try 
        while (let g,a,_ = img_grad_angle.(!i).(!j) in
                 a = dir && g > th_down) do
          img_dest.(!i).(!j) <- 255.;
          i := !i - iRowShift;
          j := !j - iColShift;
        done;
      with _ -> img_dest.(!i + iRowShift).(!j + iColShift) <- 0.  in


    for i=0 to h-1 do
      set_progress (w*i*100/(w*h)); update_progress ();
      for j=0 to w-1 do
        if img_dest.(i).(j) = 255. then (findedge i j (let _,dir,_ = img_grad_angle.(i).(j) in dir))
      done
    done
;;

(* OLD VERSION
let channel_nonmax (img_grad_angle : (float*int*float) array array) img_dest = (* performs non-maximum suppression for canny algorithm *)
  print_status "Non-maximum suppression...";
  let h = Array.length img_grad_angle and w = Array.length img_grad_angle.(0) in
  let remove_nonmax row col dir =
    let iRowShift,iColShift = match dir with
      | 0 -> 1,0
      | 45 -> 1,-1
      | 90 -> 0,1
      | 135 -> 1,1
      | _ -> img_dest.(row).(col) <- 0.; h,w in
    let i = ref (row + iRowShift) and j = ref (col + iColShift) and s = Stack.create () and maxi,maxj,maxg = ref 0,ref 0,ref 0. in
      (
        try (while (let _,a,_ = img_grad_angle.(!i).(!j) and e = img_dest.(!i).(!j) in
                     a = dir && e = 255.) do
               let g,_,_ = img_grad_angle.(!i).(!j) in
                 Stack.push (!i,!j,g) s;
                 if g > !maxg then (maxi := !i; maxj := !j; maxg := g);
                 i := !i + iRowShift;
                 j := !j + iColShift
             done)
        with _ -> ()
      );
      i := row - iRowShift; j := col - iColShift;
      (
        try (while (let  _,a,_ = img_grad_angle.(!i).(!j) and e = img_dest.(!i).(!j) in
                     a = dir && e = 255.) do
               let g,_,_ = img_grad_angle.(!i).(!j) in
                 Stack.push (!i,!j,g) s;
                 if g > !maxg then (maxi := !i; maxj := !j; maxg := g);
                 i := !i - iRowShift;
                 j := !j - iColShift
             done)
        with _ -> ()
      );
      while (not (Stack.is_empty s)) do
        let i,j,g = Stack.pop s in
          if i <> !maxi || j <> !maxj then img_dest.(i).(j) <- 0. else img_dest.(i).(j) <- 255.
      done in
    for i=0 to h-1 do
      set_progress (w*i*100/(w*h)); update_progress ();
      for j=0 to w-1 do
        if img_dest.(i).(j) = 255. then remove_nonmax i j (let _,dir,_ = img_grad_angle.(i).(j) in dir)
      done
    done
;;
*)

let channel_nonmax (img_grad_angle : (float*int*float) array array) img_dest = (* performs non-maximum suppression for canny algorithm *)
  print_status "Non-maximum suppression...";
  let h = Array.length img_grad_angle and w = Array.length img_grad_angle.(0) in
  for i=0 to h-1 do
    for j=0 to w-1 do
      if img_dest.(i).(j) = 255. then (
        let _,dir,_ = img_grad_angle.(i).(j) in
        let is,js = match dir with
	  | 0 -> 1,0
	  | 45 -> 1,1
	  | 90 -> 0,1
	  | 135 -> -1,1
	  | _ -> raise (Invalid_arg 0.) in
        try (
          let gl,ge,gr = img_grad_angle.(i-is).(j-js),img_grad_angle.(i).(j),img_grad_angle.(i+is).(j+js) in
          if not (ge > gl && ge > gr) then img_dest.(i).(j) <- 0.
	) with _ -> img_dest.(i).(j) <- 0.
      )
    done
  done
;;

let can_BLUR_ENABLE = ref true and can_BLUR_SIGMA = ref 1.4 and can_EDGETRACE_ENABLE = ref true and can_NONMAX_ENABLE = ref true;;

let channel_canny th_down th_up (* upper and lower thresholds for hysteresis comparison *) mode img = (* Performs Canny Edge Detection on <img> *)
  let img_blur = (if !can_BLUR_ENABLE then channel_convolve (k_gaussianblur !can_BLUR_SIGMA 2) mode img else img) in
  if !can_BLUR_ENABLE then draw_channel img_blur 0 0;
  let img_sobel = channel_sobel_angle mode img_blur in
    channel_threshold th_up img_sobel img_blur;
    draw_channel img_blur 0 0;
    if !can_NONMAX_ENABLE then (channel_nonmax img_sobel img_blur;
    draw_channel img_blur 0 0);
    if !can_EDGETRACE_ENABLE then (channel_edgetrace th_down th_up img_sobel img_blur;
    draw_channel img_blur 0 0);
    print_status "Done!";
    set_progress 100;
    update_progress ();
    img_blur,img_sobel
;;
