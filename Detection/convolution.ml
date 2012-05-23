(* Module Convolution : convolution-related operations *)

(* Functions :
  
  Kernel management :
  ## PUBLIC ##
    kernel_dim h : returns kernel dimension, or raise InvalidKernel if it isn't valid
    kernel_center h : returns kernel center coordinates, starting from the upper left corner of its matrix
    kernel_weight h : returns the positive,negative weights of the kernel matrix

  Convolution :
  ## PUBLIC ##
    channel_convolve h mode img : convolves channel with normalized <h>, boundary mode <mode>
    k_gaussianblur sigma n : returns a gaussian blue kernel, (2n+1)x(2n+1)

*)

open Channel;;
open Graphics;;
open Progressgraph;;

type kernel = float array array;;
exception InvalidKernel of int*int;;

(* ============================== *)
(* Convolution kernels management *)
(* ============================== *)

let kernel_dim (m : kernel) =
  let h = Array.length m and w = Array.length m.(0) in
    if h <> w || h mod 2 = 0 || w mod 2 = 0 then raise (InvalidKernel (h,w)) else h
;;

let kernel_center (h : kernel) = (kernel_dim h - 1)/2;;

let kernel_weight (m : kernel) = (* Returns negative&positive kernel weights *)
  let p = ref 0. and n = ref 0. in
    Array.iter (fun t -> Array.iter (fun c -> (if c > 0. then p := !p +. c else n := !n +. c)) t) m;
    !p,!n
;;

(* =========== *)
(* Convolution *)
(* =========== *)

let channel_convolve (hk : kernel) mode img =
  print_status "Convolution...";
  let h = Array.length img and w = Array.length img.(0) in
  let ret = Array.make_matrix h w 0. in
  let p = kernel_dim hk in
  let o = (p - 1)/2 in
  let wgp,wgn = kernel_weight hk in
  let m = ref 0. in
    for i = 0 to h-1 do
      set_progress ((w*i*100)/(w*h)); update_progress ();
      for j = 0 to w-1 do
        (* For each channel pixel, apply convolution *)
        m := 0.;
        for ki = -o to o do
          for kj = -o to o do
            m := !m +. hk.(ki+o).(kj+o)*.(Channel.get mode img (i+ki) (j+kj))
          done
        done;
        ret.(i).(j) <- (!m -. wgn) /. (wgp -. wgn)
      done
    done;
    ret
;;

let k_gaussianblur sigma n = (* Returns a gaussian blur (2n+1)*(2n+1) convolution mask, for a given sigma value *)
  let k = (Array.make_matrix (2*n+1) (2*n+1) 0. : kernel) in
    for i=0 to 2*n do
      for j=0 to 2*n do
        let ifl = float_of_int (i - n) and jfl = float_of_int (j - n) in
          k.(i).(j) <- exp ( -. (ifl*.ifl +. jfl*.jfl) /. (2. *. sigma *. sigma) )
      done
    done;
    k
;;
