(* Module Util : Utility functions *)

(* Functions :

  ## PUBLIC ##
    threshold h x : Performs thresholding on x, given threshold h ( if x <= h, 0. else, 1.)
    map_range ranges x : (ranges format : (float*float*'a) list) maps x to the first range it belongs to

*)

exception Invalid_arg of float;;

let threshold h x = (* x <= h => 0, x > h => 1 *)
  match compare x h with
    | -1 | 0 -> 0.
    | 1 -> 16777215.
    | _ -> 0.
;;

let rec map_range ranges x = (* maps x onto the ranges defined by <ranges> *)
  match ranges with
    | [] -> raise (Invalid_arg x)
    | (min,max,vl)::t -> if x <= max && x >= min then vl else map_range t x
;;

let fill t v =
  for i=0 to (Array.length t - 1) do
    for j=0 to (Array.length t.(0) - 1) do
      t.(i).(j) <- v
    done
  done
;;

let ftime f = let t = Sys.time () in let r = f () in (r,Sys.time () -. t);;

let test_gray r g b = (r-g)*(r-g) + (r-b)*(r-b) + (g-b)*(g-b) < 80;;
let test_white r g b = (r > 245) && (g > 245) && (b > 245)
let rec extract n l = (* extrait les n premiers éléments de la liste l *)
  if n = 0 then [] else match l with
    | [] -> []
    | h::t -> h::(extract (n-1) t)
;;
let rec insert compare e pL = (* insère un élément dans une pile déjà triée *)
  match pL with
    | [] -> [e]
    | h::t -> if compare e h then e::h::t else h::(insert compare e t)
;;

exception No_center;;
let gravitycenter lpts =
  let rec gravitycenter_r l fn = (* returns the center of gravity of a given 'lpts' point list *)
    match l with
      | [] -> raise No_center
      | [(i,j)] -> ((float_of_int i),(float_of_int j),fn)
      | (i,j)::t -> let fi,fj,ffn = gravitycenter_r t (fn +. 1.) in ((fi +. float_of_int i),(fj +. float_of_int j),ffn) in
  let fi,fj,fn = gravitycenter_r lpts 1. in (int_of_float (fi /. fn),int_of_float (fj /. fn))
;;

let subpicture img ci cj dh dw =
  let ret = Array.make_matrix (2*dh + 1) (2*dw + 1) 255 in
  for i=(-dh) to dh do
    for j=(-dw) to dw do
      ret.(i+dh).(j+dw) <- img.(ci+i).(cj+j)
    done
  done;
  ret
;;
