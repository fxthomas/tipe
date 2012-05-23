(* Module Channel : Channel information & extraction *)

(* Functions :

  ## PUBLIC ##
    extract chan img : extracts channel from a whole image
    get mode image i j : gets pixel at index (i,j) in <image>, given extraction mode if pixel is out of bounds
    histogram chan : returns the histogram of channel <chan>

*)

open Colorspace;;
type edgemode = Roll | Flip | Nil | Extend;;

let get mode t i j =
  let h = Array.length t and w = Array.length t.(0) in
    match mode with
      | Nil -> if i < 0 || j < 0 || i >= h || j >= w then 0. else t.(i).(j)
      | Roll -> t.(i mod h).(j mod w)
      | Flip -> let ii = abs (i)
                and jj = abs (j) in
        let i0 = if (ii/h) mod 2 = 0 then ii mod h else h - 1 - (ii mod h)
        and j0 = if (jj/w) mod 2 = 0 then jj mod w else w - 1 - (jj mod w) in
          t.(i0).(j0)
      | Extend -> t.(if i < 0 then 0 else if i > h-1 then h-1 else i).(if j < 0 then 0 else if j > w-1 then w-1 else j)
;;

let extract chan img =
    Array.map (fun t -> Array.map (Colorspace.extract chan) t) img
;;

let extract_range chan rc rw img =
    Array.map (fun t -> Array.map (Colorspace.extract_range chan rc rw) t) img
;;

let histogram chan =
  let c = int_of_float (* float -> int conversion *) in
  let t = Array.make 256 0. and h = Array.length chan and w = Array.length chan.(0) in
  let f = (* base pixel frequency *) 255./.float_of_int (w * h) in
    for i = 0 to h-1 do
      for j = 0 to w-1 do
        let id = c chan.(i).(j) in
          t.(id) <- t.(id) +. f;
      done
    done;
    t
;;
