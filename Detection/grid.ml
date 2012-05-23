(* Module Grid : Contient des fonctions pour générer une grille de points *)

let create_grid h w nh nw = (* crée une grille de points *)
  let grid = Array.make ((h/nh + 1)*(w/nw + 1)) (0,0) in
  let i    = ref 0 in
  let j    = ref 0 in
  let k    = ref 0 in
  while (!j < w && !i < h) do
    grid.(!k) <- (!i,!j);
    if !i+nh >= h then (i := 1; j := !j + nw)
    else i := !i + nh;
    incr k
  done;
  grid
;;
