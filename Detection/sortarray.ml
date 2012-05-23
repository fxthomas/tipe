type 'a sortarray = { tab : 'a array ; compare : 'a -> 'a -> bool ; length : int };; (* tableau trié par ordre croissant *)
let create compare n x = { tab = Array.make n x ; compare = compare ; length = n };;

let exchange tab i j = (* échange deux éléments du tableau *)
  let x = tab.tab.(i) in
  tab.tab.(i) <- tab.tab.(j); tab.tab.(j) <- x
;; (* ATTENTION : perd le caractère trié! *)

let rec insert e tab = (* insère un élément dans un tableau de taille fixe déjà trié, en supprimant si besoin est le dernier élément du tableau *)
  if tab.compare tab.tab.(tab.length - 1) e then () (* si l'élément est déjà plus grand que tous ceux du tableau, pas la peine de le rajouter *)
  else (* on éjecte le plus grand élément du tableau, et on décale les autre d'autant de places qu'il le faut pour que le tableau reste trié *) (
    let i = ref 0 and j = ref (tab.length - 1) in
    while (tab.compare tab.tab.(!i) e) do incr i done; (* trouve la place de l'élément à insérer *)
    while (!j > !i+1) do exchange tab !j (!j - 1); decr j done; (* décale les éléments *)
    tab.tab.(!i) <- e
  )
;;
