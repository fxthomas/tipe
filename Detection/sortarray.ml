type 'a sortarray = { tab : 'a array ; compare : 'a -> 'a -> bool ; length : int };; (* tableau tri� par ordre croissant *)
let create compare n x = { tab = Array.make n x ; compare = compare ; length = n };;

let exchange tab i j = (* �change deux �l�ments du tableau *)
  let x = tab.tab.(i) in
  tab.tab.(i) <- tab.tab.(j); tab.tab.(j) <- x
;; (* ATTENTION : perd le caract�re tri�! *)

let rec insert e tab = (* ins�re un �l�ment dans un tableau de taille fixe d�j� tri�, en supprimant si besoin est le dernier �l�ment du tableau *)
  if tab.compare tab.tab.(tab.length - 1) e then () (* si l'�l�ment est d�j� plus grand que tous ceux du tableau, pas la peine de le rajouter *)
  else (* on �jecte le plus grand �l�ment du tableau, et on d�cale les autre d'autant de places qu'il le faut pour que le tableau reste tri� *) (
    let i = ref 0 and j = ref (tab.length - 1) in
    while (tab.compare tab.tab.(!i) e) do incr i done; (* trouve la place de l'�l�ment � ins�rer *)
    while (!j > !i+1) do exchange tab !j (!j - 1); decr j done; (* d�cale les �l�ments *)
    tab.tab.(!i) <- e
  )
;;
