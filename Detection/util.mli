exception Invalid_arg of float
val threshold : 'a -> 'a -> float
val map_range : (float * float * 'a) list -> float -> 'a
val fill : 'a array array -> 'a -> unit
val ftime : (unit -> 'a) -> ('a * float)
val test_gray : int -> int -> int -> bool
val test_white : int -> int -> int -> bool
val extract : int -> 'a list -> 'a list
val insert : ('a -> 'a -> bool) -> 'a -> 'a list -> 'a list
val gravitycenter : (int * int) list -> int * int
val subpicture : int array array -> int -> int -> int -> int -> int array array

