val cclabel :
  Graphics.color array array -> int -> int -> float -> float array array
val cclabel_multilist : Graphics.color array array -> (int * int) array -> float -> ((float * float * float) * ((int * int) list)) array
val findpoints_bycolor : Graphics.color array array -> float * float * float -> int -> (int * int) array
val display : int -> (int * int) list -> unit
val findcc : ((float * float * float) * 'a) array -> float * float * float -> int -> int array
val size : (float * float * float) * (int * int) list -> int -> int -> int -> int * int
val elimcc : ((float * float * float) * (int * int) list) array -> int -> int -> int -> ((float * float * float) * (int * int) list) array * int
