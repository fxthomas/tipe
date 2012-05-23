type edgemode = Roll | Flip | Nil | Extend
val get : edgemode -> float array array -> int -> int -> float
val extract :
  Colorspace.space -> Graphics.color array array -> float array array
val histogram : float array array -> float array
val extract_range : Colorspace.space -> int*int*int -> int*int*int -> Graphics.color array array -> float array array
