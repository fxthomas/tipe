val of_int : int -> Graphics.color
val of_float : float -> Graphics.color
val of_image : Graphics.color array array -> float array array
val to_image : float array array -> Graphics.image
val draw_channel : float array array -> int -> int -> unit
