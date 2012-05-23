type space = Red | Green | Blue | Luma | Chroma_U | Chroma_V
val rgb_of_color : Graphics.color -> int * int * int
val rgbf_of_color : Graphics.color -> float * float * float
val rgb_of_yuv : int * int * int -> int * int * int
val color_of_yuv : int * int * int -> Graphics.color
val yuv_of_rgb : int * int * int -> float * float * float
val yuv_of_color : Graphics.color -> float * float * float
val extract : space -> Graphics.color -> float
val extract_range : space -> int*int*int -> int*int*int -> Graphics.color -> float
val distance : float * float * float -> float * float * float -> float
val cleq : float * float * float -> float * float * float -> float * float * float -> bool
val tcleq : float * float * float -> (float * float * float) * 'a * 'b -> (float * float * float) * 'c * 'b -> bool
val rand_color : unit -> Graphics.color
val intensity : int -> int -> Graphics.color
val distance_yellow : float * float * float -> float * bool
val project_yellow : float * float * float -> float * float * float
val project_image_yellow : Graphics.color array array -> float -> Graphics.color array array
