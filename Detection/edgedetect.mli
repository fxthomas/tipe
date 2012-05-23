val channel_sobel :
  Channel.edgemode -> float -> float array array -> float array array
val channel_sobel_angle :
  Channel.edgemode -> float array array -> (float * int * float) array array
val channel_edgetrace :
  float -> float -> (float * int * float) array array -> float array array -> unit
val channel_nonmax : (float * int * float) array array -> float array array -> unit
val channel_canny :
  float ->
  float -> Channel.edgemode -> float array array -> float array array * (float * int * float) array array
val can_BLUR_ENABLE : bool ref
val can_BLUR_SIGMA : float ref
val can_EDGETRACE_ENABLE : bool ref
val can_NONMAX_ENABLE : bool ref
