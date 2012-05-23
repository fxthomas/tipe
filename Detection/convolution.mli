type kernel = float array array
exception InvalidKernel of int * int
val kernel_dim : kernel -> int
val kernel_center : kernel -> int
val kernel_weight : kernel -> float * float
val channel_convolve :
  kernel -> Channel.edgemode -> float array array -> float array array
val k_gaussianblur : float -> int -> kernel
