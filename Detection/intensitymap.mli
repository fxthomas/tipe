type imap = int array array
val plot : int array array -> int -> int -> unit
val line : int array array -> int -> int -> float -> unit
val max : int array array -> int * int * int
val to_channel : imap -> float array array
val to_yuvmap : imap -> Graphics.color array array
