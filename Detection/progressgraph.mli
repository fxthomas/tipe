val gopen_graph : int -> int -> unit
val plot : int -> int -> unit
val moveto : int -> int -> unit
val lineto : int -> int -> unit
val draw_rect : int -> int -> int -> int -> unit
val fill_rect : int -> int -> int -> int -> unit
val draw_image : Graphics.image -> int -> int -> unit
val draw_bitmap : Bitmap.bitmap -> int -> int -> unit
val get_image : int -> int -> int -> int -> Graphics.image
val blit_image : Graphics.image -> int -> int -> unit
val get_progress : unit -> int
val set_progress : int -> unit
val incr_progress : int -> unit
val update_progress : unit -> unit
val print_status : string -> unit
