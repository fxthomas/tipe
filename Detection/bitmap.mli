type file_header = { bfType : string; bfSize : int32; bfOffBits : int32; }
type info_header = {
  biSize : int32;
  biWidth : int32;
  biHeight : int32;
  biBitCount : int;
  biCompression : int32;
  biClrUsed : int32;
}
type palette_header = Graphics.color array
type binfo = {
  imageinfo : info_header;
  fileinfo : file_header;
  palette : palette_header;
}
type bitmap = {
  info : binfo;
  data : Graphics.color array array;
  h : int;
  w : int;
}
exception IOError of string
exception InvalidFormat of string
val read_file : string -> bitmap
val draw_bitmap : bitmap -> int -> int -> unit
