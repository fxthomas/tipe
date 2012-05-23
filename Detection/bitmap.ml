(* Module Bitmap : Uncompressed Bitmap reading library *)

(*
  #directory "/usr/lib/ocaml/";;
  #directory "/usr/lib/ocaml/stublibs/";;
  #load "graphics.cma";;
  #load "unix.cma";;
*)

(* Functions :

   BMP I/O :

   get_info fd : Returns BMP information from the file header
   get_infoheader fd : Returns BMP image information header
   get_fileheader fd : Returns BMP file header
   get_palette fd n : Returns the palette containing n elements
   get_data fd info : Returns a data matrix containing the image

   ## PUBLIC ##
     read_file name : Reads bitmap file "name"

   Conversion :

   dword_of_buffer str start : Returns the DWORD (4B) value of the first 4 bytes of str, starting from start
   word_of_buffer str start : Returns the WORD (2B) value of the first 2 bytes of str, starting from start
   color_of_buffer str start : Returns a *color* value from a RGBQUAD array
   color_of_palette str start : Returns a *color* value from an index in a palette

*)

open Graphics;;
open Unix;;

(* =========== *)
(* Definitions *)
(* =========== *)

type file_header = { bfType : string (* "BM" for a bitmap file *) ; bfSize : int32 (* bitmap file size *) ; bfOffBits : int32 (* picture bits offset *) };;
type info_header = { biSize : int32 (* info header size *) ; biWidth : int32 ; biHeight : int32 (* bitmap width/height, in pixels *) ; biBitCount : int (* 2 bytes, color bits/pîxel *) ; biCompression : int32 (* 4 bytes, compression *) ; biClrUsed : int32 (* image color count *) };;
type palette_header = color array;;
type binfo = { imageinfo : info_header ; fileinfo : file_header ; palette : palette_header };;
type bitmap = { info : binfo ; data : color array array ; h : int ; w : int }

exception IOError of string;;
exception InvalidFormat of string;;

(* ================ *)
(* Conversion procs *)
(* ================ *)

let dword_of_buffer buf start =
  let ret = ref 0l in
    for i=start to start + 3 do
      ret := Int32.add !ret (Int32.shift_left (Int32.of_int (Char.code buf.[i])) (8*(i-start)))
    done;
    !ret
;;

let word_of_buffer buf start =
  (Char.code buf.[start]) + (256 * (Char.code buf.[start + 1]))
;;

let color_of_buffer buf start =
  rgb (Char.code buf.[start+2]) (Char.code buf.[start+1]) (Char.code buf.[start])
;;

let color_of_palette (pal : palette_header) buf offset =
  pal.(Char.code buf.[offset])
;;

(* ================ *)
(* Error management *)
(* ================ *)

let written = ref 0;;

let e_io i ic = if i <> ic then raise (IOError ("Read "^(string_of_int (!written + i))^" instead of "^(string_of_int (!written + ic)))) else written := !written + i;;

(* ========= *)
(* BMP Input *)
(* ========= *)

let get_fileheader bmp_fd = (* gets file header, [ignore (lseek bmp_fd 0 SEEK_SET);] to get to it *)
  let buf_header = String.make 10 '\000' in
    e_io (read bmp_fd buf_header 0 6) 6;
    ignore (lseek bmp_fd 4 SEEK_CUR);
    e_io (read bmp_fd buf_header 6 4) 4;
    { bfType = String.sub buf_header 0 2 ; bfSize = dword_of_buffer buf_header 2 ; bfOffBits = dword_of_buffer buf_header 6 }
;;

let get_infoheader bmp_fd = (* gets info header, [ignore (lseek bmp_fd 14 SEEK_SET);] to get to it *)
  let buf_header = String.make 22 '\000' in
    e_io (read bmp_fd buf_header 0 12) 12;
    ignore (lseek bmp_fd 2 SEEK_CUR);
    e_io (read bmp_fd buf_header 12 6) 6;
    ignore (lseek bmp_fd 12 SEEK_CUR);
    e_io (read bmp_fd buf_header 18 4) 4;
    ignore (lseek bmp_fd 4 SEEK_CUR);
    { biSize = dword_of_buffer buf_header 0 ; biWidth = dword_of_buffer buf_header 4 ; biHeight = dword_of_buffer buf_header 8 ; biBitCount = word_of_buffer buf_header 12 ; biCompression = dword_of_buffer buf_header 14 ; biClrUsed = dword_of_buffer buf_header 14 }
;;

let get_palette bmp_fd n = (* returns an array of n color RGBQUADs from the bmp_fd stream, starting from the current position *)
  let buf = "\000\000\000\000" and pal = Array.make n black in
    for i=0 to n-1 do
      e_io (read bmp_fd buf 0 4) 4;
      pal.(i) <- color_of_buffer buf 0;
    done;
    pal
;;

let get_info bmp_fd = (* Returns information about a bitmap file, starting from current position in the bmp_fd stream *)
  written := 0;
  let f = get_fileheader bmp_fd in
    if f.bfType <> "BM" then raise (InvalidFormat f.bfType) else (
      let i = get_infoheader bmp_fd in
      let n = if i.biClrUsed <> 0l then i.biClrUsed else (match i.biBitCount with 1 -> 1l | 4 -> 16l | 8 -> 256l | _ -> 0l) in
        { fileinfo = f ; imageinfo = i ; palette = get_palette bmp_fd (Int32.to_int n) })
;;

let get_data bmp_fd info = (* Return a matrix containing the image data *)
  written := 0;
  ignore (lseek bmp_fd (Int32.to_int info.fileinfo.bfOffBits) SEEK_SET);
  let w = Int32.to_int info.imageinfo.biWidth and h = Int32.to_int info.imageinfo.biHeight in
  let m = Array.make_matrix h w black in
  let n = (* number of read bytes *) match info.imageinfo.biBitCount with | 1 | 2 | 4 | 8 -> 1 | 24 -> 3 | _ -> raise (InvalidFormat ((string_of_int info.imageinfo.biBitCount)^" bits/piexl")) in
  let s = 4*((w*n)/4) - (w*n) in
  let offset = (* number of bytes to skip at the end of each line *) s + if s < 0 then 4 else 0 in
  let buf = String.make n '\000' in
    for i=1 to h do
      for j=1 to w do
        e_io (read bmp_fd buf 0 n) n;
        m.(h-i).(j-1) <- match n with | 3 -> (color_of_buffer buf 0) | 1 -> (color_of_palette info.palette buf 0) | _ -> 0;
      done;
      ignore (lseek bmp_fd offset SEEK_CUR); written := !written + offset
    done;
    m
;;

let read_file filename =
  let fd = openfile filename [O_RDONLY] 0x777 in
  let info = get_info fd in
  let d = get_data fd info in
  close fd;
  { info = info ; data = d ; h = Int32.to_int info.imageinfo.biHeight ; w = Int32.to_int info.imageinfo.biWidth }
;;

let draw_bitmap bmp x y =
  let i = Graphics.make_image bmp.data in draw_image i x y
;;
