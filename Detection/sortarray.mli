type 'a sortarray = {
  tab : 'a array;
  compare : 'a -> 'a -> bool;
  length : int;
}
val create : ('a -> 'a -> bool) -> int -> 'a -> 'a sortarray
val insert : 'a -> 'a sortarray -> unit
