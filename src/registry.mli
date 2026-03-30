open! Base

module Key : sig
  type t

  val create : string list -> t
  val to_list : t -> string list
end

type 'a t

val create : unit -> 'a t
val find_latest_matching : 'a t -> Key.t -> 'a option
val latest : 'a t -> Key.t option
val register : 'a t -> key:Key.t -> data:'a -> unit
