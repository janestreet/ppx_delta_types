open! Base

type 'a t

val create_at_end : 'a list -> 'a t
val to_list : 'a t -> 'a list
val split_after : 'a t -> f:('a -> bool) -> 'a t
val move_to_end : 'a t -> 'a t
val remove_before : 'a t -> ('a * 'a t) option
val insert_before : 'a t -> 'a -> 'a t
val insert_after : 'a t -> 'a -> 'a t
