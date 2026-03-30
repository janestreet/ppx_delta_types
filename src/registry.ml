open! Base

module Key = struct
  type t = string list

  let create x = x
  let equal = List.equal String.equal
  let to_list x = x
end

type 'a t = (Key.t * 'a) list ref

let create () = ref []
let latest (t : _ t) = Option.map ~f:fst (List.hd !t)

let find_latest_matching (t : 'a t) target =
  List.find_map !t ~f:(fun (key, value) -> Option.some_if (Key.equal target key) value)
;;

let register t ~key ~data = t := (key, data) :: !t
