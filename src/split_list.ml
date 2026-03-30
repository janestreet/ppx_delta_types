open! Base

type 'a t =
  { before : 'a list
  ; after : 'a list
  }

let create_at_end xs = { before = List.rev xs; after = [] }
let to_list { before; after } = List.rev_append before after
let insert_before { before; after } x = { before = x :: before; after }
let insert_after { before; after } x = { before; after = x :: after }
let move_to_end { before; after } = { before = List.rev_append after before; after = [] }

let split_after { before; after } ~f =
  let not_found x = not (f x) in
  match List.split_while ~f:not_found after with
  | new_before, found :: after ->
    { before = found :: List.rev_append new_before before; after }
  | _, [] ->
    let new_after, before = List.split_while ~f:not_found before in
    { before; after = List.rev_append new_after after }
;;

let remove_before { before; after } =
  match before with
  | [] -> None
  | head :: before -> Some (head, { before; after })
;;
