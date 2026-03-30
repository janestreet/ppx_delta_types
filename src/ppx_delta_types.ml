open! Base
open! Ppxlib

module Ast_pattern = struct
  include Ast_pattern

  let drop : _ t = T (fun _ _ _ k -> k)
end

let registry_key_attr =
  Attribute.declare
    "delta_types.id"
    Attribute.Context.type_declaration
    Ast_pattern.(
      pstr (pstr_eval (alt (pexp_construct __ none) (pexp_ident __)) nil ^:: nil))
    (fun lident -> Longident.flatten_exn lident)
;;

let modify_attr =
  Declaration.Attribute.declare "delta_types.modify" Ast_pattern.(pstr nil) ()
;;

let remove_attr =
  Declaration.Attribute.declare "delta_types.remove" Ast_pattern.(pstr nil) ()
;;

let declaration_name_pattern =
  Ast_pattern.(alt (pexp_ident (lident __)) (pexp_construct (lident __) none))
;;

let before_attr =
  Declaration.Attribute.declare
    "delta_types.before"
    Ast_pattern.(pstr (pstr_eval declaration_name_pattern nil ^:: nil))
    Fn.id
;;

let after_attr =
  Declaration.Attribute.declare
    "delta_types.after"
    Ast_pattern.(pstr (pstr_eval declaration_name_pattern nil ^:: nil))
    Fn.id
;;

let registry_key_for_declaration td : Registry.Key.t =
  Option.value ~default:[] (Attribute.get registry_key_attr td) @ [ td.ptype_name.txt ]
  |> Registry.Key.create
;;

let id_of_registry_key key =
  match List.rev (Registry.Key.to_list key) with
  | [] | [ _ ] -> None
  | last :: rest ->
    Some (List.fold_left rest ~init:(Lident last) ~f:(fun acc s -> Ldot (acc, s)))
;;

module Action = struct
  type place =
    | After of string
    | Before of string
    | Default

  type 'a t =
    | Add of place * 'a Declaration.t
    | Remove of string Loc.t
    | Modify of place * 'a Declaration.t
end

let parse_delta (type a) (decls : a Declaration.t list) : a Action.t list =
  List.map decls ~f:(fun decl ->
    let loc = Declaration.loc decl in
    let located_name = Declaration.located_name decl in
    let is_remove =
      match Declaration.kind decl with
      | Record ->
        Ast_pattern.parse
          Ast_pattern.(
            some
              (* stupidly we need to pass ptyp_attributes to allow attributes otherwise
                 the ptyp_* matchers will raise *)
              (ptyp_attributes drop (ptyp_extension (extension (string "remove") drop))))
          loc
          (Declaration.only_type decl)
          ~on_error:(fun () -> false)
          true
      | Poly_variant | Variant ->
        (match Declaration.consume_attribute remove_attr decl with
         | None -> false
         | Some ((_ : a Declaration.t), ()) ->
           (* we don't need the new decl because we're removing it *)
           true)
    in
    let decl, after_place =
      match Declaration.consume_attribute after_attr decl with
      | None -> decl, None
      | Some (decl, value) -> decl, Some value
    in
    let decl, before_place =
      match Declaration.consume_attribute before_attr decl with
      | None -> decl, None
      | Some (decl, value) -> decl, Some value
    in
    let place : Action.place =
      match after_place, before_place with
      | None, None -> Default
      | Some _, Some _ ->
        Location.raise_errorf ~loc "Specify at most one of [@after] and [@before]"
      | None, Some before -> Before before
      | Some after, None -> After after
    in
    let decl, is_modify =
      match Declaration.consume_attribute modify_attr decl with
      | None -> decl, false
      | Some (decl, ()) -> decl, true
    in
    match is_remove, place, is_modify with
    | true, (Before _ | After _), _ | true, _, true ->
      Location.raise_errorf ~loc "Cannot simultaneous remove and add or modify a field"
    | true, Default, false -> Action.Remove located_name
    | false, place, true -> Modify (place, decl)
    | false, place, false -> Add (place, decl))
;;

(* This attribute tells ppx_stable about which fields are added or removed so that it can
   generate the stable upgrade function. *)
let generate_stable_attribute (type decl) ~loc (actions : decl Action.t list) =
  let open (val Ast_builder.make loc) in
  let add, modify, remove =
    List.partition3_map actions ~f:(function
      | Add (_, new_decl) -> `Fst (Declaration.name new_decl)
      | Modify (_, new_decl) -> `Snd (Declaration.name new_decl)
      | Remove name -> `Trd name.txt)
  in
  (* The nomenclature of ppx_stable is backwards of ours. ppx_stable thinks of actions as
     what we need to do to the current type to get back the old type *)
  Ppx_stable.make_stable_changes_attribute ~loc ~add:remove ~modify ~remove:add ~set:[] ()
;;

let raise_if_any_duplicates decls =
  List.map decls ~f:(fun d -> Declaration.name d, d)
  |> Map.of_alist_reduce (module String) ~f:(fun bad1 bad2 ->
    let kind = Declaration.kind bad1 in
    let loc1, loc2 = Declaration.loc bad1, Declaration.loc bad2 in
    let loc1, loc2 = if Location.compare loc1 loc2 < 0 then loc1, loc2 else loc2, loc1 in
    Location.Error.make
      ~loc:loc2
      (Printf.sprintf
         "This %s named %s duplicates an existing one"
         (Declaration.Kind.component kind)
         (Declaration.name bad1))
      ~sub:
        [ ( loc1
          , Printf.sprintf
              "The first %s was defined here"
              (Declaration.Kind.component kind) )
        ]
    |> Location.Error.raise)
  |> (ignore : _ Declaration.t Map.M(String).t -> unit);
  decls
;;

let apply_delta
  (type decl)
  ~prev_loc
  ~loc
  (decls : decl Declaration.t list)
  (actions : decl Action.t list)
  : decl Declaration.t list
  =
  let kind =
    match decls with
    | [] -> Location.raise_errorf "BUG: Got an empty type definition"
    | decl :: _ -> Declaration.kind decl
  in
  let split_on_name_exn ~loc ~action name (decls : _ Declaration.t Split_list.t) =
    match
      Split_list.split_after decls ~f:(fun d -> String.equal name (Declaration.name d))
      |> Split_list.remove_before
    with
    | None ->
      Location.Error.make
        ~loc
        (Printf.sprintf
           "Unable to find %s named %s to %s"
           (Declaration.Kind.component kind)
           name
           action)
        ~sub:[ prev_loc, "Previous declaration was here" ]
      |> Location.Error.raise
    | Some result -> result
  in
  let move_to_place ~loc place decls =
    match (place : Action.place) with
    | Default -> decls
    | Before name ->
      let decl, decls = split_on_name_exn ~loc ~action:"add before" name decls in
      Split_list.insert_after decls decl
    | After name ->
      let decl, decls = split_on_name_exn ~loc ~action:"add after" name decls in
      Split_list.insert_before decls decl
  in
  let init =
    decls
    |> List.map ~f:(fun decl -> decl |> Declaration.move ~loc |> Declaration.hide)
    |> Split_list.create_at_end
  in
  List.fold actions ~init ~f:(fun decls action ->
    match action with
    | Modify (place, new_decl) ->
      let loc = Declaration.loc new_decl in
      let name = Declaration.name new_decl in
      let (_ : decl Declaration.t), decls =
        split_on_name_exn ~loc ~action:"modify" name decls
      in
      let decls = move_to_place ~loc place decls in
      Split_list.insert_before decls new_decl
    | Remove { txt = name; loc } ->
      let (_ : decl Declaration.t), decls =
        split_on_name_exn ~loc ~action:"remove" name decls
      in
      decls
    | Add (place, new_decl) ->
      let loc = Declaration.loc new_decl in
      let decls = move_to_place ~loc place decls in
      Split_list.insert_before decls new_decl)
  |> Split_list.to_list
  |> raise_if_any_duplicates
;;

let registry = Registry.create ()

let register_knot =
  Context_free.Rule.extension
  @@ Extension.declare
       "delta_types.delta_knot"
       Extension.Context.structure_item
       Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
       (fun ~loc ~path:_ rec_flag tds ->
         let open (val Ast_builder.make { loc with loc_ghost = true }) in
         List.iter tds ~f:(fun td ->
           match Declaration.of_type_decl td with
           | None ->
             Location.raise_errorf
               ~loc:td.ptype_loc
               "A delta knot must be a record, variant, or polymorphic variant"
           | Some declarations ->
             Registry.register
               registry
               ~key:(registry_key_for_declaration td)
               ~data:(td.ptype_loc, declarations));
         pstr_type rec_flag tds)
;;

let register_delta =
  Context_free.Rule.extension
  @@ Extension.declare
       "delta_types.delta"
       Extension.Context.structure_item
       Ast_pattern.(pstr (pstr_type __ __ ^:: nil))
       (fun ~loc ~path:_ rec_flag tds ->
         let loc = { loc with loc_ghost = true } in
         let open (val Ast_builder.make loc) in
         let tds =
           List.map tds ~f:(fun td ->
             let key = registry_key_for_declaration td in
             let prev_loc, T { kind = source_kind; declarations = source_decls } =
               match Registry.find_latest_matching registry key with
               | None ->
                 let reason =
                   match Registry.latest registry with
                   | None -> "Did you mean to use type%delta_knot instead?"
                   | Some prev_key ->
                     (match id_of_registry_key prev_key, id_of_registry_key key with
                      | None, None ->
                        "The type name of the delta type must match the type you want to \
                         apply to."
                      | Some _, None -> "Did you forget to include an [@@id] attribute?"
                      | _, Some _ ->
                        "The [@@id] attributes of the delta type and the type to modify \
                         must match.")
                 in
                 Location.raise_errorf
                   ~loc:td.ptype_loc
                   "Unable to find previous declaration.\n%s"
                   reason
               | Some source -> source
             in
             let (T { kind = delta_kind; declarations = delta_decls }) =
               match Declaration.of_type_decl td with
               | None ->
                 Location.raise_errorf
                   ~loc:td.ptype_loc
                   "A delta type must be a record or a variant"
               | Some data -> data
             in
             let T =
               match Declaration.Kind.same_witness delta_kind source_kind with
               | None ->
                 Location.Error.make
                   ~loc:td.ptype_loc
                   (Printf.sprintf
                      "You cannot apply a %s delta onto a %s type"
                      (Declaration.Kind.to_string delta_kind)
                      (Declaration.Kind.to_string source_kind))
                   ~sub:[ prev_loc, "Previous type was defined here" ]
                 |> Location.Error.raise
               | Some eq -> eq
             in
             let delta = parse_delta delta_decls in
             let result_decls = apply_delta ~prev_loc ~loc source_decls delta in
             let result : Declaration.Packed_list.t =
               T { kind = delta_kind; declarations = result_decls }
             in
             Registry.register registry ~key ~data:(td.ptype_loc, result);
             let td = Declaration.replace_type_in_decl td result in
             { td with
               ptype_attributes =
                 generate_stable_attribute ~loc delta :: td.ptype_attributes
             })
         in
         pstr_type rec_flag tds)
;;

let () =
  Driver.register_transformation "delta_types" ~rules:[ register_knot; register_delta ]
;;
