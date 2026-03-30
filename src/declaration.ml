open! Base
open! Ppxlib

module Kind = struct
  type 'declaration t =
    | Record : label_declaration t
    | Variant : constructor_declaration t
    | Poly_variant : row_field t

  let same_witness : type a1 a2. a1 t -> a2 t -> (a1, a2) Type_equal.t option =
    fun x y ->
    match x, y with
    | Record, Record -> Some T
    | Variant, Variant -> Some T
    | Poly_variant, Poly_variant -> Some T
    | _ -> None
  ;;

  let to_string (type x1) (v : x1 t) =
    match v with
    | Record -> "record"
    | Variant -> "variant"
    | Poly_variant -> "polymorphic variant"
  ;;

  let component (type x1) (v : x1 t) =
    match v with
    | Record -> "field"
    | Variant | Poly_variant -> "constructor"
  ;;
end

module Attribute = struct
  module Per_component = Ppxlib.Attribute

  type 'a t =
    { label : (label_declaration, 'a) Per_component.t
    ; constructor : (constructor_declaration, 'a) Per_component.t
    ; row_field : (row_field, 'a) Per_component.t
    }

  let declare name payload k =
    { label = Per_component.declare name Per_component.Context.label_declaration payload k
    ; constructor =
        Per_component.declare name Per_component.Context.constructor_declaration payload k
    ; row_field = Per_component.declare name Per_component.Context.rtag payload k
    }
  ;;

  let consume (type decl) t (kind : decl Kind.t) (decl : decl) : (decl * _) option =
    match kind with
    | Record -> Per_component.consume t.label decl
    | Variant -> Per_component.consume t.constructor decl
    | Poly_variant -> Per_component.consume t.row_field decl
  ;;
end

type 'a t =
  { name : label Loc.t
  ; loc : Location.t
  ; kind : 'a Kind.t
  ; raw : 'a
  }

let consume_attribute attr t =
  match Attribute.consume attr t.kind t.raw with
  | None -> None
  | Some (raw, result) -> Some ({ t with raw }, result)
;;

let hide_unless_hidden attributes =
  match
    List.exists attributes ~f:(fun attr ->
      String.equal attr.attr_name.txt Merlin_helpers.hide_attribute.attr_name.txt)
  with
  | false -> Some (Merlin_helpers.hide_attribute :: attributes)
  | true -> None
;;

let hide (type decl) (t : decl t) : decl t =
  match t.kind with
  | Record ->
    (match hide_unless_hidden t.raw.pld_attributes with
     | None -> t
     | Some pld_attributes -> { t with raw = { t.raw with pld_attributes } })
  | Variant ->
    (match hide_unless_hidden t.raw.pcd_attributes with
     | None -> t
     | Some pcd_attributes -> { t with raw = { t.raw with pcd_attributes } })
  | Poly_variant ->
    (match hide_unless_hidden t.raw.prf_attributes with
     | None -> t
     | Some prf_attributes -> { t with raw = { t.raw with prf_attributes } })
;;

let mover =
  object
    inherit [Location.t] Ast_traverse.map_with_context
    method! location new_loc _ = new_loc

    (* we need to leave attributes untouched so that ppxlib can identify an attribute used
       for a later type-definition *)
    method! attribute _ x = x
  end
;;

let move (type decl) (t : decl t) ~loc : decl t =
  match t.kind with
  | Record -> { t with loc; raw = mover#label_declaration loc t.raw }
  | Variant -> { t with loc; raw = mover#constructor_declaration loc t.raw }
  | Poly_variant -> { t with loc; raw = mover#row_field loc t.raw }
;;

let rtag_contents_exn ~loc : row_field_desc -> label loc * core_type option = function
  | Rtag (label, true, []) -> label, None
  | Rtag (label, false, [ core_type ]) -> label, Some core_type
  | Rtag _ ->
    Location.raise_errorf
      ~loc
      "ppx_delta_types does not support polymorphic variants whose parameter contains an \
       intersection"
  | Rinherit (_ : core_type) ->
    (* Inherited polymorphic variants can't be used in delta types because the AST doesn't
       store the inherited type's internal rtags. It simply indicates the name of the
       inherited type. This would make it hard impossible for child versions of the delta
       type to remove specific rtags that were inherited. *)
    Location.raise_errorf
      ~loc
      "ppx_delta_types does not support inherited polymorphic variants."
;;

let only_type (type decl) (t : decl t) =
  match t.kind with
  | Record -> Some t.raw.pld_type
  | Variant ->
    (match t.raw.pcd_args with
     | Pcstr_tuple [ arg ] -> Some (Ppxlib_jane.Shim.Pcstr_tuple_arg.to_core_type arg)
     | _ -> None)
  | Poly_variant -> rtag_contents_exn ~loc:t.loc t.raw.prf_desc |> snd
;;

let kind t = t.kind
let name t = t.name.txt
let located_name t = t.name
let loc t = t.loc
let raw t = t.raw

module Packed_list = struct
  type 'kind declaration = 'kind t

  type t =
    | T :
        { kind : 'kind Kind.t
        ; declarations : 'kind declaration list
        }
        -> t
end

let of_raw (type decl) (kind : decl Kind.t) (raw : decl) =
  match kind with
  | Record -> { name = raw.pld_name; loc = raw.pld_loc; raw; kind }
  | Variant -> { name = raw.pcd_name; loc = raw.pcd_loc; raw; kind }
  | Poly_variant ->
    let name = rtag_contents_exn ~loc:raw.prf_loc raw.prf_desc |> fst in
    { name; loc = raw.prf_loc; raw; kind }
;;

let of_type_decl td : Packed_list.t option =
  match Ppxlib_jane.Shim.Type_kind.of_parsetree td.ptype_kind with
  | Ptype_record fields ->
    Some (T { kind = Record; declarations = List.map ~f:(of_raw Record) fields })
  | Ptype_record_unboxed_product _ ->
    Location.raise_errorf
      ~loc:td.ptype_loc
      "ppx_delta_types: unboxed record types not yet supported"
  | Ptype_variant cons ->
    Some (T { kind = Variant; declarations = List.map ~f:(of_raw Variant) cons })
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | Some { ptyp_desc; ptyp_loc = loc; ptyp_loc_stack = _; ptyp_attributes = _ } ->
       (* The ignored fields are accounted for when we [replace_type_in_decl] below *)
       (match ptyp_desc with
        | Ptyp_variant (row_fields, openness, labels) ->
          (match openness, labels with
           | Closed, None ->
             (* We only support fully-refined poly variants. All other poly variant types
                are disallowed below. *)
             Some
               (T
                  { kind = Poly_variant
                  ; declarations = List.map ~f:(of_raw Poly_variant) row_fields
                  })
           | _, Some _ | Open, _ ->
             Location.raise_errorf
               ~loc
               "ppx_delta_types only supports exact, fully-refined polymorphic variants")
        | _ -> None)
     | None -> None)
  | Ptype_open -> None
;;

let replace_type_in_decl td (packed_list : Packed_list.t) : type_declaration =
  match packed_list with
  | T { kind = Poly_variant; declarations = row_fields } ->
    let row_fields = List.map row_fields ~f:raw in
    let ptyp_desc = Ptyp_variant (row_fields, Closed, None) in
    (* Option.value_exn is safe here because all Poly_variants are required to have a
       manifest core_type to use this PPX. *)
    let manifest_type =
      Option.value_exn ~message:"BUG: abstract type without manifest" td.ptype_manifest
    in
    let core_type = { manifest_type with ptyp_desc } in
    { td with ptype_manifest = Some core_type }
  | T { kind = Record; declarations = fields } ->
    { td with ptype_kind = Ptype_record (List.map ~f:raw fields) }
  | T { kind = Variant; declarations = cons } ->
    { td with ptype_kind = Ptype_variant (List.map ~f:raw cons) }
;;
