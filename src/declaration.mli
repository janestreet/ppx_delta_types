open! Base
open! Ppxlib

module Kind : sig
  type 'declaration t =
    | Record : label_declaration t
    | Variant : constructor_declaration t
    | Poly_variant : row_field t

  val same_witness : 'a t -> 'b t -> ('a, 'b) Type_equal.t option
  val to_string : _ t -> string
  val component : _ t -> string
end

module Attribute : sig
  type 'a t

  val declare : label -> (payload, 'parse, 'a) Ast_pattern.t -> 'parse -> 'a t
  val consume : 'a t -> 'kind Kind.t -> 'kind -> ('kind * 'a) option
end

type 'kind t

module Packed_list : sig
  type 'kind declaration := 'kind t

  type t =
    | T :
        { kind : 'kind Kind.t
        ; declarations : 'kind declaration list
        }
        -> t
end

val raw : 'kind t -> 'kind
val kind : 'kind t -> 'kind Kind.t
val name : 'kind t -> string
val located_name : 'kind t -> string Loc.t
val loc : 'kind t -> location
val move : 'kind t -> loc:location -> 'kind t
val hide : 'kind t -> 'kind t
val only_type : 'kind t -> core_type option
val consume_attribute : 'a Attribute.t -> 'kind t -> ('kind t * 'a) option
val of_type_decl : type_declaration -> Packed_list.t option
val replace_type_in_decl : type_declaration -> Packed_list.t -> type_declaration
