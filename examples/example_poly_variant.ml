open! Core

[@@@warning "-34-32-60"]

module Increasing_versions = struct
  (* At time T1 the file looks like this (the signature would be in the mli file but it's
     inlined here for convenience) *)

  module T1 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B
      | `C
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V1 : sig
        type nonrec t = t
      end
    end
  end = struct
    module Stable = struct
      module V1 = struct
        type%delta_knot t =
          [ `A1
          | `A2
          | `A3
          | `B
          | `C
          ]
        [@@deriving enumerate, sexp, stable_variant]
      end
    end

    include Stable.V1
  end

  (* At time T2 we add V2. In the signature we move the exposed type from V1 to V2 and
     modify it in place. In the implementation we use delta types. *)
  module T2 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B of unit
      | `D
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V1 : sig
        type t [@@deriving sexp]
      end

      module V2 : sig
        type nonrec t = t [@@deriving sexp]
      end
    end
  end = struct
    module Stable = struct
      module V1 = struct
        type%delta_knot t =
          [ `A1
          | `A2
          | `A3
          | `B
          | `C
          ]
        [@@deriving enumerate, sexp, stable_variant]
      end

      module V2 = struct
        type%delta t =
          (* A* are not mentioned so they stay the same *)
          [ `B of unit (* Type of B has changed so we have to annotate it *) [@modify]
          | `C (* C is removed so we have to annotate it *) [@remove]
          | `D
          ]
        (* Note that adding requires no annotation *)
        [@@deriving enumerate, sexp, stable_variant ~version:V1.t]
      end
    end

    include Stable.V2
  end

  (* Do one more change at time T3 *)
  module T3 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B of unit
      | `D
      | `E
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V1 : sig
        type t [@@deriving enumerate, sexp]
      end

      module V2 : sig
        type t [@@deriving enumerate, sexp]
      end

      module V3 : sig
        type nonrec t = t [@@deriving enumerate, sexp]

        val of_prev : V2.t -> t
      end
    end
  end = struct
    module Stable = struct
      module V1 = struct
        type%delta_knot t =
          [ `A1
          | `A2
          | `A3
          | `B
          | `C
          ]
        [@@deriving enumerate, sexp, stable_variant]
      end

      module V2 = struct
        type%delta t =
          [ `B of unit [@modify]
          | `C [@remove]
          | `D
          ]
        [@@deriving enumerate, sexp, stable_variant ~version:V1.t]
      end

      module V3 = struct
        type%delta t = [ `E ] [@@deriving enumerate, sexp, stable_variant ~version:V2.t]

        (* [stable_variant] did not ask you to specify what was added or removed due to
           the integration with delta types. *)
        let of_prev = of_V2_t
      end
    end

    include Stable.V3
  end

  open T3

  let%expect_test "enumerate V1" =
    List.iter Stable.V1.all ~f:(printf !"%{sexp:Stable.V1.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      B
      C
      |}]
  ;;

  let%expect_test "enumerate V2" =
    List.iter Stable.V2.all ~f:(printf !"%{sexp:Stable.V2.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      (B ())
      D
      |}]
  ;;

  let%expect_test "enumerate V3" =
    List.iter Stable.V3.all ~f:(printf !"%{sexp:Stable.V3.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      (B ())
      D
      E
      |}]
  ;;
end

module Decreasing_versions = struct
  module T1 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B
      | `C
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving sexp]
      end
    end
  end = struct
    module Stable = struct
      module V1 = struct
        type t =
          [ `A1
          | `A2
          | `A3
          | `B
          | `C
          ]
        [@@deriving sexp, stable_variant]
      end
    end

    include Stable.V1
  end

  (* Now, we need to modify B, remove C, add D: *)
  module T2 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B of unit
      | `D
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V2 : sig
        type nonrec t = t [@@deriving sexp]
      end

      module V1 : sig
        type t [@@deriving sexp]
      end
    end
  end = struct
    (* 1. Rename module V1 -> V2
       2. Change the type to how you want it
       3. Add module V1 containing the delta type reversing your change
    *)

    module Stable = struct
      module V2 = struct
        type%delta_knot t =
          [ `A1
          | `A2
          | `A3
          | `B of unit
          | `D
          ]
        [@@deriving enumerate, sexp, stable_variant]
      end

      module V1 = struct
        type%delta t =
          [ `B [@modify]
          | `C
          | `D [@remove]
          ]
        [@@deriving enumerate, sexp, stable_variant ~version:V2.t]
      end
    end

    include Stable.V2
  end

  (* Now, add E: *)
  module T3 : sig
    type t =
      [ `A1
      | `A2
      | `A3
      | `B of unit
      | `D
      | `E
      ]
    [@@deriving sexp_of]

    module Stable : sig
      module V3 : sig
        type nonrec t = t [@@deriving enumerate, sexp]
      end

      module V2 : sig
        type t [@@deriving enumerate, sexp]
      end

      module V1 : sig
        type t [@@deriving enumerate, sexp]
      end
    end
  end = struct
    (* 1. Rename module V2 -> V3
       2. Change the type to how you want it
       3. Add module V2 containing the delta type reversing your change

       We didn't have to touch V1 at all. And the new module V2 is super concise.
    *)
    module Stable = struct
      module V3 = struct
        type%delta_knot t =
          [ `A1
          | `A2
          | `A3
          | `B of unit
          | `D
          | `E
          ]
        [@@deriving enumerate, sexp, stable_variant]
      end

      module V2 = struct
        type%delta t = [ `E [@remove] ]
        [@@deriving enumerate, sexp, stable_variant ~version:V3.t]
      end

      module V1 = struct
        type%delta t =
          [ `B [@modify]
          | `C
          | `D [@remove]
          ]
        [@@deriving enumerate, sexp, stable_variant ~version:V2.t]
      end
    end

    include Stable.V3
  end

  open T3.Stable

  let%expect_test "enumerate V1" =
    List.iter V1.all ~f:(printf !"%{sexp: V1.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      B
      C
      |}]
  ;;

  let%expect_test "enumerate V2" =
    List.iter V2.all ~f:(printf !"%{sexp: V2.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      (B ())
      D
      |}]
  ;;

  let%expect_test "enumerate V3" =
    List.iter V3.all ~f:(printf !"%{sexp: V3.t}\n");
    [%expect
      {|
      A1
      A2
      A3
      (B ())
      D
      E
      |}]
  ;;
end
