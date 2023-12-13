[@@@warning "-unused-value-declaration"]
[@@@warning "-unused-type-declaration"]

(* Example of using first-class modules as the core mechanism and
   implementations that are either record-like built out of callbacks
   or created from a value-based static module, as well as upcasting
   a first-class module to satisfy a smaller dependencies signature.

   I'm convinced that this works: the first-class module is istelf
   a value and therefore can subsume any particular implementation
   approach.

   The ability to do include in both signatures and impelmentations
   is huge for making this work - it means minimal boilerplate both
   for types (signature) and values (implementations); the only
   unavoidable boilerplate is 2x when there's a value-based
   implementation, but 2x is much better than Nx for N = number of layers.

   It's worth noting that we can probably add namespacing inside the
   modules fairly easily if we really want to, which is much harder
   to do in my opinion with records because recursive pattern-matching
   is syntactically challenging.
*)


module Layer0 = struct

  module type Signature = sig
    val query0a : string -> int

    val query0b : string -> float
  end

  let from_callbacks ~query0a ~query0b =
    let module Implementation = struct
      let query0a = query0a
      let query0b = query0b
    end in
    (module Implementation : Signature)
end


(* Building a new layer with record-like semantics *)
module Layer1 = struct
  module type Signature = sig
    include Layer0.Signature

    val query1a : string -> int

    val query1b : string -> float
  end

  let from_callbacks ~layer0 ~query1a ~query1b =
    let module Layer0 = (val layer0: Layer0.Signature) in
    let module Implementation = struct
      include Layer0
      let query1a = query1a
      let query1b = query1b
    end in
    (module Implementation : Signature)
end


(* Building a new layer out of a value-based static module *)
module Layer2 = struct
  module type Signature = sig
    include Layer1.Signature

    val query2a : string -> int
  end

  module ValueBased = struct
    type t = int option

    let query2a value the_string =
      Option.value value ~default:0 + String.length the_string
  end

  let from_implementation ~layer1 ~value =
    let module Layer1 = (val layer1: Layer1.Signature) in
    let module Implementation = struct
      include Layer1
      let query2a = ValueBased.query2a value
    end in
    (module Implementation : Signature)
end


(* Upcasting a first-class module to a smaller signature *)
module NeedsSmallerDependenciesSignature = struct

  module type Dependencies = sig
    val query0a: string -> int
    val query1b: string -> float
  end

  let implementation ~dependencies the_string =
    let module DependenciesUnpacked = (val dependencies: Dependencies) in
    ( DependenciesUnpacked.query0a the_string,
      DependenciesUnpacked.query1b the_string)

  let from_layer_1 ~layer1 the_string =
    let module Layer1 = (val layer1 : Layer1.Signature) in
    let dependencies = (module Layer1 : Dependencies) in
    implementation ~dependencies the_string
end
