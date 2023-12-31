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
module SimpleFirstClass = struct
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

end



(* Here's an example of a value-and-functor based approach. I suspect that it's
   possible to write more efficient code this way than using "pure" first-class
   modules as above, since the modules can be made static, and all dynamic data
   can be packed into the values. From this example I think it's doable, I don't
   like how everything needs a functor but the complexity of the functors at
   least seems to be tolerable (it's always parameterized over a layer + value
   with a constraint to make the value both compatible with lower layers and
   extensible). *)
module ValueBasedUsingFunctors = struct

  module Layer0 = struct

    module type Signature = sig
      module Value: sig
        type t
      end
      val query0a : Value.t -> string -> int
      val query0b : Value.t -> string -> float
    end

    module type AbstractValue = sig
      type t
      val as_layer0: t -> int
    end

    module Implementation(V: AbstractValue): Signature with type Value.t = V.t = struct
      module Value = V
      let query0a value the_string = Value.as_layer0 value + String.length the_string
      let query0b value the_string = query0a value the_string |> float_of_int
    end

    module ConcreteValue = struct
      type t = int
      let as_layer0 = Fun.id
    end

    module Concrete = Implementation(ConcreteValue)

  end


  module Layer1 = struct

    module type Signature = sig
      include Layer0.Signature
      val query1a : Value.t -> string -> int
      val query1b : Value.t -> string -> float
     end

    (* Okay let's check what it looks like to inject multiple implementations
       (this is trivial for the first-class module approach). In this case it
       requires multiple functors, but the boilerplate is still
       O(implementations) + 1, so maybe not objectively worse. *)

    module VariantA = struct
      module type AbstractValue = sig
        include Layer0.AbstractValue
        val as_layer1: t -> string
      end

      module Implementation
          (L0: Layer0.Signature)
          (V: AbstractValue with type t = L0.Value.t)
      : Signature with type Value.t = V.t = struct
        include L0
        module Value = V  (* Have to add this in; the Layer0 functor strips this-layer type information *)
        let query1a value the_string = query0a value the_string + String.length (Value.as_layer1 value)
        let query1b value the_string = query1a value the_string |> float_of_int
      end
    end

    module VariantB = struct
      module type AbstractValue = sig
        include Layer0.AbstractValue
        val as_layer1: t -> float
      end


      module Implementation
          (L0: Layer0.Signature)
          (V: AbstractValue with type t = L0.Value.t)
      : Signature with type Value.t = V.t = struct
        include L0
        module Value = V
        let query1a value the_string = query0a value the_string + int_of_float (Value.as_layer1 value)
        let query1b value the_string = query1a value the_string |> float_of_int
      end
    end

  end

  module Layer2 = struct

    module type Signature = sig
      include Layer1.Signature
      val query2a : Value.t -> string -> int
      val query2b : Value.t -> string -> float
    end

    module type AbstractValue = sig
      include Layer1.VariantB.AbstractValue
      val as_layer2: t -> int
    end

    module Implementation
      (L1: Layer1.Signature)
      (V: AbstractValue with type t = L1.Value.t)
    : Signature with type Value.t = V.t = struct
      include L1
      module Value = V
      let query2a value the_string = Value.as_layer0 value + String.length the_string
      let query2b value the_string = query0a value the_string |> float_of_int
    end

  end


  module ConcreteStack = struct
    module Value = struct
      type t = float
      let as_layer0 = int_of_float
      let as_layer1 = Fun.id
      let as_layer2 = int_of_float
    end

    module Layer0 = Layer0.Implementation(Value)
    module Layer1 = Layer1.VariantB.Implementation(Layer0)(Value)
    module Layer2 = Layer2.Implementation(Layer1)(Value)
  end


  let func = ConcreteStack.Layer1.query1a
end




module StaticBasedOnObjects = struct


  module Layer0 : sig
    type 'a t = (< layer0_value: int; ..> as 'a)
    val query0 : 'a t -> string -> int
  end = struct
    type 'a t = (< layer0_value: int; ..> as 'a)
    let layer0_value o = o#layer0_value
    let query0 o the_string = layer0_value o + String.length the_string
  end

  module Layer1 : sig
    type 'a t = (< layer1_value: float; .. > as 'a)
    val query1 : 'a t -> string -> float
  end = struct
    type 'a t = (< layer1_value: float; .. > as 'a)
    let layer1_value o = o#layer1_value
    let query1 o the_string = layer1_value o +. float_of_int (String.length the_string)
  end

  module Stacked = struct
    type 'a t = (< layer0_value: int ; layer1_value: float ; .. > as 'a)

    let create layer0_value layer1_value = object
        method layer0_value = layer0_value
        method layer1_value = layer1_value
    end

    module Layer0 = Layer0
    module Layer1 = Layer1

    let combined_query (o: 'a t) the_string =
      let v0 = (Layer0.query0 o the_string |> float_of_int) in
      let v1 = (Layer1.query1 o the_string) in
      v0 +. v1
  end

  let x = (Stacked.create 5 5.5 |> Stacked.combined_query) "a string"

  let show_x () = Format.print_float x

end


let () = StaticBasedOnObjects.show_x (); Format.print_newline ()
