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


(* I want to understand whether I can get value-based modules to nest as well as
   first-class modules do. I think the key is that the value itself has to be
   abstracted sufficiently. There are probably multiple ways to do this, but
   here's one attempt using module signatures to get that abstraction. Note that
   we start needing functorization which is a downside in my book. The
   functorization, at least on this toy example, remains relatively simple but I
   get stuck in Layer2. *)
module ValueBasedUsingSigatures_simple_does_not_work = struct

  module Layer0 = struct

    module type Signature = sig
      type t
      val query0a : t -> string -> int
      val query0b : t -> string -> float
    end

    module type AbstractValue = sig
      type t
      val as_layer0: t -> int
    end

    module Implementation(Value: AbstractValue): Signature with type t = Value.t = struct
      type t = Value.t
      let query0a value the_string = Value.as_layer0 value + String.length the_string
      let query0b value the_string = query0a value the_string |> float_of_int
    end

  end


  module Layer1 = struct

    module type Signature = sig
      include Layer0.Signature
      val query1a : t -> string -> int
      val query1b : t -> string -> float
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

      module Implementation(Value: AbstractValue): Signature with type t = Value.t = struct
        include Layer0.Implementation(Value)
        let query1a value the_string = query0a value the_string + String.length (Value.as_layer1 value)
        let query1b value the_string = query1a value the_string |> float_of_int
      end
    end

    module VariantB = struct
      module type AbstractValue = sig
        include Layer0.AbstractValue
        val as_layer1: t -> float
      end

      module Implementation(Value: AbstractValue): Signature with type t = Value.t = struct
        include Layer0.Implementation(Value)
        let query1a value the_string = query0a value the_string + int_of_float (Value.as_layer1 value)
        let query1b value the_string = query1a value the_string |> float_of_int
      end
    end

  end

  module Layer2 = struct

    module type Signature = sig
      include Layer1.Signature
      val query2a : t -> string -> int
      val query2b : t -> string -> float
     end

    (* Okay, here's where I get stuck on the value-based approach using
    functors. I want to make a new AbstractValue that has to implement the
    Layer1 AbstractValue protocol, but I don't want to pin to VariantA or
    VariantB, I want to be generic over them. There's not a nice simple way to
    do this. I'm pretty sure a complex signature incantation involving some kind
    of type bound does exist, but this is exactly the kind of ever-expanding
    functor logic I want to avoid based on how out-of-hand it got in
    SharedMemory (remember we broke VSCode's syntax highlighting!) *)

    module type AbstractValue = sig
      include Layer0.AbstractValue (* How do I say "some layer 1 AbstractValue?" *)
    end

  end

end
