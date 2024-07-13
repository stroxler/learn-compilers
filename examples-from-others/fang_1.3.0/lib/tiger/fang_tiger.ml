(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

open Fang_iter
include Prelude

type source_span = Source_span.t

module Nop = struct
  type _ t = unit

  let int _ _k = ()
  let string _ _k = ()
  let nil _k = ()
  let arith _ _ _ _k = ()
  let neg _ _k = ()
  let cmp _ _ _ _k = ()
  let value _ _k = ()
  let if_ ~condition:_ ~yes:_ ?no:_ _k = ()
  let while_ ~condition:_ ~body:_ _k = ()
  let for_ _ ~initial:_ ~final:_ ~body:_ _k = ()
  let break _k = ()
  let seq _ _k = ()
  let call _ _ _k = ()
  let uniform_array _ ~size:_ ~initial:_ _k = ()
  let array _ _ _k = ()
  let size _ _k = ()
  let record _ _ _k = ()
  let scope _ _ _k = ()
  let assign _ _ _k = ()
  let name _ _k = ()
  let index _ _ _k = ()
  let access _ _ _k = ()
  let var _ ?type_name:_ _ _k = ()
  let fns _ _k = ()
  let types _ _k = ()
  let fn _ _ ?type_name:_ _ _k = ()
  let param _ _k = ()
  let alias_type ~name:_ ~target:_ _k = ()
  let array_type ~name:_ ~item:_ _k = ()
  let record_type _ _ _k = ()
end

module Pretty = Pretty

type parsing_error = Parsing.error

let pp_parsing_error = Parsing.pp_error

module Parsing = Parsing.Make

type validation_error = Validation.error

module Validation = Validation.Make

(** Operations on properties. *)
module Property = struct
  type 'a t = 'a Source_span.Map.t

  exception Unknown of Source_span.t

  let query k p = Source_span.Map.find k p
  let iter p = Iter.adapt2 Source_span.Map.iter p
end

type 'a property = 'a Property.t

type analysis =
  { escaping: [`Local | `Escapes] Property.t
  ; mutability: [`Constant | `Mutates] Property.t
  ; leaves: [`Branch | `Leaf] Property.t }

let escaping w = w.escaping
let mutability w = w.mutability
let leaves w = w.leaves

module Analysis (L : TIGER) = struct
  module M0 = Escaping.Make (L)
  module M1 = Mutability.Make (M0)
  module M2 = Leaves.Make (M1)
  include M2

  let analyze t =
    let leaves, t = M2.analyze t in
    let mutability, t = M1.analyze t in
    let escaping, t = M0.analyze t in
    ({leaves; mutability; escaping}, t)
end
