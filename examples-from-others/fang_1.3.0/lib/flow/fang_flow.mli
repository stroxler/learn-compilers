(* Copyright 2021 Jesse Haber-Kucharsky *)
(* SPDX-License-Identifier: GPL-3.0-only *)

(** Flow graphs.

    This is an implementation of the paper \[1\] by Ramsey and Dias.

    {1 References}

    \[1\] N. Ramsey and J. Dias, “An applicative control-flow graph based on
    Huet's zipper,” {i Electronic Notes in Theoretical Computer Science}, vol.
    148, no. 2, pp. 105–126, 2006. *)

open Fang_asm
open Fang_iter
open Fang_vm

(** Nodes in flow graphs.

    A {!block} is consists of:

    - The {!first} node
    - One or more {!middle} nodes
    - The {!last} node *)
module Node : sig
  type first =
    [ `Entry of label
      (** The entry-point of a frame with the designated label. *)
    | `Label of label  (** The definition of a label. *) ]

  (** A non-branching assembly instruction. *)
  type middle = [`Asm of asm]

  type last =
    [ `Exit  (** The end of the frame. *)
    | `Cbranch of (label -> asm) * label * (label -> asm) * label
      (** A conditional branch. See {!Graph.cbranch}. *)
    | `Branch of (label -> asm) * label
      (** An unconditional branch. See {!Graph.branch}. *) ]

  (** A generic node, in an arbitrary position. *)
  type t = [`First of first | `Middle of middle | `Last of last]

  val pp_first : first Fmt.t
  val pp_middle : box Fmt.t -> middle Fmt.t
  val pp_last : box Fmt.t -> last Fmt.t
  val pp : box Fmt.t -> t Fmt.t
end

type node = Node.t

(** A "basic-block".

    A block is a "straight-line" sequence of connected nodes in the flow graph.
    Control flow begins at the first node and ends at the last node, with no
    branches.

    Since each flow graph corresponds to a single frame (i.e., flow analysis is
    not interprocedural), nodes in a block can include calls to subroutines.

    Flow graphs are automatically organized into blocks when they're constructed
    via cursors and fragments.

    Blocks always begin with a label definition and end with either a branch to
    a different label or the end of the frame. *)
type block

module Block : sig
  type t = block

  val pp : box Fmt.t -> t Fmt.t
end

(** A flow graph for a particular frame.

    The nodes in a flow graph correspond to assembly instructions and labels. A
    directed edge exists from node [x] to node [y] if control flow can reach [y]
    directly from [x]. *)
type graph

(** Constructing, manipulating, and querying flow graphs. *)
module Graph : sig
  type t = graph

  val empty : label -> t
  (** [empty l] is an empty graph for the frame with the label [l]. *)

  val pp : box Fmt.t -> t Fmt.t

  (** A point at which graph fragments may be inserted into a graph. *)
  type cursor

  val entry : t -> cursor
  (** [entry g] is a cursor pointing to first non-entry node of [g]. *)

  val exit : t -> cursor
  (** [exit g] is a cursor pointing to the last non-exit node of [g]. *)

  val blur : cursor -> t
  (** [blur c] is the graph to which [c] is pointing. *)

  (** {1 Fragments} *)

  (** A collection of one or more nodes forming part of a graph.

      Given a cursor [c] into a graph and a fragment [f], [f c] is a cursor into
      a new graph which includes all the nodes of the original graph and also
      the nodes of [f].

      The new nodes are inserted immediately after the node to which the cursor
      was originally pointing. The position of the cursor doesn't change.

      Larger fragments are made by combining smaller ones.

      {2 Example}

      Consider the fragments [f1], [f2], [f3], and a cursor [c]. We'd like to
      insert all three fragments into the graph immediately following the node
      to which [c] is pointing.

      We first insert [f3]

      {[ let c' = f3 c ]}

      then [f2]

      {[ let c'' = f2 c' ]}

      and finally [f1]

      {[ let c''' = f1 c'' ]}

      The "order" of the new graph is [c -> f1 -> f2 -> f3].

      We could have written this as

      {[ f1 (f2 (f3 c)) ]}

      Or, using the [@@] operator, as

      {[ f1 @@ f2 @@ f3 c ]}

      Here, [@@] looks like "append" (if you squint).

      Thus, the combination of all three fragments into a single fragment [f] is

      {[ let f c = f1 @@ f2 @@ f3 c ]} *)
  type fragment = cursor -> cursor

  val asm : asm -> fragment
  (** [asm s] is a fragment consisting of a single node which is the
      (non-branching) assembly instruction [s]. *)

  val label : label -> (label -> asm) -> fragment
  (** [label l m] is a fragment consisting of a single node which is the
      definition of the label [l]. [m l] is an assembly instruction for
      branching to [l], which is necessary for forming blocks in the graph.

      {i Important}: the behaviour is unspecified when a fragment or graph
      defines the same label multiple times. *)

  val branch : (label -> asm) -> label -> fragment
  (** [branch m l] is a fragment consisting of a single node which represents an
      unconditional branch to the label [l]. [m l] is an assembly instruction
      for branching to [l]. *)

  val cbranch : (label -> asm) -> label -> (label -> asm) -> label -> fragment
  (** [cbranch m pos m_inv neg] is a fragment consisting of a single node which
      represents a branch of execution conditional on some internal state
      (typically, the result of comparing two values).

      [m pos] is an assembly instruction which conditionally branches to [pos]
      based on the value of the state.

      If the condition does not hold then execution continues at [neg].

      [m_inv neg] is an assembly instruction which checks the "inverted"
      condition of [m pos] and which conditionally branches to [neg]. This is
      required information for moving the location of the conditional branch in
      the flow graph. *)

  (** {1 Finalizing the graph} *)

  val trace : t -> t
  (** [trace g] is the graph [g] with only the blocks in [g] which are
      reachable.

      When building graphs with cursors and fragments, every intermediate graph
      consists of well-formed blocks. As a consequence, sometimes the
      materialized blocks are extraneous.

      Tracing the graph removes any unnecessary blocks.

      {i Important}: prior to tracing the graph, all branches must refer to
      defined labels. If they don't, the behaviour is unspecified. *)

  val finalize : branch:(label -> asm) -> t -> node iter
  (** [finalize ~branch g] iterates over the nodes of [g] after "clean-ups" have
      been performed on the blocks comprising [g]. These include eliminating
      unused labels and unnecessary jumps, and "taming conditional branches".

      [branch l] is an assembly instruction for jumping to label [l] and is
      necessary for finalizing the graph.

      {i Important}: prior to finalizing the graph, all branches must refer to
      defined labels. If they don't, the behaviour is unspecified. *)

  (** {1 Querying the graph} *)

  val blocks : t -> block iter
  val nodes : t -> node iter

  val find_block_with_label : label -> t -> block
  (** {i Important}: the behaviour is unspecified when the label is not defined
      in the graph. *)

  (** {1 Transforming the graph} *)

  val filter_map : (asm -> asm option) -> t -> t
  val flat_map : (asm -> asm list) -> t -> t
end

(** {1 Generic graph analysis}

    These are tools for performing generic incremental backwards analysis on
    flow-graphs based on the definition of a generic "fact" which is of interest
    for each node and rules for computing it.

    {!Liveness} is a specialization of these tools specifically for the case of
    computing which registers are "live" at every node.

    Analysis is generic because it lead to a deeper understanding of the
    applicable concepts for the author and because it was fun. *)

module type FACT = sig
  (** A fact is some kind of information corresponding to each node in a flow
      graph.

      Facts are elements of a {{:https://en.wikipedia.org/wiki/Lattice_(order)}
      lattice}. Intuitively,

      - {!initial} is the absence of knowledge of the fact: it's the least there
        is to be known
      - {!merge} combines two facts into a fact that reflects the knowledge of
        both. It's the addition of knowledge to "refine" a fact.
      - {!is_refinement} checks whether a fact is a more refined version of
        another fact (i.e., if it has more information than the other). *)
  type t

  val initial : t
  val is_refinement : prev:t -> t -> bool
  val merge : t -> t -> t
end

(** Describes how to compute facts for each kind of node in a graph
    incrementally. *)
module type RULE = sig
  (** See {!FACT}. *)
  type fact

  val first : fact -> Node.first -> fact
  (** [first fact n] is a fact for the entry into the first node in a block,
      [n], given the currently-known fact for the entry into the next node. *)

  val middle : fact -> Node.middle -> fact
  (** [middle fact n] is a fact for the entry into an assembly node in a block,
      [n], given the currently-known fact for the entry into the next node. *)

  val last : (block -> fact) -> graph -> Node.last -> fact
  (** [last q g n] is a fact for the entry into the last node in the graph [g],
      [n], given a function which queries for the currently-known fact at the
      entry-point of an arbitrary block ([q]). *)
end

(** Generic backward analysis on flow graphs. *)
module type ANALYSIS = sig
  (** The result of analyzing a graph. *)
  type t

  (** See {!FACT}. *)
  type fact

  val run : graph -> t
  (** The following properties must hold of the graph prior to running an
      analysis:

      - All labels referenced in branches must be defined
      - No labels may be defined multiple times

      If they don't, the behaviour is unspecified. *)

  (** {1 Querying blocks}

      In the functions below, the block being queried must originate from the
      same graph that was analyzed. If it doesn't, the behaviour is unspecified. *)

  val in_block : block -> t -> fact
  (** [in_block block t] is the computed fact at the entry-point of [block]. *)

  val out_block : block -> t -> fact
  (** [out_block block t] is the computed fact leaving [block] (i.e., the
      exit-point). *)

  val in_nodes : block -> t -> (node * fact) iter
  (** [in_nodes block t] iterates over the nodes of [block] and the computed
      fact at the entry-point of each. *)

  val out_nodes : block -> t -> (node * fact) iter
  (** [out_nodes block t] iterates over the nodes of [block] and the computed
      fact at the exit-point of each. *)
end

(** Analyze flow graphs for generic combinations of facts and rules. *)
module Analysis (F : FACT) (R : RULE with type fact = F.t) :
  ANALYSIS with type fact = F.t

(** {1 Liveness analysis} *)

(** Compute liveness information for all registers in a flow-graph.

    Liveness information is critical for register allocation. *)
module Liveness (R : sig
  val live_at_exit : box iter
end) : ANALYSIS with type fact = Box.Set.t
