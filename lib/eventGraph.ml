open Lang

type event = delay
type wire = WireCollection.wire
type wire_collection = WireCollection.t

(* module Typing = struct
  type judgment = {
    v : identifier;
    w : wire option;
  }
end *)

type event_graph = {
  mutable events: event list;
  mutable wires: wire_collection;
  typedefs: TypedefMap.t;
}

(* let rec visit_expr (_eg : event_graph) (_e : expr) = () *)

let build (cunit : compilation_unit) =
  let typedefs = TypedefMap.of_list cunit.type_defs in
  {
    events = [];
    wires = [];
    typedefs
  }

