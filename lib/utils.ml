module StringMap = Map.Make(String)
type 'a string_map = 'a StringMap.t

module StringSet = Set.Make(String)
type string_set = StringSet.t

module IntSet = Set.Make(Int)
type int_set = IntSet.t

(** min n. 2^n >= v *)
let int_log2 (v : int) : int =
  let n = ref 1
  and c = ref 0 in
  while !n < v do
    n := !n * 2;
    c := !c + 1
  done;
  !c

let map_of_list (l : (string * 'a) list) : 'a string_map =
  let folder = fun m (s, d) -> StringMap.add s d m in
  List.fold_left folder StringMap.empty l


(* TODO: better error messages *)
let list_match_reorder (order : string list)
                       (data : (string * 'a) list) : 'a list option =
  if (List.length order) <> (List.length data) then
    None
  else
    let order_map = List.mapi (fun idx x -> (x, idx)) order |> List.to_seq |> StringMap.of_seq in
    try
      let data_ordered = List.map (fun (k, x) -> (StringMap.find k order_map, x)) data |>
          List.sort (fun (idx1, _) (idx2, _) -> idx1 - idx2) in
      let bad = ref false in
      List.iteri (fun idx (i, _) -> if idx <> i then bad := true) data_ordered;
      if !bad then
        None
      else
        Some (List.map snd data_ordered)
    with _ -> None
