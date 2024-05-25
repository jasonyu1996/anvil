module StringMap = Map.Make(String)
type 'a string_map = 'a StringMap.t

module StringSet = Set.Make(String)
type string_set = StringSet.t

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
