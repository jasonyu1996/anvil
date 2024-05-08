module StringMap = Map.Make(String)
type 'a string_map = 'a StringMap.t


let rec int_log2 (v : int) : int =
  if v < 1 then
    0
  else
    1 + (int_log2 (v / 2))

let map_of_list (l : (string * 'a) list) : 'a string_map =
  let folder = fun m (s, d) -> StringMap.add s d m in
  List.fold_left folder StringMap.empty l
