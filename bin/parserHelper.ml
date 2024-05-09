let bit_literal_of_string (s : string) : Lang.bit list =
  let spl = String.split_on_char 'b' s in
  let base =  List.nth spl 1 |>
  String.fold_left (fun hm c -> if c = '0' then Lang.Zero::hm else Lang.One::hm) [] in
  let len = String.split_on_char '\'' s |> List.hd |> int_of_string in
  if List.length base < len then
    base @ (List.init (len - (List.length base)) (fun _ -> Lang.Zero)) else
    base
