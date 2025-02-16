open Lang

exception LiteralConversionError of Except.error_message

let bit_literal_of_string (s : string) : literal =
  let spl = String.split_on_char 'b' s in
  let base =  List.nth spl 1 |>
  String.fold_left (fun hm c -> if c = '0' then `Z0::hm else `Z1::hm) [] in
  let len = String.split_on_char '\'' s |> List.hd |> int_of_string in
  Binary (len, base)

let dec_literal_of_string (s : string) : literal =
  let digit_of_char = fun (c : char) : digit ->
    match c with
    | '0' -> `Z0
    | '1' -> `Z1
    | '2' -> `Z2
    | '3' -> `Z3
    | '4' -> `Z4
    | '5' -> `Z5
    | '6' -> `Z6
    | '7' -> `Z7
    | '8' -> `Z8
    | '9' -> `Z9
    | _ -> raise (LiteralConversionError [Text "Bad character in decimal literal!"])
  in
  let spl = String.split_on_char 'd' s in
  let base =  List.nth spl 1 |>
  String.fold_left (fun hm c -> let d = digit_of_char c in d::hm) [] in
  let len = String.split_on_char '\'' s |> List.hd |> int_of_string in
  Decimal (len, base)


let hex_literal_of_string (s : string) : literal =
  let hexit_of_char = fun (c : char) : hexit ->
    match c with
    | '0' -> `Z0
    | '1' -> `Z1
    | '2' -> `Z2
    | '3' -> `Z3
    | '4' -> `Z4
    | '5' -> `Z5
    | '6' -> `Z6
    | '7' -> `Z7
    | '8' -> `Z8
    | '9' -> `Z9
    | 'a' -> `Za
    | 'b' -> `Zb
    | 'c' -> `Zc
    | 'd' -> `Zd
    | 'e' -> `Ze
    | 'f' -> `Zf
    | _ -> raise (LiteralConversionError [Text "Bad character in hexadecimal literal!"])
  in
  let spl = String.split_on_char 'd' s in
  let base =  List.nth spl 1 |>
  String.fold_left (fun hm c -> let d = hexit_of_char c in d::hm) [] in
  let len = String.split_on_char '\'' s |> List.hd |> int_of_string in
  Hexadecimal (len, base)
