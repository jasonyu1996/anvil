(** Print out a span of code *)
let print_code_span ?(indent = 2) out filename span =
  let repeat_s s times = Seq.repeat s |> Seq.take times |> List.of_seq |> String.concat "" in
  let open Anvil.Lang in
  let indent_s = repeat_s " " indent in
  try
    In_channel.with_open_text filename
      (
        fun in_chn ->
          let char_offset = span.st.pos_bol in
          In_channel.seek in_chn (Int64.of_int char_offset);
          for line_no = span.st.pos_lnum to span.ed.pos_lnum do
            let line_s = In_channel.input_line in_chn |> Option.get in
            let so = if line_no = span.st.pos_lnum then span.st.pos_cnum - span.st.pos_bol else 0
            and eo = if line_no = span.ed.pos_lnum then span.ed.pos_cnum - span.ed.pos_bol else String.length line_s in
            Printf.fprintf out "%s%5d| %s\n" indent_s line_no line_s;
            let blank_s = repeat_s " " so
            and underline_s = repeat_s "^" (eo - so) in
            Printf.fprintf out "%s     | %s%s\n" indent_s blank_s underline_s
          done
      )
  with
  | _ -> ()
