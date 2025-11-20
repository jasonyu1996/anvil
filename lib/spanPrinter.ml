(* Library for printing and rendering code spans as strings. *)

(** Get a span of code as a formatted string that may be printed or displayed.
    [trunc] specifies the truncation: [0] for no truncation, positive for truncating at the start,
    and negative for truncating to the end (keeping [trunc] lines).
 *)
let string_of_code_span ?(indent = 2) ?(trunc = 0) filename span =
  let repeat_s s times = Seq.repeat s |> Seq.take times |> List.of_seq |> String.concat "" in
  let open Lang in
  let indent_s = repeat_s " " indent in
  let buf = Buffer.create 256 in
  let result =
    try
      In_channel.with_open_text filename
        (
          fun in_chn ->
            let char_offset = span.st.pos_bol in
            In_channel.seek in_chn (Int64.of_int char_offset);
            let (start_lnum, end_lnum) =
              if trunc = 0 then (span.st.pos_lnum, span.ed.pos_lnum)
              else if trunc > 0 then (span.st.pos_lnum, Int.min (span.st.pos_lnum + trunc - 1) span.ed.pos_lnum)
              else (* trunc < 0 *) (Int.max (span.ed.pos_lnum + trunc + 1) span.st.pos_lnum, span.ed.pos_lnum)
            in
            let print_truncate_msg n = Printf.bprintf buf "<%d line(s) truncated>\n" n in
            if start_lnum <> span.st.pos_lnum then
              print_truncate_msg (start_lnum - span.st.pos_lnum);
            for line_no = span.st.pos_lnum to span.ed.pos_lnum do
              let line_s = In_channel.input_line in_chn |> Option.get in
              if line_no >= start_lnum && line_no <= end_lnum then (
                let so = if line_no = span.st.pos_lnum then span.st.pos_cnum - span.st.pos_bol else 0
                and eo = if line_no = span.ed.pos_lnum then span.ed.pos_cnum - span.ed.pos_bol else String.length line_s in
                Printf.bprintf buf "%s%5d| %s\n" indent_s line_no line_s;
                let blank_s = repeat_s " " so
                and underline_s = repeat_s "^" (eo - so) in
                Printf.bprintf buf "%s     | %s%s\n" indent_s blank_s underline_s
              )
            done;
            if end_lnum <> span.ed.pos_lnum then
              print_truncate_msg (span.ed.pos_lnum - end_lnum)
        );
      Some (Buffer.contents buf)
    with _ -> None
  in
  result

(** Prints out a span of code. See: [string_of_code_span]. *)
let print_code_span ?(indent = 2) ?(trunc = 0) out filename span =
  match string_of_code_span ~indent ~trunc filename span with
  | Some s -> output_string out s
  | None -> ()
