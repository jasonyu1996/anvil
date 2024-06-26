type t = {
  mutable cur_level : int;
  indent_unit : string;
  cur_indent : Buffer.t;
  tab : int;
  out : out_channel;
}

let create out tab =
  {
    cur_level = 0;
    indent_unit = Seq.repeat " " |> Seq.take tab |> List.of_seq |> String.concat "";
    cur_indent = Buffer.create 8;
    tab;
    out;
  }

let adjust_level lvl_delta printer =
  printer.cur_level <- printer.cur_level + lvl_delta;
  if lvl_delta > 0 then
    for _ = 1 to lvl_delta do
      Buffer.add_string printer.cur_indent printer.indent_unit
    done
  else if lvl_delta < 0 then
    Buffer.truncate printer.cur_indent (printer.tab * printer.cur_level)
  else ()

let print_line ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) printer s =
  adjust_level lvl_delta_pre printer;
  Printf.fprintf printer.out "%s%s\n" (Buffer.contents printer.cur_indent) s;
  adjust_level lvl_delta_post printer

let print_lines ?(lvl_delta_pre = 0) ?(lvl_delta_post = 0) printer ss =
  adjust_level lvl_delta_pre printer;
  (* first line no extra indent *)
  match ss with
  | [] -> ()
  | s::ss' ->
    print_line printer s;
    List.iter (fun s' -> print_line printer (printer.indent_unit ^ s')) ss'
  ;
  adjust_level lvl_delta_post printer
