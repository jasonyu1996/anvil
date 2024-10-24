exception CompileError of (string * Lang.code_span) option * string

let raise_compile_error file_name span msg =
  raise (CompileError (Some (file_name, span), msg))

let raise_compile_error_brief msg =
  raise (CompileError (None, msg))

let canonicalise_file_name file_origin file_name =
  if Filename.is_relative file_name then
    Filename.concat (Filename.dirname file_origin) file_name
  else
    file_name

let rec parse_recursive cunits parsed_files _config filename =
  if Utils.StringSet.mem filename !parsed_files |> not then (
    parsed_files := Utils.StringSet.add filename !parsed_files;
    let cunit =
      try
        In_channel.with_open_bin
          filename
          (fun in_channel ->
            let lexbuf = Lexing.from_channel in_channel in
            try Parser.cunit Lexer.read lexbuf
            with
              | Lexer.SyntaxError msg ->
                Printf.sprintf "Syntax error: %s" msg
                  |> raise_compile_error filename
                  (let open Lang in {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf})
              | _ ->
                raise_compile_error filename
                  (let open Lang in {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf})
                  "Syntax error"
          )
      with
        | Sys_error msg ->
          raise_compile_error_brief msg
    in
    cunits := (filename, cunit)::!cunits;
    List.iter (fun imp ->
      let open Lang in
      if not imp.is_extern then
        canonicalise_file_name filename imp.file_name
          |> parse_recursive cunits parsed_files _config
    ) cunit.imports
  )

let compile out config =
  let open Config in
  let toplevel_filename = List.hd config.input_filenames in
  let cunits = ref [] in
  parse_recursive cunits (ref Utils.StringSet.empty) config toplevel_filename;
  (* collect all channel class and type definitions *)
  let all_channel_classes = List.concat_map (fun (_, cunit) -> let open Lang in cunit.channel_classes) !cunits in
  let all_type_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.type_defs) !cunits in
  let all_procs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.procs) !cunits in
  let event_graphs =
    List.map (fun (filename, cunit) ->
      try
        GraphBuilder.build config
          (
            let open Lang in
            {cunit with channel_classes = all_channel_classes; type_defs = all_type_defs; _extern_procs = all_procs}
          )
      with
      | EventGraph.LifetimeCheckError msg ->
        Printf.sprintf "Borrow checking failed: %s" msg
          |> raise_compile_error_brief
      | Except.TypeError msg ->
        Printf.sprintf "Type error: %s\n" msg
          |> raise_compile_error_brief
      | Except.UnimplementedError msg ->
        Printf.sprintf "Unimplemented error: %s\n" msg
          |> raise_compile_error_brief
      | EventGraph.EventGraphError (msg, span) ->
        Printf.sprintf "Event graph error (%s)" msg
          |> raise_compile_error filename span
      | Except.UnknownError msg ->
        Printf.sprintf "Unknown error (%s)" msg
          |> raise_compile_error_brief
    )
    !cunits
  in
  let all_event_graphs = List.concat_map
    (fun graph_collection -> let open EventGraph in graph_collection.event_graphs) event_graphs in
  (* generate preamble *)
  Codegen.generate_preamble out;
  (* pull code from imported external files *)
  let visited_extern_files = ref Utils.StringSet.empty in
  let open Lang in
  List.iter (fun (file_name, cunit) ->
    List.iter (fun {is_extern; file_name = imp_file_name} ->
      if is_extern then
        let imp_file_name_canonical = canonicalise_file_name file_name imp_file_name in
        if Utils.StringSet.mem imp_file_name_canonical !visited_extern_files |> not then (
          visited_extern_files := Utils.StringSet.add imp_file_name_canonical !visited_extern_files;
          try Codegen.generate_extern_import out imp_file_name_canonical
          with Sys_error msg -> raise_compile_error_brief msg
        )
    ) cunit.imports
  ) !cunits;
  (* generate the code from event graphs *)
  List.iter (fun graphs ->
    Codegen.generate out config {graphs with EventGraph.external_event_graphs = all_event_graphs}) event_graphs

