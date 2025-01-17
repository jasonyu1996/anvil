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
  let all_procs = List.concat_map (fun (file_name, cunit) -> let open Lang in List.map (fun p -> (file_name, p)) cunit.procs) !cunits in
  let all_func_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.func_defs) !cunits in
  let all_enum_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.enum_defs) !cunits in
  let all_macro_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.macro_defs) !cunits in
  let proc_map = List.map (fun (file_name, proc) -> (let open Lang in (proc:proc_def).name, (proc, file_name))) all_procs
    |> Utils.StringMap.of_list in
  let sched = BuildScheduler.create () in
  (* add processes that are concrete *)
  List.iter (fun (_, proc) ->
    let open Lang in
    if proc.params = [] then
      let _ = BuildScheduler.add_proc_task sched proc.name [] in ()
  ) all_procs;
  let modules_visited = ref Utils.StringSet.empty in
  let event_graph_complete = ref false in
  let graph_collection_queue = Queue.create () in
  while not !event_graph_complete do
    match BuildScheduler.next sched with
    | None -> event_graph_complete := true
    | Some task -> (
      if Utils.StringSet.mem task.module_name !modules_visited |> not then (
        modules_visited := Utils.StringSet.add task.module_name !modules_visited;
        let proc, file_name = Utils.StringMap.find
          (let open BuildScheduler in task.proc_name)
          proc_map in
        let cunit = let open Lang in
          (* hacky *)
          {channel_classes = all_channel_classes; type_defs = all_type_defs;
           procs = [proc]; imports = []; _extern_procs = []; 
           func_defs = all_func_defs; enum_defs = all_enum_defs; 
           macro_defs = all_macro_defs} in
        let graph_collection =
          try GraphBuilder.build config sched task.module_name task.param_values cunit
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
              |> raise_compile_error file_name span
          | Except.UnknownError msg ->
            Printf.sprintf "Unknown error (%s)" msg
              |> raise_compile_error_brief
        in
        Queue.add graph_collection graph_collection_queue
      )
    )
  done;
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
  let all_collections = Queue.to_seq graph_collection_queue |> List.of_seq in
  let all_event_graphs = List.concat_map (fun collection -> let open EventGraph in collection.event_graphs) all_collections in
  List.iter (fun graphs ->
    Codegen.generate out config
     {graphs with EventGraph.external_event_graphs = all_event_graphs}) all_collections

