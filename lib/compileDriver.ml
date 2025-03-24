exception CompileError of Except.error_message

let raise_compile_error file_name msg =
  let open Except in
  let msg = match file_name with
  | Some file_name -> List.map (fun frag ->
      match frag with
      | Codespan (None, codespan) -> Codespan (Some file_name, codespan)
      | _ -> frag
    ) msg
  | None -> msg
  in
  raise (CompileError msg)

let canonicalise_file_name file_origin file_name =
  if Filename.is_relative file_name then
    Filename.concat (Filename.dirname file_origin) file_name
  else
    file_name

let rec parse_recursive cunits parsed_files config filename =
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
                let open Except in
                [
                  (Text "Syntax error:");
                  (Text msg);
                  (Codespan (Some filename, {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf}))
                ] |> raise_compile_error (Some filename)
              | _ ->
                let open Except in
                [
                  (Text "Syntax error");
                  (Codespan (Some filename, {st = Lexing.lexeme_start_p lexbuf; ed = Lexing.lexeme_end_p lexbuf}))
                ] |> raise_compile_error (Some filename)
          )
      with
        | Sys_error msg ->
          raise_compile_error (Some filename) [Except.Text msg]
    in
    let cunit = {cunit with cunit_file_name = Some filename} in
    (
      try
        GraphBuilder.syntax_tree_precheck config cunit
      with
        | Except.TypeError msg -> raise_compile_error (Some filename) msg
    );
    cunits := (filename, cunit)::!cunits;
    List.iter (fun imp ->
      let open Lang in
      if not imp.is_extern then
        canonicalise_file_name filename imp.file_name
          |> parse_recursive cunits parsed_files config
    ) cunit.imports
  )

let compile out config =
  let open Config in
  let toplevel_filename = List.hd config.input_filenames in
  let cunits = ref [] in
  (
    try parse_recursive cunits (ref Utils.StringSet.empty) config toplevel_filename
    with
      | Except.TypeError msg ->
        (Except.Text "Type error:")::msg
          |> raise_compile_error None
  );
  (* collect all channel class and type definitions *)
  let all_channel_classes = List.concat_map (fun (_, cunit) -> let open Lang in cunit.channel_classes) !cunits in
  let all_type_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.type_defs) !cunits in
  let all_procs = List.concat_map (fun (file_name, cunit) -> let open Lang in List.map (fun p -> (file_name, p)) cunit.procs) !cunits in
  let all_func_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.func_defs) !cunits in
  let all_macro_defs = List.concat_map (fun (_, cunit) -> let open Lang in cunit.macro_defs) !cunits in
  let proc_map = List.map (fun (file_name, proc) -> (let open Lang in (proc:proc_def).name, (proc, file_name))) all_procs
    |> Utils.StringMap.of_list in
  let sched = BuildScheduler.create () in
  (* add processes that are concrete *)
  List.iter (fun (file_name, proc) ->
    let open Lang in
    if proc.params = [] then
      let _ = BuildScheduler.add_proc_task sched file_name Lang.code_span_dummy proc.name [] in ()
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
        let proc_file_opt = Utils.StringMap.find_opt
          (let open BuildScheduler in task.proc_name)
          proc_map in
        let open Except in
        match proc_file_opt with
        | None ->
          let msg_text = Printf.sprintf "Process '%s' not found!" task.proc_name in
          raise_compile_error None
            [
              Text msg_text;
              codespan_in task.file_name task.codespan
            ]
        | Some (proc, file_name) ->
          let cunit = let open Lang in
            (* hacky *)
            {cunit_file_name = Some file_name;
            channel_classes = all_channel_classes; type_defs = all_type_defs;
            procs = [proc]; imports = []; _extern_procs = [];
            func_defs = all_func_defs;
            macro_defs = all_macro_defs} in
          let graph_collection =
            match GraphBuilder.build config sched task.module_name task.param_values cunit
            with
            | res -> res
            | exception exc ->
              (
                let msg =
                  match exc with
                  | EventGraph.LifetimeCheckError msg ->
                    (Text "Borrow checking failed:")::msg
                  | Except.TypeError msg ->
                    (Text "Type error:")::msg
                  | Except.UnimplementedError msg ->
                    (Text "Unimplemented error:")::msg
                  | EventGraph.EventGraphError msg ->
                    (Text "Event graph error:")::msg
                  | Except.UnknownError msg ->
                    (Text "Unknown error:")::msg
                  | _ -> raise exc
                in
                raise_compile_error (Some file_name) msg
              )
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
          with Sys_error msg -> raise_compile_error (Some file_name) [Except.Text msg]
        )
    ) cunit.imports
  ) !cunits;
  (* generate the code from event graphs *)
  let all_collections = Queue.to_seq graph_collection_queue |> List.of_seq in
  let all_event_graphs = List.concat_map (fun collection -> let open EventGraph in collection.event_graphs) all_collections in
  List.iter (fun graphs ->
    Codegen.generate out config
     {graphs with EventGraph.external_event_graphs = all_event_graphs}) all_collections

