type compile_config = {
  verbose: bool;
  stdin: bool;
  disable_lt_checks : bool;
  opt_level : int;
  two_round_graph: bool;
  json_output: bool;
  input_filenames: string list;
}

let parse_args () : compile_config =
  let verbose = ref false
  and stdin = ref false
  and disable_lt_checks = ref false
  and opt_level = ref 2
  and two_round_graph = ref false
  and json_output = ref false
  and input_filenames = ref [] in
  let add_input_filename s =
    input_filenames := s::!input_filenames
  in
  Arg.parse
    [
      ("-stdin", Arg.Set stdin, "Read from standard input. If a filename is provided despite this flag, it is treated as the path of standard input data.");
      ("-verbose", Arg.Set verbose, "Enable verbose output");
      ("-disable-lt-checks", Arg.Set disable_lt_checks, "Disable lifetime/borrow-related checks");
      ("-O", Arg.Set_int opt_level, "Set optimisation level: 0, 1, 2 (default)");
      ("-two-round", Arg.Set two_round_graph, "Enable codegen of logic for two rounds");
      ("-json", Arg.Set json_output, "Output compilation results in JSON format")
    ]
    add_input_filename
    "anvil [-verbose] [-disable-lt-checks] [-O <opt-level>] [-two-round] [-json] <file1> [<file2>] ...";
  {
    verbose = !verbose;
    stdin = !stdin;
    disable_lt_checks = !disable_lt_checks;
    opt_level = !opt_level;
    two_round_graph = !two_round_graph;
    json_output = !json_output;
    input_filenames = !input_filenames;
  }


let debug_println (config : compile_config) (msg : string) =
  if config.verbose then
    Printf.eprintf "%s\n" msg
  else ()
