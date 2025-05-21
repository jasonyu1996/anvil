type compile_config = {
  verbose: bool;
  check_only : bool;
  disable_lt_checks : bool;
  opt_level : int;
  two_round_graph: bool;
  input_filenames: string list;
}

let parse_args () : compile_config =
  let verbose = ref false
  and check_only = ref false
  and disable_lt_checks = ref false
  and opt_level = ref 2
  and two_round_graph = ref false
  and input_filenames = ref [] in
  let add_input_filename s =
    input_filenames := s::!input_filenames
  in
  Arg.parse
    [
      ("-verbose", Arg.Set verbose, "Enable verbose output");
      ("-check-only", Arg.Set check_only, "Only perform type checks");
      ("-disable-lt-checks", Arg.Set disable_lt_checks, "Disable lifetime/borrow-related checks");
      ("-O", Arg.Set_int opt_level, "Set optimisation level: 0, 1, 2 (default)");
      ("-two-round", Arg.Set two_round_graph, "Enable codegen of logic for two rounds")
    ]
    add_input_filename
    "anvil [-verbose] [-disable-lt-checks] [-O <opt-level>] [-two-round] <file1> [<file2>] ...";
  {
    verbose = !verbose;
    check_only = !check_only;
    disable_lt_checks = !disable_lt_checks;
    opt_level = !opt_level;
    two_round_graph = !two_round_graph;
    input_filenames = !input_filenames;
  }


let debug_println (config : compile_config) (msg : string) =
  if config.verbose then
    Printf.eprintf "%s\n" msg
  else ()
