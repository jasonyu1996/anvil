type compile_config = {
  verbose: bool;
  stdin: bool;
  disable_lt_checks : bool;
  weak_typecasts : bool;
  opt_level : int;
  output_filename : string option;
  just_check : bool;
  two_round_graph: bool;
  json_output: bool;
  input_filenames: string list;
}

let parse_args () : compile_config =
  let verbose = ref false
  and stdin = ref false
  and disable_lt_checks = ref false
  and strong_typecasts = ref false
  and opt_level = ref 2
  and two_round_graph = ref false
  and json_output = ref false
  and input_filenames = ref []
  and output_filename = ref None
  and just_check = ref false in

  let add_input_filename s =
    input_filenames := s::!input_filenames
  in
  Arg.parse
    [
      ("-stdin", Arg.Set stdin, "Read from standard input. If a filename is provided despite this flag, it is treated as the path of standard input data.");
      ("-verbose", Arg.Set verbose, "Enable verbose output");
      ("-disable-lt-checks", Arg.Set disable_lt_checks, "Disable lifetime/borrow-related checks");
      ("-strict-dtc", Arg.Set strong_typecasts, "Restrict Abstract Typecasts");
      ("-O", Arg.Set_int opt_level, "Set optimisation level: 0, 1, 2 (default)");
      ("-o", Arg.String (fun s -> output_filename := Some s), "Set output filename");
      ("-just-check", Arg.Set just_check, "Only typecheck and validate the input files");
      ("-two-round", Arg.Set two_round_graph, "Enable codegen of logic for two rounds");
      ("-json", Arg.Set json_output, "Output compilation results in JSON format")
    ]
    add_input_filename
    "anvil [-verbose] [-disable-lt-checks] [-O <opt-level>] [-two-round] [-json] [-strict-dtc] <file1> [<file2>] ...";
  {
    verbose = !verbose;
    stdin = !stdin;
    disable_lt_checks = !disable_lt_checks;
    weak_typecasts = not !strong_typecasts;
    opt_level = !opt_level;
    two_round_graph = !two_round_graph;
    json_output = !json_output;
    input_filenames = !input_filenames;
    output_filename = !output_filename;
    just_check = !just_check;
  }


let debug_println (config : compile_config) (msg : string) =
  if config.verbose then
    Printf.eprintf "%s\n" msg
  else ()
