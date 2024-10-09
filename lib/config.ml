type compile_config = {
  verbose: bool;
  disable_lt_checks : bool;
  input_filenames: string list;
}

let parse_args () : compile_config =
  let verbose = ref false
  and disable_lt_checks = ref false
  and input_filenames = ref [] in
  let add_input_filename s =
    input_filenames := s::!input_filenames
  in
  Arg.parse
    [
      ("-verbose", Arg.Set verbose, "Enable verbose output");
      ("-disable-lt-checks", Arg.Set disable_lt_checks, "Disable lifetime/borrow-related checks")
    ]
    add_input_filename
    "anvil [-verbose] [-disable-lt-checks] <file1> [<file2>] ...";
  {
    verbose = !verbose;
    disable_lt_checks = !disable_lt_checks;
    input_filenames = !input_filenames;
  }


let debug_println (config : compile_config) (msg : string) =
  if config.verbose then
    Printf.eprintf "%s\n" msg
  else ()
