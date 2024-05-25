type compile_config = {
  verbose: bool;
  input_filenames: string list;
}

let parse_args () : compile_config =
  let verbose = ref false
  and input_filenames = ref [] in
  let add_input_filename s =
    input_filenames := s::!input_filenames
  in
  Arg.parse
    [("-verbose", Arg.Set verbose, "Enable verbose output")]
    add_input_filename
    "anvil [-verbose] <file1> [<file2>] ...";
  {
    verbose = !verbose;
    input_filenames = !input_filenames;
  }


let debug_println (config : compile_config) (msg : string) =
  if config.verbose then
    Printf.eprintf "%s\n" msg
  else ()
