(** A fragment in an error message. *)
type error_message_fragment =
| Text of string (** text *)
| Codespan of string option * Lang.code_span (** [(filename, span)],
                                                 [filename] is optional *)

(** A helper function for obtaining a {!Codespan} without a file name. *)
let codespan_local span = Codespan (None, span)

(** A helper function for obtaining a {!Codespan} with a specified file name. *)
let codespan_in filename span = Codespan (Some filename, span)

type error_message = error_message_fragment list

exception UnimplementedError of error_message
exception TypeError of error_message
exception UnknownError of error_message


let unknown_error_default msg_text = UnknownError [Text msg_text]
