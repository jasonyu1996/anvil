type 'a maybe_param =
  | Param of string
  | Concrete of 'a

module ParamEnvTbl = Hashtbl.Make(String)

type 'a param_env = 'a ParamEnvTbl.t

let create_env () = ParamEnvTbl.create 8

let add_value s v env =
  ParamEnvTbl.add env s v

let concretise env p =
  match p with
  | Concrete _ -> Some p
  | Param param_ident ->
    ParamEnvTbl.find_opt env param_ident
      |> Option.map (fun v -> Concrete v)

let get_opt p =
  match p with
  | Concrete v -> Some v
  | Param _ -> None

let concretise_and_get env p =
  let n = concretise env p in
  Option.bind n get_opt
