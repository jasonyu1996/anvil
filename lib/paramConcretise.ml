open ParamEnv
open Lang

let rec concretise_params int_env type_env params =
  List.map (fun p ->
    match p with
    | IntParamValue _ -> p
    | TypeParamValue dtype ->
      (
        match dtype with
        | `Named (ident, []) ->
          (* this is just an identifier, might also be an int param *)
          (
            match concretise_and_get int_env (Param ident) with
            | Some n -> IntParamValue n
            | None -> TypeParamValue (concretise_dtype_params int_env type_env dtype)
          )
        | _ -> TypeParamValue (concretise_dtype_params int_env type_env dtype)
      )
  ) params
and concretise_dtype_params int_env type_env (dtype : data_type) : data_type =
  match dtype with
  | `Array (v, n) ->
    `Array (concretise_dtype_params int_env type_env v, concretise int_env n |> Option.get)
  | `Named (ident, params) ->
    if params = [] then
      let t = Param ident in
      (
        match concretise_and_get type_env t with
        | Some v -> v
        | None -> dtype
      )
    else
      let params' = concretise_params int_env type_env params in
      `Named (ident, params')
  | `Record fields ->
    let fields' = List.map (fun (field_ident, field_dtype) ->
                        (field_ident, concretise_dtype_params int_env type_env field_dtype))
                        fields
    in
    `Record fields'
  | `Variant variants ->
    let variants' = List.map
            (fun (var_ident, var_dtype_opt) ->
              (
                var_ident,
                Option.map (concretise_dtype_params int_env type_env) var_dtype_opt
              ))
            variants
            in
    `Variant variants'
  | `Tuple elems_dtypes ->
    let elems_dtypes' = List.map (concretise_dtype_params int_env type_env) elems_dtypes in
    `Tuple elems_dtypes'
  | _ -> dtype

let build_param_envs param_values params =
  let int_env = create_env ()
  and type_env = create_env () in
  let vals_s = List.to_seq param_values
  and params_s = List.to_seq params in
  Seq.zip vals_s params_s
    |> Seq.iter
      (fun (v, p) ->
        match v, p.param_ty with
        | IntParamValue n, IntParam ->
          add_value p.param_name n int_env
        | TypeParamValue t, TypeParam ->
          add_value p.param_name t type_env
        | _ -> raise (Except.TypeError "Invalid parameter!")
      );
  (int_env, type_env)

let concretise_proc param_values proc =
  if proc.params = [] then
    proc
  else (
    let (int_param_env, type_param_env) = build_param_envs param_values proc.params in
    let args = List.map
      (fun (a : Lang.endpoint_def) ->
        let params = concretise_params int_param_env type_param_env a.channel_params in
        {a with channel_params = params}
      ) proc.args in
    (
      match proc.body with
      | Extern _ -> proc
      | Native body ->
        let regs = List.map
          (fun r -> {r with dtype = concretise_dtype_params int_param_env type_param_env r.dtype})
          body.regs in
        let spawns = List.map
          (fun sp ->
            let compile_params = concretise_params int_param_env type_param_env sp.compile_params in
            {sp with compile_params}
          )
          body.spawns in
        let channels = List.map
          (fun ch ->
            let params = concretise_params int_param_env type_param_env ch.channel_params in
            {ch with channel_params = params}
          )
          body.channels in
        {proc with args; body = Native {body with regs; spawns; channels}}
    )
  )

let concretise_dtype params param_values =
  let (int_param_env, type_param_env) = build_param_envs param_values params in
  concretise_dtype_params int_param_env type_param_env

let concretise_message params param_values (msg : Lang.message_def) =
  let (int_param_env, type_param_env) = build_param_envs param_values params in
  let sig_types = List.map
    (fun (stype : Lang.sig_type_chan_local) ->
      {stype with dtype = concretise_dtype_params int_param_env type_param_env stype.dtype}
    )
    msg.sig_types
  in
  {msg with sig_types}
