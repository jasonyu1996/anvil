open ParamEnv
open Lang

let rec concretise_dtype int_env type_env (dtype : data_type) : data_type =
  match dtype with
  | `Array (v, n) ->
    `Array (concretise_dtype int_env type_env v, concretise int_env n |> Option.get)
  | `Named ident ->
    let t = Param ident in
    (
      match concretise_and_get type_env t with
      | Some v -> v
      | None -> dtype
    )
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
    (
      match proc.body with
      | Extern _ -> proc
      | Native body ->
        let regs = List.map
          (fun r -> {r with dtype = concretise_dtype int_param_env type_param_env r.dtype})
          body.regs in
        let spawns = List.map
          (fun sp ->
            let compile_params = List.map
              (fun pv ->
                match pv with
                | TypeParamValue (`Named ident) ->
                  (
                    (* TODO: hacky *)
                    let int_v = concretise_and_get int_param_env (Param ident)
                    and type_v = concretise_and_get type_param_env (Param ident) in
                    match int_v, type_v with
                    | Some n, None -> IntParamValue n
                    | None, Some t -> TypeParamValue t
                    | None, None -> pv
                    | _ -> raise (Except.TypeError "Ambigous parameters!")
                  )
                | _ -> pv
              ) sp.compile_params in
            {sp with compile_params}
          )
          body.spawns in
        {proc with body = Native {body with regs; spawns}}
    )
  )

