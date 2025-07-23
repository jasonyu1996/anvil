%token EOF
%token LEFT_BRACE           (* { *)
%token RIGHT_BRACE          (* } *)
%token LEFT_BRACKET         (* [ *)
%token RIGHT_BRACKET        (* ] *)
%token LEFT_PAREN           (* ( *)
%token RIGHT_PAREN          (* ) *)
%token COMMA                (* , *)
%token SEMICOLON            (* ; *)
%token COLON                (* : *)
%token DOUBLE_COLON         (* :: *)
%token KEYWORD_FUNCTION    (* func *)
%token KEYWORD_CALL                (* call *)
%token SHARP                (* # *)
%token EQUAL                (* = *)
%token COLON_EQ             (* := *)
%token LEFT_ABRACK          (* < *)
%token RIGHT_ABRACK         (* > *)
%token LEFT_ABRACK_EQ       (* <= *)
%token RIGHT_ABRACK_EQ      (* >= *)
%token DOUBLE_LEFT_ABRACK   (* << *)
%token DOUBLE_RIGHT_ABRACK  (* << *)
%token KEYWORD_CONST        (* const *)
%token KEYWORD_READY        (* ready *)
%token KEYWORD_PROBE        (* probe *)
%token DOUBLE_EQ            (* == *)
%token EQ_GT                (* => *)
%token DOUBLE_GT            (* >> *)
%token EXCL_EQ              (* != *)
%token ASTERISK             (* * *)
%token PLUS                 (* + *)
%token MINUS                (* - *)
%token DOUBLE_MINUS         (* -- *)
%token XOR                  (* ^ *)
%token AND                  (* & *)
%token DOUBLE_AND           (* && *)
%token OR                   (* | *)
%token DOUBLE_OR            (* || *)
%token AT                   (* @ *)
%token TILDE                (* ~ *)
%token PERIOD               (* . *)
%token KEYWORD_LOOP         (* loop *)
%token KEYWORD_PROC         (* proc *)
%token KEYWORD_CHAN         (* chan *)
%token KEYWORD_LEFT         (* left *)
%token KEYWORD_RIGHT        (* right *)
%token KEYWORD_PUT          (* put *)
%token KEYWORD_LOGIC        (* logic *)
%token KEYWORD_FOREIGN      (* foreign *)
%token KEYWORD_GENERATE     (* generate *)
%token KEYWORD_GENERATE_SEQ     (* generate *)
%token KEYWORD_IF           (* if *)
%token KEYWORD_ELSE         (* else *)
%token KEYWORD_LET          (* let *)
%token KEYWORD_SEND         (* send *)
%token KEYWORD_RECV         (* recv *)
%token KEYWORD_IN          (* in *)
%token KEYWORD_ETERNAL      (* eternal *)
%token KEYWORD_TYPE         (* type *)
%token KEYWORD_SET          (* set *)
%token KEYWORD_MATCH        (* match *)
%token KEYWORD_SYNC         (* sync *)
%token KEYWORD_DYN          (* dyn *)
%token KEYWORD_CYCLE        (* cycle *)
%token KEYWORD_REG          (* reg *)
%token KEYWORD_SPAWN        (* spawn *)
%token KEYWORD_DPRINT       (* dprint *)
%token KEYWORD_DFINISH      (* dfinish *)
%token KEYWORD_IMPORT       (* import *)
%token KEYWORD_EXTERN       (* extern *)
%token KEYWORD_INT          (* int *)
%token KEYWORD_RECURSIVE    (* recursive *)
%token KEYWORD_RECURSE      (* recurse *)
%token KEYWORD_STRUCT       (* struct *)
%token KEYWORD_ENUM         (* enum *)
%token KEYWORD_WITH         (* with *)
%token KEYWORD_TRY          (* try *)
%token <int>INT             (* int literal *)
%token <string>IDENT        (* identifier *)
%token <string>BIT_LITERAL  (* bit literal *)
%token <string>DEC_LITERAL  (* decimal literal *)
%token <string>HEX_LITERAL  (* hexadecimal literal *)
%token <string>STR_LITERAL
%token KEYWORD_SHARED       (* shared *)
%token KEYWORD_ASSIGNED     (* assigned *)
%token KEYWORD_BY           (* by *)
%right LEFT_ABRACK RIGHT_ABRACK LEFT_ABRACK_EQ RIGHT_ABRACK_EQ
%right DOUBLE_GT SEMICOLON
%right KEYWORD_LET KEYWORD_SET KEYWORD_PUT
%left DOUBLE_AND DOUBLE_OR
%left EXCL_EQ DOUBLE_EQ
%left XOR AND OR PLUS MINUS
%left DOUBLE_LEFT_ABRACK DOUBLE_RIGHT_ABRACK
%left PERIOD
%left LEFT_BRACKET RIGHT_BRACKET
%nonassoc TILDE UMINUS UAND UOR KEYWORD_IN
%start <Lang.compilation_unit> cunit
%%

%public %inline node(X):
  x=X { Lang.ast_node_of_data $startpos $endpos x }

cunit:
| EOF
  { Lang.cunit_empty }
| p = proc_def; c = cunit
  { Lang.cunit_add_proc c p }
| macro  = macro_def;c = cunit
  { Lang.cunit_add_macro_def c macro }
| func_def = function_def; c = cunit
  { Lang.cunit_add_func_def c func_def }
| ty = type_def; c = cunit
  { Lang.cunit_add_type_def c ty }
| cc = channel_class_def; c = cunit
  { Lang.cunit_add_channel_class c cc }
| im = import_directive; c = cunit
  { Lang.cunit_add_import c im }
;

import_directive:
| KEYWORD_IMPORT; file_name = STR_LITERAL
  {
    let open Lang in {file_name; is_extern = false}
  }
| KEYWORD_EXTERN; KEYWORD_IMPORT; file_name = STR_LITERAL
  {
    let open Lang in {file_name; is_extern = true}
  }
;


proc_def:
| KEYWORD_PROC; ident = IDENT;
  LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  LEFT_BRACE; body = proc_def_body; RIGHT_BRACE
  {
    {
      name = ident;
      args = args;
      params = [];
      body = let open Lang in Native body
    } : Lang.proc_def
  }
| KEYWORD_PROC; ident = IDENT; LEFT_ABRACK; params = separated_list(COMMA, param_def); RIGHT_ABRACK;
  LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  LEFT_BRACE; body = proc_def_body; RIGHT_BRACE
  {
    {
      name = ident;
      args = args;
      params = params;
      body = let open Lang in Native body
    } : Lang.proc_def
  }
| KEYWORD_PROC; ident = IDENT; LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  KEYWORD_EXTERN; LEFT_PAREN; mod_name = STR_LITERAL; RIGHT_PAREN;
  LEFT_BRACE; body = proc_def_body_extern; RIGHT_BRACE
  {
    {
      name = ident;
      args = args;
      params = [];
      body = let open Lang in Extern (mod_name, body)
    } : Lang.proc_def
  }
;

proc_def_body:
|
  {
    let open Lang in {
      channels = [];
      spawns = [];
      regs = [];
      threads = [];
      shared_vars = [];
    }
  }
| KEYWORD_LOOP; reset_by = message_specifier?; LEFT_BRACE; thread_prog = node(expr); RIGHT_BRACE; body=proc_def_body //For thread definitions
  {
    let open Lang in
    let thread_prog = {thread_prog with d = Wait (thread_prog, dummy_ast_node_of_data Recurse)} in
    {body with threads = (thread_prog, reset_by)::(body.threads) }
  }
| KEYWORD_RECURSIVE; LEFT_BRACE; thread_prog = node(expr); RIGHT_BRACE; body = proc_def_body
  {
    let open Lang in
    { body with threads = (thread_prog, None)::(body.threads) }
  }
| KEYWORD_CHAN; chan_def = node(channel_def); SEMICOLON; body = proc_def_body // For Channel Invocation and interface aquisition
  {
    let open Lang in {body with channels = chan_def::(body.channels)}
  }
| KEYWORD_REG; reg_def = node(reg_def); SEMICOLON; body = proc_def_body // For reg definition
  {
    let open Lang in {body with regs = reg_def::(body.regs)}
  }
| KEYWORD_SPAWN; spawn_def = node(spawn); SEMICOLON; body = proc_def_body // For instantiating processes
  {
    let open Lang in {body with spawns = spawn_def::(body.spawns)}
  }
| shared_var = node(shared_var_def); SEMICOLON; body = proc_def_body
  {
    let open Lang in {body with shared_vars = shared_var :: body.shared_vars}
  }
;

proc_def_body_extern:
| { let open Lang in {named_ports = []; msg_ports = []} }
| name = IDENT; LEFT_PAREN; s = STR_LITERAL; RIGHT_PAREN; SEMICOLON; body = proc_def_body_extern
  {
    let open Lang in {body with named_ports = (name, s)::body.named_ports}
  }
| msg = message_specifier; LEFT_PAREN; s0 = STR_LITERAL?; COLON;
  s1 = STR_LITERAL?; COLON; s2 = STR_LITERAL?; RIGHT_PAREN; SEMICOLON; body = proc_def_body_extern
  {
    let open Lang in {body with msg_ports = (msg, s0, s1, s2)::body.msg_ports}
  }
;

param_def:
| name = IDENT; COLON; KEYWORD_INT
  {
    let open Lang in {param_name = name; param_ty = IntParam}
  }
| name = IDENT; COLON; KEYWORD_TYPE
  {
    let open Lang in {param_name = name; param_ty = TypeParam}
  }
;

param_list:
  LEFT_ABRACK; params = separated_list(COMMA, param_def); RIGHT_ABRACK;
  { params }
;

// For defining struct for messages and message types
type_def:
| KEYWORD_TYPE; name = IDENT; params = param_list?;
  EQUAL; dtype = data_type; SEMICOLON
  {
    { name = name; body = dtype; params = Option.value ~default:[] params } : Lang.type_def
  }
| KEYWORD_ENUM; name = IDENT; params = param_list?;
  LEFT_BRACE; variants = separated_nonempty_list(COMMA, variant_def); RIGHT_BRACE
  {
    { name = name; body = `Variant variants; params = Option.value ~default:[] params } : Lang.type_def
  }
| KEYWORD_STRUCT; name = IDENT; params = param_list?;
  LEFT_BRACE; fields = separated_nonempty_list(COMMA, field_def); RIGHT_BRACE
  {
    { name = name; body = `Record (List.rev fields); params = Option.value ~default:[] params } : Lang.type_def
  }
;


variant_def:
| name = IDENT; dtype_opt = variant_type_spec?
  { (name, dtype_opt) }
;

variant_type_spec:
| LEFT_PAREN; dtype = data_type; RIGHT_PAREN
  { dtype }
| LEFT_PAREN; dtype = data_type; COMMA; more_dtypes = separated_nonempty_list(COMMA, data_type); RIGHT_PAREN
  { `Tuple (dtype::more_dtypes) }
;

field_def:
| name = IDENT; COLON; dtype = data_type
  { (name, dtype) }
;

// For channel declaration for each pair of process
channel_class_def:
| KEYWORD_CHAN; ident = IDENT; LEFT_BRACE; messages = separated_list(COMMA, message_def); RIGHT_BRACE
  {
    {
      name = ident;
      messages = messages;
      params = [];
      span = {st = $startpos; ed = $endpos}
    } : Lang.channel_class_def
  }
| KEYWORD_CHAN; ident = IDENT; LEFT_ABRACK; params = separated_list(COMMA, param_def); RIGHT_ABRACK;
  LEFT_BRACE; messages = separated_list(COMMA, message_def); RIGHT_BRACE
  {
    {
      name = ident;
      messages = messages;
      params = params;
      span = {st = $startpos; ed = $endpos}
    } : Lang.channel_class_def
  }
;
//For passing the list of channels that are nessacary to communicate with the instantiation of the process
proc_def_arg_list:
  l = separated_list(COMMA, node(proc_def_arg))
  { l }
;
// To Do: Can be renamed to inherited channel or something
proc_def_arg:
  foreign = foreign_tag; ident = IDENT; COLON; chan_dir = channel_direction; chan_class = channel_class_concrete
  {
    {
      name = ident;
      channel_class = fst chan_class;
      channel_params = snd chan_class;
      dir = chan_dir;
      foreign = foreign; (* These are ignored for now.
                        Instead we just look at which endpoints are used in spawn *)
      opp = None;
    } : Lang.endpoint_def
  }
;

// To Ask: What does this do
foreign_tag:
| { false }
| KEYWORD_FOREIGN { true }
;
// Specifies the dirn of comm (inp or out)
channel_direction:
| KEYWORD_LEFT
  { Lang.Left }
| KEYWORD_RIGHT
  { Lang.Right }
;
// channels are instantantiated with endpoint aquisition by native channel inside the proc
channel_def:
| left_foreign = foreign_tag; left_endpoint = IDENT; DOUBLE_MINUS;
  right_foreign = foreign_tag; right_endpoint = IDENT; COLON;
  chan_class = channel_class_concrete
  {
    let visibility = match left_foreign, right_foreign with
      | true, true -> Lang.BothForeign
      | false, true -> Lang.RightForeign
      | _ -> Lang.LeftForeign
    in
    {
      channel_class = fst chan_class;
      channel_params = snd chan_class;
      endpoint_left = left_endpoint;
      endpoint_right = right_endpoint;
      visibility = visibility;
    } : Lang.channel_def
  }
;

channel_class_concrete:
| chan_class_name = IDENT
  { (chan_class_name, []) }
| chan_class_name = IDENT; LEFT_ABRACK; params = separated_list(COMMA, param_value); RIGHT_ABRACK
  { (chan_class_name, params) }
;

// Instantiating the proc for comm
spawn:
| proc = IDENT; LEFT_PAREN; params = separated_list(COMMA, IDENT); RIGHT_PAREN
  {
    {
      proc = proc;
      params = params;
      compile_params = [];
    } : Lang.spawn_def
  }
| proc = IDENT; LEFT_ABRACK; compile_params = separated_list(COMMA, param_value); RIGHT_ABRACK;
  LEFT_PAREN; params = separated_list(COMMA, IDENT); RIGHT_PAREN
  {
    {
      proc = proc;
      params = params;
      compile_params = compile_params;
    } : Lang.spawn_def
  }
;

param_value:
| n = INT
  { Lang.IntParamValue n }
| t = data_type
  { Lang.TypeParamValue t }
;

//register declaration, To Do: Add support for declaration as well as init
reg_def:
  ident = IDENT; COLON; dtype = data_type
  {
    {
      name = ident;
      d_type = dtype;
      init = None;
    } : Lang.reg_def
  }
;
//term definitions for expressions
term:
| literal_str = BIT_LITERAL
  { Lang.Literal (ParserHelper.bit_literal_of_string literal_str) }
| literal_str = DEC_LITERAL
  { Lang.Literal (ParserHelper.dec_literal_of_string literal_str) }
| literal_str = HEX_LITERAL
  { Lang.Literal (ParserHelper.hex_literal_of_string literal_str) }
| literal_val = INT
  { Lang.Literal (Lang.NoLength literal_val)}
| ident = IDENT
  { Lang.Identifier ident }
| KEYWORD_RECURSE
  { Lang.Recurse }
;
//expressions
expr:
| KEYWORD_SET; lval = lvalue; COLON_EQ; v = node(expr) %prec KEYWORD_SET
  { Lang.Assign (lval, v) }
| e = term
  { e }
| e = un_expr
  { e }
| e = bin_expr
  { e }
| LEFT_PAREN; first_expr = node(expr); COMMA; tuple = separated_list(COMMA, node(expr)); RIGHT_PAREN
  { Lang.Tuple (first_expr::tuple) }
| LEFT_PAREN; RIGHT_PAREN
  { Lang.Tuple ([]) }
| LEFT_PAREN; e = expr; RIGHT_PAREN
  { e }
| KEYWORD_SYNC; ident = IDENT
  { Lang.Sync ident }
| KEYWORD_READY; msg_spec = message_specifier
  { Lang.Ready msg_spec }
| KEYWORD_PROBE; msg_spec = message_specifier
  { Lang.Probe msg_spec }
| KEYWORD_PUT; ident = IDENT; COLON_EQ; v = node(expr) %prec KEYWORD_PUT
  { Lang.SharedAssign (ident, v) }
| KEYWORD_LET; binding = IDENT; EQUAL; v = node(expr) %prec KEYWORD_LET
  { Lang.Let ([binding], v) }
// | KEYWORD_LET; binding = IDENT; EQUAL; v = node(expr); EQ_GT; body = node(expr)
//   {
//     let open Lang in
//     LetIn ([binding], v, {d = Wait ({d = Identifier binding; span = body.span}, body); span = body.span})
//   }
// | KEYWORD_LET; LEFT_PAREN; bindings = separated_list(COMMA, IDENT); RIGHT_PAREN; EQUAL; v = node(expr)
//   { Lang.Let (bindings, v) }
| v = node(expr); SEMICOLON; body = node(expr)
  { Lang.Join (v, body) }
| KEYWORD_SEND; send_pack = send_pack
  { Lang.Send send_pack }
| KEYWORD_RECV; recv_pack = recv_pack
  { Lang.Recv recv_pack }
| v = node(expr); DOUBLE_GT; body = node(expr)
  { Lang.Wait (v, body) }
| KEYWORD_GENERATE; LEFT_PAREN; i=IDENT; COLON; start = INT; COMMA; end_v = INT; COMMA;
  offset = INT; RIGHT_PAREN; LEFT_BRACE; body = node(expr); RIGHT_BRACE
  { Lang.generate_expr (i,start, end_v, offset, body) }
| KEYWORD_GENERATE_SEQ; LEFT_PAREN; i=IDENT; COLON; start = INT; COMMA; end_v = INT; COMMA;
  offset = INT; RIGHT_PAREN; LEFT_BRACE; body = node(expr); RIGHT_BRACE
  { Lang.generate_expr_seq (i,start, end_v, offset, body) }
| e = if_branch
  { e }
| KEYWORD_CALL ; func = IDENT; LEFT_PAREN; args = separated_list(COMMA, node(expr)); RIGHT_PAREN
  { Lang.Call (func, args) }
// | KEYWORD_TRY; KEYWORD_SEND; send_pack = send_pack; KEYWORD_THEN;
//   succ_expr = node(expr); KEYWORD_ELSE; fail_expr = node(expr)
//   { Lang.TrySend (send_pack, succ_expr, fail_expr) }
// | KEYWORD_TRY; KEYWORD_RECV; recv_pack = recv_pack; KEYWORD_THEN;
//   succ_expr = node(expr); KEYWORD_ELSE; fail_expr = node(expr)
//   { Lang.TryRecv (recv_pack, succ_expr, fail_expr) }
// | KEYWORD_SEND; send_pack = send_pack; KEYWORD_THEN; succ_expr = node(expr)
//   { Lang.TrySend (send_pack, succ_expr, Lang.Tuple []) }
// | KEYWORD_RECV; recv_pack = recv_pack; KEYWORD_THEN; succ_expr = node(expr)
//   { Lang.TryRecv (recv_pack, succ_expr, Lang.Tuple []) }
| KEYWORD_CYCLE; n = INT
  { Lang.Cycle n }
| e = node(expr); LEFT_BRACKET; ind = index; RIGHT_BRACKET
  { Lang.Index (e, ind) }
| e = node(expr); PERIOD; fieldname = IDENT
  { Lang.Indirect (e, fieldname) }
| SHARP; LEFT_BRACE; components = separated_list(COMMA, node(expr)); RIGHT_BRACE
  { Lang.Concat components }
| LEFT_BRACE; e = expr; RIGHT_BRACE
  { e }
| KEYWORD_MATCH; e = node(expr); LEFT_BRACE; match_arm_list = separated_list(COMMA, match_arm); RIGHT_BRACE
  { Lang.Match (e, match_arm_list) }
| ASTERISK; reg_ident = IDENT
  { Lang.Read reg_ident }
| constructor_spec = constructor_spec; e = ioption(node(expr_in_parenthese))
  { Lang.Construct (constructor_spec, e) }
| record_name = IDENT; DOUBLE_COLON; LEFT_BRACE;
  base = node(expr); KEYWORD_WITH;
  record_fields = separated_nonempty_list(SEMICOLON, record_field_constr);
  RIGHT_BRACE
  { Lang.Record (record_name, record_fields, Some base) }
| record_name = IDENT; DOUBLE_COLON; LEFT_BRACE;
  record_fields = separated_nonempty_list(SEMICOLON, record_field_constr);
  RIGHT_BRACE
  { Lang.Record (record_name, record_fields, None) }
  (* debug operations *)
| KEYWORD_DPRINT; s = STR_LITERAL; LEFT_PAREN; v = separated_list(COMMA, node(expr)); RIGHT_PAREN
  { Lang.Debug (Lang.DebugPrint (s, v)) }
| KEYWORD_DFINISH
  { Lang.Debug Lang.DebugFinish }
| LEFT_BRACKET; li = separated_list(COMMA, node(expr)); RIGHT_BRACKET
  { Lang.List li }
;

expr_in_parenthese:
  LEFT_PAREN; e = expr; RIGHT_PAREN
  { e }
;

if_branch:
| KEYWORD_IF; cond = node(expr); LEFT_BRACE; then_v = node(expr); RIGHT_BRACE;
  else_v = node(else_branch)?
  { Lang.IfExpr (cond, then_v, Option.value ~default:Lang.dummy_unit_node else_v) }
// | KEYWORD_TRY; ident = IDENT; EQUAL; KEYWORD_RECV; recv_pack = recv_pack
//   { Lang.TryRecv (ident, recv_pack, Lang.dummy_unit_node, Lang.dummy_unit_node) }
| KEYWORD_TRY; ident = IDENT; EQUAL; KEYWORD_RECV; recv_pack = recv_pack;
  LEFT_BRACE; then_v = node(expr); RIGHT_BRACE; else_v = node(else_branch)?
  { Lang.TryRecv (ident, recv_pack, then_v, Option.value ~default:Lang.dummy_unit_node else_v) }
// | KEYWORD_TRY; KEYWORD_SEND; send_pack = send_pack
//   { Lang.TrySend (send_pack, Lang.dummy_unit_node, Lang.dummy_unit_node) }
| KEYWORD_TRY; KEYWORD_SEND; send_pack = send_pack;
  LEFT_BRACE; then_v = node(expr); RIGHT_BRACE; else_v = node(else_branch)?
  { Lang.TrySend (send_pack, then_v, Option.value ~default:(Lang.dummy_ast_node_of_data @@ Lang.Tuple []) else_v) }
;

else_branch:
| KEYWORD_ELSE; LEFT_BRACE; else_v = expr; RIGHT_BRACE
  {
    else_v
  }
| KEYWORD_ELSE; e = if_branch
  {
    e
  }
;

constructor_spec:
  ty = IDENT; DOUBLE_COLON; variant = IDENT
  { let open Lang in {variant_ty_name = ty; variant} }
;
%inline record_field_constr:
  field_name = IDENT; EQUAL; field_value = node(expr)
  { (field_name, field_value) }
;
// The string that sends message
send_pack:
  msg_specifier = message_specifier;
  LEFT_PAREN; data = node(expr); RIGHT_PAREN
  {
    {
      Lang.send_msg_spec = msg_specifier;
      Lang.send_data = data;
    }
  }
;
// The command that recvs message
recv_pack:
  msg_specifier = message_specifier
  {
    {
      Lang.recv_msg_spec = msg_specifier
    }
  }
;

bin_expr:
| v1 = node(expr); DOUBLE_EQ; v2 = node(expr)
  { Lang.Binop (Lang.Eq, v1, (`Single v2)) }
| v1 = node(expr); KEYWORD_IN; LEFT_BRACE; v2 = separated_list(COMMA, node(expr)); RIGHT_BRACE
  { Lang.Binop (Lang.In, v1, (`List v2)) }
| v1 = node(expr); EXCL_EQ; v2 = node(expr)
  { Lang.Binop (Lang.Neq, v1, (`Single v2)) }
| v1 = node(expr); DOUBLE_LEFT_ABRACK; v2 = node(expr)
  { Lang.Binop (Lang.Shl, v1, (`Single v2)) }
| v1 = node(expr); DOUBLE_RIGHT_ABRACK; v2 = node(expr)
  { Lang.Binop (Lang.Shr, v1, (`Single v2)) }
| v1 = node(expr); LEFT_ABRACK; v2 = node(expr)
  { Lang.Binop (Lang.Lt, v1, (`Single v2)) }
| v1 = node(expr); RIGHT_ABRACK; v2 = node(expr)
  { Lang.Binop (Lang.Gt, v1, (`Single v2)) }
| v1 = node(expr); LEFT_ABRACK_EQ; v2 = node(expr)
  { Lang.Binop (Lang.Lte, v1, (`Single v2)) }
| v1 = node(expr); RIGHT_ABRACK_EQ; v2 = node(expr)
  { Lang.Binop (Lang.Gte, v1, (`Single v2)) }
| v1 = node(expr); PLUS; v2 = node(expr)
  { Lang.Binop (Lang.Add, v1, (`Single v2)) }
| v1 = node(expr); MINUS; v2 = node(expr)
  { Lang.Binop (Lang.Sub, v1, (`Single v2)) }
| v1 = node(expr); XOR; v2 = node(expr)
  { Lang.Binop (Lang.Xor, v1, (`Single v2)) }
| v1 = node(expr); AND; v2 = node(expr)
  { Lang.Binop (Lang.And, v1, (`Single v2)) }
| v1 = node(expr); OR; v2 = node(expr)
  { Lang.Binop (Lang.Or, v1, (`Single v2)) }
| v1 = node(expr); DOUBLE_AND; v2 = node(expr)
  { Lang.Binop (Lang.LAnd, v1, (`Single v2)) }
| v1 = node(expr); DOUBLE_OR; v2 = node(expr)
  { Lang.Binop (Lang.LOr, v1, (`Single v2)) }
;

un_expr:
| MINUS; e = node(expr)
  { Lang.Unop (Lang.Neg, e) } %prec UMINUS
| TILDE; e = node(expr)
  { Lang.Unop (Lang.Not, e) }
| AND; e = node(expr)
  { Lang.Unop (Lang.AndAll, e) } %prec UAND
| OR; e = node(expr)
  { Lang.Unop (Lang.OrAll, e) } %prec UOR
;
lvalue:
| regname = IDENT
  { Lang.Reg regname }
| lval = lvalue; LEFT_BRACKET; ind = index; RIGHT_BRACKET
  { Lang.Indexed (lval, ind) }
| lval = lvalue; PERIOD; fieldname = IDENT
  { Lang.Indirected (lval, fieldname) }
| LEFT_PAREN; lval = lvalue; RIGHT_PAREN
  { lval }
;

index:
| i = node(expr)
  { Lang.Single i }
| le = node(expr); PLUS; COLON; sz = node(expr)
  { Lang.Range (le, sz) }
;

match_arm:
| pattern = node(expr); EQ_GT body_opt = node(expr)
  { (pattern, Some body_opt) }
;

// match_arm:
// | OR_GT; pattern = match_pattern; body_opt = match_arm_body?
//   { (pattern, body_opt) }
// ;

// %inline match_pattern:
//   cstr = IDENT; bind_name_opt = IDENT?
//   {
//     { cstr; bind_name = bind_name_opt } : Lang.match_pattern
//   }
// ;

// %inline match_arm_body:
//   POINT_TO; e = node(expr)
//   { e }
;
//Definition of messages: To Do: Doesnt support custom lifetime types, just sync?
message_def:
  dir = message_direction; ident = IDENT; COLON; LEFT_PAREN; data = separated_list(COMMA, sig_type_chan_local); RIGHT_PAREN;
  left_sync_mode_opt = message_sync_mode_spec?;
  right_sync_mode_opt = recv_message_sync_mode_spec?
  {
    let (send_sync_mode_opt, recv_sync_mode_opt) =
      match dir with
      | Lang.Inp -> (right_sync_mode_opt, left_sync_mode_opt)
      | Lang.Out -> (left_sync_mode_opt, right_sync_mode_opt)
    in
    let send_sync_mode = Option.value ~default:Lang.Dynamic send_sync_mode_opt
    and recv_sync_mode = Option.value ~default:Lang.Dynamic recv_sync_mode_opt in
    {
      name = ident;
      dir = dir;
      send_sync = send_sync_mode;
      recv_sync = recv_sync_mode;
      sig_types = data;
      span = {st = $startpos; ed = $endpos}
    } : Lang.message_def
  }
;
//To Do: Support for multiple message types
recv_message_sync_mode_spec:
  MINUS; sync = message_sync_mode_spec
  { sync }
;

message_sync_mode_spec:
| AT; SHARP; n = INT
  {
    Lang.Static (n - 1, n)
  }
| AT; SHARP; o = INT; TILDE; n = INT
  {
    Lang.Static (o, n)
  }
| AT; SHARP; m = IDENT
  {
    Lang.Dependent (m, 0)
  }
| AT; SHARP; m = IDENT; PLUS; n = INT
  {
    Lang.Dependent (m, n)
  }
| AT; KEYWORD_DYN
  {
    Lang.Dynamic
  }
;

message_direction:
| KEYWORD_LEFT
  { Lang.Inp }
| KEYWORD_RIGHT
  { Lang.Out }
;

// sig_type:
//   dtype = data_type; AT; lifetime_opt = lifetime_spec?
//   {
//     let lifetime = Option.value ~default:Lang.sig_lifetime_this_cycle lifetime_opt in
//     {
//       dtype = dtype;
//       lifetime = lifetime;
//     } : Lang.sig_type
//   }
// ;

sig_type_chan_local:
  dtype = data_type; AT; lifetime_opt = lifetime_spec_chan_local?
  {
    let lifetime = Option.value ~default:Lang.sig_lifetime_this_cycle_chan_local lifetime_opt in
    {
      dtype = dtype;
      lifetime = lifetime;
    } : Lang.sig_type_chan_local
  }
;

int_maybe_param:
| n = INT
  { ParamEnv.Concrete n }
| ident = IDENT
  { ParamEnv.Param ident }
;

data_type_no_array:
// | dtype = data_type; LEFT_BRACKET; n = IDENT; RIGHT_BRACKET
//   { `Parametrized (dtype, n) }
| KEYWORD_LOGIC
  { `Logic }
| typename = IDENT
  { `Named (typename, []) }
| typename = IDENT; LEFT_ABRACK; compile_params = separated_list(COMMA, param_value); RIGHT_ABRACK;
  { `Named (typename, compile_params) }
| LEFT_PAREN; comps = separated_list(COMMA, data_type); RIGHT_PAREN
  { `Tuple comps }
;

data_type_array_range:
| LEFT_BRACKET; n = int_maybe_param; RIGHT_BRACKET
  { n }
;

data_type:
| dtype = data_type_no_array; ranges = data_type_array_range*
  { List.fold_right (fun n dtype_c -> `Array (dtype_c, n)) ranges dtype }
  (* We need to reverse the array ranges so logic[3][2] is 3 arrays of length 2 *)
;

lifetime_spec:
  e_time = timestamp
  {
    {
      e = e_time;
    } : Lang.sig_lifetime
  }
;

lifetime_spec_chan_local:
  e_time = timestamp_chan_local
  {
    {
      e = e_time;
    } : Lang.sig_lifetime_chan_local
  }
;


timestamp:
| SHARP; n = INT
  { `Cycles n }
| msg_spec = message_specifier
  { `Message msg_spec }
| KEYWORD_ETERNAL
  { `Eternal }
;

timestamp_chan_local:
| SHARP; n = INT
  { `Cycles n }
| msg = IDENT
  { `Message msg }
| KEYWORD_ETERNAL
  { `Eternal }
;
//Message string
message_specifier:
  endpoint = IDENT; PERIOD; msg_type = IDENT
  {
    {
      endpoint = endpoint;
      msg = msg_type;
    } : Lang.message_specifier
  }
;

shared_var_def:
  KEYWORD_SHARED; LEFT_PAREN; lifetime = lifetime_spec; RIGHT_PAREN; ident = IDENT; KEYWORD_ASSIGNED; KEYWORD_BY; thread_id = INT
  {
    {
      ident = ident;
      assigning_thread = thread_id;
      shared_lifetime = lifetime;
    } : Lang.shared_var_def
  }
;

macro_def:
  | KEYWORD_CONST; id = IDENT; EQUAL; value = INT; SEMICOLON
    {
      { id = id; value = value } : Lang.macro_def
    }
;

function_def:
  | KEYWORD_FUNCTION; name = IDENT; LEFT_PAREN; args = separated_list(COMMA, typed_arg); RIGHT_PAREN; LEFT_BRACE; body = node(expr); RIGHT_BRACE
    {
      { name = name; args = args; body = body } : Lang.func_def
    }
;

typed_arg:
  | name = IDENT; COLON; dtype = data_type
    {
      { arg_name = name; arg_type = Some dtype } : Lang.typed_arg
    }
  | name = IDENT
    {
      { arg_name = name; arg_type = None } : Lang.typed_arg
    }
