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
%token SHARP                (* # *)
%token EQUAL                (* = *)
%token POINT_TO             (* -> *)
%token COLON_EQ             (* := *)
%token LEFT_ABRACK          (* < *)
%token RIGHT_ABRACK         (* > *)
%token LEFT_ABRACK_EQ       (* <= *)
%token RIGHT_ABRACK_EQ      (* >= *)
%token DOUBLE_LEFT_ABRACK   (* << *)
%token DOUBLE_RIGHT_ABRACK  (* << *)
%token DOUBLE_EQ            (* == *)
%token EXCL_EQ              (* != *)
%token EXCL                 (* ! *)
%token PLUS                 (* + *)
%token MINUS                (* - *)
%token DOUBLE_MINUS         (* -- *)
%token XOR                  (* ^ *)
%token AND                  (* & *)
%token OR                   (* | *)
%token AT                   (* @ *)
%token TILDE                (* ~ *)
%token PERIOD               (* . *)
%token KEYWORD_PROC         (* proc *)
%token KEYWORD_CHAN         (* chan *)
%token KEYWORD_IN           (* in *)
%token KEYWORD_LEFT         (* left *)
%token KEYWORD_RIGHT        (* right *)
%token KEYWORD_LOGIC        (* logic *)
%token KEYWORD_FOREIGN      (* foreign *)
%token KEYWORD_IF           (* if *)
%token KEYWORD_THEN         (* then *)
%token KEYWORD_ELSE         (* else *)
%token KEYWORD_WHILE        (* while *)
%token KEYWORD_LET          (* let *)
%token KEYWORD_SEND         (* send *)
%token KEYWORD_RECV         (* recv *)
%token KEYWORD_ETERNAL      (* eternal *)
%token KEYWORD_DONE         (* done *)
%token KEYWORD_TYPE         (* type *)
%token KEYWORD_OF           (* of *)
%token KEYWORD_SET          (* set *)
%token KEYWORD_MATCH        (* match *)
%token KEYWORD_WITH         (* with *)
%token KEYWORD_DYN          (* dyn *)
%token KEYWORD_WAIT         (* wait *)
%token KEYWORD_CYCLE        (* cycle *)
%token KEYWORD_REG          (* reg *)
%token KEYWORD_SPAWN        (* spawn *)
%token KEYWORD_TRY          (* try *)
%token KEYWORD_DPRINT       (* dprint *)
%token KEYWORD_DFINISH      (* dfinish *)
%token <int>INT             (* int literal *)
%token <string>IDENT        (* identifier *)
%token <string>BIT_LITERAL  (* bit literal *)
%token <string>DEC_LITERAL  (* decimal literal *)
%token <string>HEX_LITERAL  (* hexadecimal literal *)
%token <string>STR_LITERAL
%right LEFT_ABRACK RIGHT_ABRACK LEFT_ABRACK_EQ RIGHT_ABRACK_EQ
%right DOUBLE_LEFT_ABRACK DOUBLE_RIGHT_ABRACK
%right EXCL_EQ DOUBLE_EQ
%right KEYWORD_THEN KEYWORD_IN KEYWORD_ELSE SEMICOLON
%right COLON_EQ
%right CONSTRUCT
%left LEFT_BRACKET XOR AND OR PLUS MINUS
%left PERIOD
%nonassoc TILDE UMINUS UAND UOR
%start <Anvil.Lang.compilation_unit> cunit
%%

cunit:
| EOF
  { Anvil.Lang.cunit_empty }
| p = proc_def; c = cunit
  { Anvil.Lang.cunit_add_proc c p }
| ty = type_def; c = cunit
  { Anvil.Lang.cunit_add_type_def c ty }
| cc = channel_class_def; c = cunit
  { Anvil.Lang.cunit_add_channel_class c cc }
;

proc_def:
  KEYWORD_PROC; ident = IDENT; LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  EQUAL; body = proc_def_body
  {
    {
      name = ident;
      args = args;
      body = body;
    } : Anvil.Lang.proc_def
  }
;

proc_def_body:
| prog = expr
  {
    let open Anvil.Lang in {
      channels = [];
      spawns = [];
      regs = [];
      prog = prog;
    }
  }
| KEYWORD_CHAN; chan_def = channel_def; body = proc_def_body
  {
    let open Anvil.Lang in {body with channels = chan_def::(body.channels)}
  }
| KEYWORD_REG; reg_def = reg_def; body = proc_def_body
  {
    let open Anvil.Lang in {body with regs = reg_def::(body.regs)}
  }
| KEYWORD_SPAWN; spawn_def = spawn; body = proc_def_body
  {
    let open Anvil.Lang in {body with spawns = spawn_def::(body.spawns)}
  }
;

type_def:
  KEYWORD_TYPE; name = IDENT; EQUAL; dtype = data_type
  {
    { name = name; body = dtype } : Anvil.Lang.type_def
  }
;

channel_class_def:
  KEYWORD_CHAN; ident = IDENT; EQUAL; LEFT_BRACE; messages = separated_list(COMMA, message_def); RIGHT_BRACE
  {
    {
      name = ident;
      messages = messages;
    } : Anvil.Lang.channel_class_def
  }
;

proc_def_arg_list:
  l = separated_list(COMMA, proc_def_arg)
  { l }
;

proc_def_arg:
  foreign = foreign_tag; ident = IDENT; COLON; chan_dir = channel_direction; chan_class_ident = IDENT
  {
    {
      name = ident;
      channel_class = chan_class_ident;
      dir = chan_dir;
      foreign = foreign;
      opp = None;
    } : Anvil.Lang.endpoint_def
  }
;

foreign_tag:
| { false }
| KEYWORD_FOREIGN { true }
;

channel_direction:
| KEYWORD_LEFT
  { Anvil.Lang.Left }
| KEYWORD_RIGHT
  { Anvil.Lang.Right }
;

channel_def:
  left_foreign = foreign_tag; left_endpoint = IDENT; DOUBLE_MINUS;
  right_foreign = foreign_tag; right_endpoint = IDENT; COLON;
  chan_class = IDENT
  {
    let visibility = match left_foreign, right_foreign with
      | true, true -> Anvil.Lang.BothForeign
      | false, true -> Anvil.Lang.RightForeign
      | _ -> Anvil.Lang.LeftForeign
    in
    {
      channel_class = chan_class;
      endpoint_left = left_endpoint;
      endpoint_right = right_endpoint;
      visibility = visibility;
    } : Anvil.Lang.channel_def
  }
;

spawn:
  proc = IDENT; LEFT_PAREN; params = separated_list(COMMA, IDENT); RIGHT_PAREN
  {
    {
      proc = proc;
      params = params;
    } : Anvil.Lang.spawn_def
  }
;

reg_def:
  ident = IDENT; COLON; dtype = data_type
  {
    {
      name = ident;
      dtype = dtype;
      init = None;
    } : Anvil.Lang.reg_def
  }
;

term:
| literal_str = BIT_LITERAL
  { Anvil.Lang.Literal (ParserHelper.bit_literal_of_string literal_str) }
| literal_str = DEC_LITERAL
  { Anvil.Lang.Literal (ParserHelper.dec_literal_of_string literal_str) }
| literal_str = HEX_LITERAL
  { Anvil.Lang.Literal (ParserHelper.hex_literal_of_string literal_str) }
| literal_val = INT
  { Anvil.Lang.Literal (Anvil.Lang.NoLength literal_val)}
| ident = IDENT
  { Anvil.Lang.Identifier ident }
;

expr:
| KEYWORD_SET; lval = lvalue; COLON_EQ; v = expr
  { Anvil.Lang.Assign (lval, v) }
| e = term
  { e }
| e = un_expr
  { e }
| e = bin_expr
  { e }
// | LEFT_PAREN; tuple = separated_list(COMMA, expr); RIGHT_PAREN
//   { Anvil.Lang.Tuple tuple }
| LEFT_PAREN; e = expr; RIGHT_PAREN
  { e }
| KEYWORD_LET; binding = IDENT; EQUAL; v = expr; KEYWORD_IN; body = expr
  { Anvil.Lang.LetIn (binding, v, body) }
| v = expr; SEMICOLON; body = expr
  { Anvil.Lang.LetIn ("_", v, body) } // TODO: check that the result is of unit type
| KEYWORD_WAIT; KEYWORD_SEND; send_pack = send_pack; KEYWORD_THEN; body = expr
  { Anvil.Lang.Wait (`Send send_pack, body) }
| KEYWORD_WAIT; KEYWORD_RECV; recv_pack = recv_pack; KEYWORD_THEN; body = expr
  { Anvil.Lang.Wait (`Recv recv_pack, body) }
| KEYWORD_CYCLE; SHARP n = INT; KEYWORD_THEN; body = expr
  { Anvil.Lang.Wait (`Cycles n, body) }
| KEYWORD_CYCLE; KEYWORD_THEN; body = expr
  { Anvil.Lang.Wait (Anvil.Lang.delay_single_cycle, body) }
| KEYWORD_IF; cond = expr; KEYWORD_THEN; then_v = expr; KEYWORD_ELSE; else_v = expr
  { Anvil.Lang.IfExpr (cond, then_v, else_v) }
| KEYWORD_TRY; KEYWORD_SEND; send_pack = send_pack; KEYWORD_THEN;
  succ_expr = expr; KEYWORD_ELSE; fail_expr = expr
  { Anvil.Lang.TrySend (send_pack, succ_expr, fail_expr) }
| KEYWORD_TRY; KEYWORD_RECV; recv_pack = recv_pack; KEYWORD_THEN;
  succ_expr = expr; KEYWORD_ELSE; fail_expr = expr
  {
    Anvil.Lang.TryRecv (recv_pack, succ_expr, fail_expr)
  }
| e = expr; LEFT_BRACKET; ind = index; RIGHT_BRACKET
  { Anvil.Lang.Index (e, ind) }
| e = expr; PERIOD; fieldname = IDENT
  { Anvil.Lang.Indirect (e, fieldname) }
| LEFT_BRACE; components = separated_list(COMMA, expr); RIGHT_BRACE
  { Anvil.Lang.Concat components }
| KEYWORD_MATCH; e = expr; KEYWORD_WITH; match_arm_list = match_arm+; KEYWORD_DONE
  { Anvil.Lang.Match (e, match_arm_list) }
| EXCL; reg_ident = IDENT
  { Anvil.Lang.Read reg_ident }
| constructor_spec = constructor_spec; e = ioption(expr)
  { Anvil.Lang.Construct (constructor_spec, e) } %prec CONSTRUCT
  (* debug operations *)
| KEYWORD_DPRINT; s = STR_LITERAL; LEFT_PAREN; v = separated_list(COMMA, expr); RIGHT_PAREN
  { Anvil.Lang.Debug (Anvil.Lang.DebugPrint (s, v)) }
| KEYWORD_DFINISH
  { Anvil.Lang.Debug Anvil.Lang.DebugFinish }
;

constructor_spec:
  ty = IDENT; DOUBLE_COLON; variant = IDENT
  { let open Anvil.Lang in {variant_ty_name = ty; variant} }
;

send_pack:
  msg_specifier = message_specifier;
  LEFT_PAREN; data = expr; RIGHT_PAREN
  {
    {
      Anvil.Lang.send_msg_spec = msg_specifier;
      Anvil.Lang.send_data = data;
    }
  }
;

recv_pack:
  bindings = separated_list(COMMA, IDENT); EQUAL;
  msg_specifier = message_specifier
  {
    {
      Anvil.Lang.recv_binds = bindings;
      Anvil.Lang.recv_msg_spec = msg_specifier
    }
  }
;

bin_expr:
| v1 = expr; DOUBLE_EQ; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Eq, v1, v2) }
| v1 = expr; EXCL_EQ; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Neq, v1, v2) }
| v1 = expr; DOUBLE_LEFT_ABRACK; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Shl, v1, v2) }
| v1 = expr; DOUBLE_RIGHT_ABRACK; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Shr, v1, v2) }
| v1 = expr; LEFT_ABRACK; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Lt, v1, v2) }
| v1 = expr; RIGHT_ABRACK; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Gt, v1, v2) }
| v1 = expr; LEFT_ABRACK_EQ; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Lte, v1, v2) }
| v1 = expr; RIGHT_ABRACK_EQ; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Gte, v1, v2) }
| v1 = expr; PLUS; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Add, v1, v2) }
| v1 = expr; MINUS; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Sub, v1, v2) }
| v1 = expr; XOR; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Xor, v1, v2) }
| v1 = expr; AND; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.And, v1, v2) }
| v1 = expr; OR; v2 = expr
  { Anvil.Lang.Binop (Anvil.Lang.Or, v1, v2) }
;

un_expr:
| MINUS; e = expr
  { Anvil.Lang.Unop (Anvil.Lang.Neg, e) } %prec UMINUS
| TILDE; e = expr
  { Anvil.Lang.Unop (Anvil.Lang.Not, e) }
| AND; e = expr
  { Anvil.Lang.Unop (Anvil.Lang.AndAll, e) } %prec UAND
| OR; e = expr
  { Anvil.Lang.Unop (Anvil.Lang.OrAll, e) } %prec UOR
;

lvalue:
| regname = IDENT
  { Anvil.Lang.Reg regname }
| lval = lvalue; LEFT_BRACKET; ind = index; RIGHT_BRACKET
  { Anvil.Lang.Indexed (lval, ind) }
| lval = lvalue; PERIOD; fieldname = IDENT
  { Anvil.Lang.Indirected (lval, fieldname) }
| LEFT_PAREN; lval = lvalue; RIGHT_PAREN
  { lval }
;

index:
| i = expr
  { Anvil.Lang.Single i }
| le = expr; COLON; ri = expr
  { Anvil.Lang.Range (le, ri) }
;

match_arm:
| OR; pattern = match_pattern; body_opt = match_arm_body?
  { (pattern, body_opt) }
;

match_pattern:
  cstr = IDENT; bind_name_opt = IDENT?
  {
    { cstr; bind_name = bind_name_opt } : Anvil.Lang.match_pattern
  }
;

match_arm_body:
  POINT_TO; e = expr; SEMICOLON
  { e }
;

message_def:
  dir = message_direction; ident = IDENT; COLON; LEFT_PAREN; data = separated_list(COMMA, sig_type_chan_local); RIGHT_PAREN;
  send_sync_mode_opt = message_sync_mode_spec?;
  recv_sync_mode_opt = recv_message_sync_mode_spec?
  {
    let send_sync_mode = Option.value ~default:Anvil.Lang.Dynamic send_sync_mode_opt
    and recv_sync_mode = Option.value ~default:Anvil.Lang.Dynamic recv_sync_mode_opt in
    {
      name = ident;
      dir = dir;
      send_sync = send_sync_mode;
      recv_sync = recv_sync_mode;
      sig_types = data;
    } : Anvil.Lang.message_def
  }
;

recv_message_sync_mode_spec:
  MINUS; sync = message_sync_mode_spec
  { sync }
;

message_sync_mode_spec:
| AT; t = timestamp_chan_local
  {
    Anvil.Lang.Dependent t
  }
| AT; KEYWORD_DYN
  {
    Anvil.Lang.Dynamic
  }
;

message_direction:
| KEYWORD_LEFT
  { Anvil.Lang.In }
| KEYWORD_RIGHT
  { Anvil.Lang.Out }
;

sig_type:
  dtype = data_type; AT; lifetime_opt = lifetime_spec?
  {
    let lifetime = Option.value ~default:Anvil.Lang.sig_lifetime_this_cycle lifetime_opt in
    {
      dtype = dtype;
      lifetime = lifetime;
    } : Anvil.Lang.sig_type
  }
;

sig_type_chan_local:
  dtype = data_type; AT; lifetime_opt = lifetime_spec_chan_local?
  {
    let lifetime = Option.value ~default:Anvil.Lang.sig_lifetime_this_cycle_chan_local lifetime_opt in
    {
      dtype = dtype;
      lifetime = lifetime;
    } : Anvil.Lang.sig_type_chan_local
  }
;

data_type:
| KEYWORD_LOGIC
  { `Logic }
| dtype = data_type; LEFT_BRACKET; n = INT; RIGHT_BRACKET
  { `Array (dtype, n) }
| typename = IDENT
  { `Named typename }
| LEFT_BRACKET; variants = variant_def+; RIGHT_BRACKET
  { `Variant variants }
| LEFT_BRACE; fields = separated_nonempty_list(SEMICOLON, field_def); RIGHT_BRACE
  { `Record fields }
| LEFT_PAREN; comps = separated_list(COMMA, data_type); RIGHT_PAREN
  { `Tuple comps }
;

variant_def:
| OR; name = IDENT; dtype_opt = variant_type_spec?
  { (name, dtype_opt) }
;

variant_type_spec:
| KEYWORD_OF; dtype = data_type
  { dtype }
;

field_def:
| name = IDENT; COLON; dtype = data_type
  { (name, dtype) }
;

lifetime_spec:
  b_time = timestamp; MINUS; e_time = timestamp
  {
    {
      b = b_time;
      e = e_time;
    } : Anvil.Lang.sig_lifetime
  }
;

lifetime_spec_chan_local:
  b_time = timestamp_chan_local; MINUS; e_time = timestamp_chan_local
  {
    {
      b = b_time;
      e = e_time;
    } : Anvil.Lang.sig_lifetime_chan_local
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

message_specifier:
  endpoint = IDENT; DOUBLE_COLON; msg = IDENT
  {
    {
      endpoint = endpoint;
      msg = msg;
    } : Anvil.Lang.message_specifier
  }
;
