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
%token D_POINT_TO           (* => *)
%token COLON_EQ             (* := *)
%token LEFT_ABRACK          (* < *)
%token RIGHT_ABRACK         (* > *)
%token LEFT_ABRACK_EQ       (* <= *)
%token RIGHT_ABRACK_EQ      (* >= *)
%token DOUBLE_LEFT_ABRACK   (* << *)
%token DOUBLE_RIGHT_ABRACK  (* << *)
%token DOUBLE_EQ            (* == *)
%token EXCL_EQ              (* != *)
%token PLUS                 (* + *)
%token MINUS                (* - *)
%token XOR                  (* ^ *)
%token AND                  (* & *)
%token OR                   (* | *)
%token AT                   (* @ *)
%token TILDE                (* ~ *)
%token PERIOD               (* . *)
%token KEYWORD_PROC         (* proc *)
%token KEYWORD_CHAN         (* chan *)
%token KEYWORD_IN           (* in *)
%token KEYWORD_OUT          (* out *)
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
%token KEYWORD_RETURN       (* return *)
%token KEYWORD_REF          (* ref *)
%token KEYWORD_ETERNAL      (* eternal *)
%token KEYWORD_DONE         (* done *)
%token KEYWORD_TRYSEND      (* try_send *)
%token KEYWORD_TRYRECV      (* try_recv *)
%token KEYWORD_TYPE         (* type *)
%token KEYWORD_OF           (* of *)
%token KEYWORD_SET          (* set *)
%token <int>INT             (* int literal *)
%token <string>IDENT        (* identifier *)
%token <string>BIT_LITERAL  (* bit literal *)
%token <string>DEC_LITERAL  (* decimal literal *)
%token <string>HEX_LITERAL  (* hexadecimal literal *)
%right COLON_EQ
%right KEYWORD_IN KEYWORD_ELSE
%right EXCL_EQ DOUBLE_EQ
%right DOUBLE_LEFT_ABRACK DOUBLE_RIGHT_ABRACK
%right LEFT_ABRACK RIGHT_ABRACK LEFT_ABRACK_EQ RIGHT_ABRACK_EQ
%nonassoc TILDE UMINUS UAND UOR
%left LEFT_BRACKET PERIOD XOR AND OR PLUS MINUS
%start <Lang.compilation_unit> cunit
%%

cunit:
| EOF
  { Lang.cunit_empty }
| p = proc_def; c = cunit
  { Lang.cunit_add_proc c p }
| ty = type_def; c = cunit
  { Lang.cunit_add_type_def c ty }
| cc = channel_class_def; c = cunit
  { Lang.cunit_add_channel_class c cc }
;

proc_def:
  KEYWORD_PROC; ident = IDENT; LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  LEFT_BRACE; channel_def_list = channel_def_list; RIGHT_BRACE;
  LEFT_BRACE; spawns = spawn_list; RIGHT_BRACE;
  LEFT_BRACE; reg_def_list = reg_def_list; RIGHT_BRACE;
  LEFT_BRACE; ref_def_list = separated_list(COMMA, ref_def); RIGHT_BRACE;
  LEFT_BRACE; body = proc_body_list; RIGHT_BRACE
  {
    {
      name = ident;
      args = args;
      channels = channel_def_list;
      spawns = spawns;
      regs = reg_def_list;
      refs = ref_def_list;
      body = body;
    } : Lang.proc_def
  }
;

type_def:
  KEYWORD_TYPE; name = IDENT; EQUAL; dtype = data_type
  {
    { name = name; body = dtype } : Lang.type_def
  }

channel_class_def:
  KEYWORD_CHAN; ident = IDENT; LEFT_BRACE; messages = message_def_list; RIGHT_BRACE
  {
    {
      name = ident;
      messages = messages;
    } : Lang.channel_class_def
  }
;

ref_def:
  ident = IDENT; COLON; ty = sig_type
  {
    {
      name = ident;
      ty = ty;
    } : Lang.ref_def
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
    } : Lang.endpoint_def
  }
;

foreign_tag:
| { false }
| KEYWORD_FOREIGN { true }
;

channel_direction:
| KEYWORD_LEFT
  { Lang.Left }
| KEYWORD_RIGHT
  { Lang.Right }
;

channel_def_list:
  l = separated_list(COMMA, channel_def)
  { l }
;

channel_def:
  left_foreign = foreign_tag; left_endpoint = IDENT; POINT_TO;
  right_foreign = foreign_tag; right_endpoint = IDENT; COLON;
  chan_class = IDENT
  {
    let visibility = match left_foreign, right_foreign with
      | true, true -> Lang.BothForeign
      | false, true -> Lang.RightForeign
      | _ -> Lang.LeftForeign
    in
    {
      channel_class = chan_class;
      endpoint_left = left_endpoint;
      endpoint_right = right_endpoint;
      visibility = visibility;
    } : Lang.channel_def
  }
;

spawn_list:
  l = separated_list(COMMA, spawn)
  { l }
;

spawn:
  proc = IDENT; LEFT_PAREN; params = ident_list; RIGHT_PAREN
  {
    {
      proc = proc;
      params = params;
    } : Lang.spawn_def
  }
;

ident_list:
  l = separated_list(COMMA, IDENT)
  { l }
;

reg_def_list:
  l = separated_list(COMMA, reg_def)
  { l }
;

reg_def:
  ident = IDENT; COLON; dtype = data_type
  {
    {
      name = ident;
      dtype = dtype;
      init = None;
    } : Lang.reg_def
  }
;

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
;

expr:
| KEYWORD_SET; lval = lvalue; COLON_EQ; v = expr
  { Lang.Assign (lval, v) }
| e = term
  { e }
| e = un_expr
  { e }
| e = bin_expr
  { e }
// | LEFT_PAREN; tuple = separated_list(COMMA, expr); RIGHT_PAREN
//   { Lang.Tuple tuple }
| LEFT_PAREN; e = expr; RIGHT_PAREN
  { e }
| KEYWORD_LET; binding = IDENT; EQUAL; v = expr; KEYWORD_IN; body = expr
  { Lang.LetIn (binding, v, body) } (* FIXME: conflicts *)
| KEYWORD_IF; cond = expr; KEYWORD_THEN; then_v = expr; KEYWORD_ELSE; else_v = expr
  { Lang.IfExpr (cond, then_v, else_v) } (* FIXME: conflicts *)
| KEYWORD_RETURN; endpoint = IDENT; DOUBLE_COLON; msg = IDENT; v = expr; KEYWORD_DONE
  { Lang.Return (endpoint, msg, v) }
| KEYWORD_REF; ref_name = IDENT; EQUAL; v = expr; KEYWORD_DONE
  { Lang.Ref (ref_name, v) }
| KEYWORD_TRYSEND; bindings = separated_list(COMMA, IDENT); EQUAL;
  endpoint = IDENT; DOUBLE_COLON; msg = IDENT;
  LEFT_PAREN; data = expr; RIGHT_PAREN; KEYWORD_THEN;
  succ_expr = expr; KEYWORD_ELSE; fail_expr = expr
  { Lang.TrySend ({Lang.send_binds = bindings;
                   Lang.send_msg_spec = {Lang.endpoint = endpoint; Lang.msg = msg};
                   Lang.send_data = data}, succ_expr, fail_expr) }
| KEYWORD_TRYRECV; bindings = separated_list(COMMA, IDENT); EQUAL;
  endpoint = IDENT; DOUBLE_COLON; msg = IDENT; KEYWORD_THEN;
  succ_expr = expr; KEYWORD_ELSE; fail_expr = expr
  {
    Lang.TryRecv ({
      Lang.recv_binds = bindings;
      Lang.recv_msg_spec = {Lang.endpoint = endpoint; Lang.msg = msg};
    }, succ_expr, fail_expr)
  }
| e = expr; LEFT_BRACKET; ind = index; RIGHT_BRACKET
  { Lang.Index (e, ind) }
| e = expr; PERIOD; fieldname = IDENT
  { Lang.Indirect (e, fieldname) }
| LEFT_BRACE; components = separated_list(COMMA, expr); RIGHT_BRACE
  { Lang.Concat components }
(* TODO: constructor *)
;

bin_expr:
| v1 = expr; DOUBLE_EQ; v2 = expr
  { Lang.Binop (Lang.Eq, v1, v2) }
| v1 = expr; EXCL_EQ; v2 = expr
  { Lang.Binop (Lang.Neq, v1, v2) }
| v1 = expr; DOUBLE_LEFT_ABRACK; v2 = expr
  { Lang.Binop (Lang.Shl, v1, v2) }
| v1 = expr; DOUBLE_RIGHT_ABRACK; v2 = expr
  { Lang.Binop (Lang.Shr, v1, v2) }
| v1 = expr; LEFT_ABRACK; v2 = expr
  { Lang.Binop (Lang.Lt, v1, v2) }
| v1 = expr; RIGHT_ABRACK; v2 = expr
  { Lang.Binop (Lang.Gt, v1, v2) }
| v1 = expr; LEFT_ABRACK_EQ; v2 = expr
  { Lang.Binop (Lang.Lte, v1, v2) }
| v1 = expr; RIGHT_ABRACK_EQ; v2 = expr
  { Lang.Binop (Lang.Gte, v1, v2) }
| v1 = expr; PLUS; v2 = expr
  { Lang.Binop (Lang.Add, v1, v2) }
| v1 = expr; MINUS; v2 = expr
  { Lang.Binop (Lang.Sub, v1, v2) }
| v1 = expr; XOR; v2 = expr
  { Lang.Binop (Lang.Xor, v1, v2) }
| v1 = expr; AND; v2 = expr
  { Lang.Binop (Lang.And, v1, v2) }
| v1 = expr; OR; v2 = expr
  { Lang.Binop (Lang.Or, v1, v2) }
;

un_expr:
| MINUS; e = expr %prec UMINUS
  { Lang.Unop (Lang.Neg, e) }
| TILDE; e = expr
  { Lang.Unop (Lang.Not, e) }
| AND; e = expr %prec UAND
  { Lang.Unop (Lang.AndAll, e) }
| OR; e = expr %prec UOR
  { Lang.Unop (Lang.OrAll, e) }
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
| i = expr
  { Lang.Single i }
| le = expr; COLON; ri = expr
  { Lang.Range (le, ri) }
;

proc_body_list:
  l = separated_list(SEMICOLON, proc_body)
  { l }
;

proc_body:
| delays = separated_list(SEMICOLON, delay); D_POINT_TO; cycle_expr = expr
  {
    {
      delays = delays;
      cycle = cycle_expr;
      transition = Lang.Seq;
    } : Lang.proc_body
  }
| KEYWORD_IF delays = separated_list(SEMICOLON, delay); D_POINT_TO; cycle_expr = expr;
  LEFT_BRACE; if_body = proc_body_list; RIGHT_BRACE;
  else_clause = proc_else_clause?
  {
    let trans = match else_clause with
      | Some else_body -> Lang.IfElse (if_body, else_body)
      | None -> Lang.If if_body
    in
    {
      delays = delays;
      cycle = cycle_expr;
      transition = trans;
    } : Lang.proc_body
  }
| KEYWORD_WHILE delays = separated_list(SEMICOLON, delay); D_POINT_TO; cycle_expr = expr;
  LEFT_BRACE; while_body = proc_body_list; RIGHT_BRACE;
  {
    {
      delays = delays;
      cycle = cycle_expr;
      transition = Lang.While while_body;
    } : Lang.proc_body
  }
;

proc_else_clause:
  KEYWORD_ELSE; LEFT_BRACE; else_body = proc_body_list; RIGHT_BRACE
  { else_body }
;

delay:
| SHARP; n = INT
  { `Cycles n }
| KEYWORD_SEND; bindings = separated_list(COMMA, IDENT);
  EQUAL; endpoint_ident = IDENT; DOUBLE_COLON; message_name = IDENT;
  LEFT_PAREN; e = expr; RIGHT_PAREN
  {
    `Send ({
      send_binds = bindings;
      send_msg_spec = { endpoint = endpoint_ident; msg = message_name };
      send_data = e
    })
  }
| KEYWORD_RECV;  bindings = separated_list(COMMA, IDENT);
  EQUAL; endpoint_ident = IDENT; DOUBLE_COLON; message_name = IDENT
  {
    `Recv ({
      recv_binds = bindings;
      recv_msg_spec = { endpoint = endpoint_ident; msg = message_name }
    })
  }
;

message_def_list:
  messages = separated_list(COMMA, message_def)
  { messages }
;

message_def:
  dir = message_direction; ident = IDENT; COLON; LEFT_PAREN; data = separated_list(COMMA, sig_type_chan_local); RIGHT_PAREN;
  POINT_TO; LEFT_PAREN; rets = separated_list(COMMA, sig_type_chan_local); RIGHT_PAREN
  {
    {
      name = ident;
      dir = dir;
      sig_types = data;
      ret_types = rets;
    } : Lang.message_def
  }
;

message_direction:
| KEYWORD_IN
  { Lang.In }
| KEYWORD_OUT
  { Lang.Out }
;

sig_type:
  dtype = data_type; AT; lifetime_opt = lifetime_spec?
  {
    let lifetime = Option.value ~default:Lang.sig_lifetime_this_cycle lifetime_opt in
    {
      dtype = dtype;
      lifetime = lifetime;
    } : Lang.sig_type
  }
;

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

data_type:
| KEYWORD_LOGIC
  { `Logic }
| dtype = data_type; LEFT_BRACKET; n = INT; RIGHT_BRACKET
  { `Array (dtype, n) }
| typename = IDENT
  { `Named typename }
| LEFT_BRACKET; variants = variant_def+; RIGHT_BRACKET
  { `Variant variants }
| LEFT_BRACE; fields = separated_list(SEMICOLON, field_def); RIGHT_BRACE
  { `Record fields }
| LEFT_PAREN; comps = separated_list(COMMA, data_type); RIGHT_PAREN
  { `Tuple comps }
;

variant_def:
| OR; name = IDENT; KEYWORD_OF; dtype = data_type
  { (name, dtype) }
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
    } : Lang.sig_lifetime
  }
;

lifetime_spec_chan_local:
  b_time = timestamp_chan_local; MINUS; e_time = timestamp_chan_local
  {
    {
      b = b_time;
      e = e_time;
    } : Lang.sig_lifetime_chan_local
  }
;


timestamp:
| SHARP; n = INT
  { `Cycles n }
| KEYWORD_SEND; msg_spec = message_specifier
  { `Send msg_spec }
| KEYWORD_RECV; msg_spec = message_specifier
  { `Recv msg_spec }
| KEYWORD_ETERNAL
  { `Eternal }
;

timestamp_chan_local:
| SHARP; n = INT
  { `Cycles n }
| KEYWORD_SEND; msg = IDENT
  { `Send msg }
| KEYWORD_RECV; msg = IDENT
  { `Recv msg }
| KEYWORD_ETERNAL
  { `Eternal }
;

message_specifier:
  endpoint = IDENT; DOUBLE_COLON; msg = IDENT
  {
    {
      endpoint = endpoint;
      msg = msg;
    } : Lang.message_specifier
  }
;
