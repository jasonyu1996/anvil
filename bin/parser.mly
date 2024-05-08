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
%token PERIOD               (* . *)
%token EQUAL                (* = *)
%token POINT_TO             (* -> *)
%token D_POINT_TO           (* => *)
%token POINT_BACK           (* <- *)
%token PLUS                 (* + *)
%token MINUS                (* - *)
%token XOR                  (* ^ *)
%token AND                  (* & *)
%token OR                   (* | *)
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
%token <int>INT             (* int literal *)
%token <string>IDENT        (* identifier *)
%token <string>LITERAL      (* bit literal *)
%start <Lang.compilation_unit> cunit
%%

cunit:
| EOF
  { Lang.cunit_empty }
| p = proc_def; c = cunit
  { Lang.cunit_add_proc c p }
| cc = channel_class_def; c = cunit
  { Lang.cunit_add_channel_class c cc }
;

proc_def:
  KEYWORD_PROC; ident = IDENT; LEFT_PAREN; args = proc_def_arg_list; RIGHT_PAREN;
  LEFT_BRACE; channel_def_list = channel_def_list; RIGHT_BRACE;
  LEFT_BRACE; spawns = spawn_list; RIGHT_BRACE;
  LEFT_BRACE; reg_def_list = reg_def_list; RIGHT_BRACE;
  LEFT_BRACE; body = proc_body_list; RIGHT_BRACE
  {
    {
      name = ident;
      args = args;
      channels = channel_def_list;
      spawns = spawns;
      regs = reg_def_list;
      body = body;
    } : Lang.proc_def
  }
;

channel_class_def:
  KEYWORD_CHAN; ident = IDENT; LEFT_BRACE; messages = message_def_list; RIGHT_BRACE
  {
    {
      name = ident;
      messages = messages;
    } : Lang.channel_class_def
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

expr:
| literal_str = LITERAL
  { Lang.Literal (ParserHelper.bit_literal_of_string literal_str) }
| ident = IDENT
  { Lang.Identifier ident }
| ident = IDENT; POINT_BACK; v = expr
  { Lang.Assign (ident, v) }
| v1 = expr; binop = binop; v2 = expr (* FIXME: shift/reduce conflicts *)
  { Lang.Binop (binop, v1, v2) }
| LEFT_PAREN; tuple = separated_list(COMMA, expr); RIGHT_PAREN
  { Lang.Tuple tuple }
| KEYWORD_LET; binding = IDENT; EQUAL; v = expr; KEYWORD_IN; body = expr
  { Lang.LetIn (binding, v, body) } (* FIXME: conflicts *)
| KEYWORD_IF; cond = expr; KEYWORD_THEN; then_v = expr; KEYWORD_ELSE; else_v = expr
  { Lang.IfExpr (cond, then_v, else_v) } (* FIXME: conflicts *)
| KEYWORD_RETURN; endpoint = IDENT; DOUBLE_COLON; msg = IDENT; v = expr
  { Lang.Return (endpoint, msg, v) }
;

binop:
| PLUS
  { Lang.Add }
| MINUS
  { Lang.Sub }
| XOR
  { Lang.Xor }
| AND
  { Lang.And }
| OR
  { Lang.Or }
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
  { Lang.WaitCycles n }
| bindings = separated_list(COMMA, IDENT);
  EQUAL; KEYWORD_SEND; endpoint_ident = IDENT; DOUBLE_COLON; message_name = IDENT;
  LEFT_PAREN; e = expr; RIGHT_PAREN
  {
    Lang.Send (bindings, { endpoint = endpoint_ident; msg = message_name }, e)
  }
| bindings = separated_list(COMMA, IDENT);
  EQUAL; KEYWORD_RECV; endpoint_ident = IDENT; DOUBLE_COLON; message_name = IDENT
  {
    Lang.Recv (bindings, { endpoint = endpoint_ident; msg = message_name })
  }
;

message_def_list:
  messages = separated_list(COMMA, message_def)
  { messages }
;

message_def:
  dir = message_direction; ident = IDENT; COLON; LEFT_PAREN; data = sig_type_list; RIGHT_PAREN;
  POINT_TO; LEFT_PAREN; rets = sig_type_list; RIGHT_PAREN
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

sig_type_list:
  l = separated_list(COMMA, sig_type)
  { l }
;

(* TODO: lifetime *)
sig_type:
  dtype = data_type
  {
    {
      dtype = dtype;
      lifetime = { b = Lang.Cycles 0; e = Lang.Cycles 0 };
    } : Lang.sig_type
  }
;

data_type:
| KEYWORD_LOGIC
  { Lang.Logic }
| dtype = data_type; LEFT_BRACKET; n = INT; RIGHT_BRACKET
  { Lang.Array (dtype, n) }
;
