type 'a maybe_int_const =
  | Const of int
  | NonConst of 'a

let add_const d non_const_ev v =
  match v with
  | Const v' -> Const (v' + d)
  | NonConst nv' -> NonConst (non_const_ev d nv')

let mul_const d non_const_ev v =
  match v with
  | Const v' -> Const (v' * d)
  | NonConst nv' -> NonConst (non_const_ev d nv')

let map f v =
  match v with
  | Const n -> Const n
  | NonConst nv -> NonConst (f nv)

let binop (eval0 : int -> int -> int) (eval1 : int -> 'a -> 'a) (eval2 : 'a -> 'a -> 'a)
          (a : 'a maybe_int_const) (b : 'a maybe_int_const) =
  match a, b with
  | Const n1, Const n2 -> Const (eval0 n1 n2)
  | Const n1, NonConst v2
  | NonConst v2, Const n1 -> NonConst (eval1 n1 v2)
  | NonConst v1, NonConst v2 -> NonConst (eval2 v1 v2)

let add adder1 adder2 = binop (fun a b -> a + b) adder1 adder2
let mul muler1 muler2 = binop (fun a b -> a * b) muler1 muler2

let string_of f v =
  match v with
  | Const n -> Printf.sprintf "Const (%d)" n
  | NonConst v -> f v |> Printf.sprintf "NonConst (%s)"
