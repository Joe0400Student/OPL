exception Eval_error

exception Substitution_error


(*
TODO:REFACTOR THE CODE
*)
type typ =
  | TBool
  | TInt
  | TArrow of typ * typ

type type_environment = (string * typ) list

type exp = 
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp
  | TypeError

type environment = (string * exp) list

let rec step (env: environment) (e : exp) : (environment * exp) = 
  match e with
  | If(a,b,c) -> (
    match a with
    | True -> (env, b)
    | False -> (env, c)
    | Num(d) -> (env, TypeError)
    | Lambda(d,e) -> (env, TypeError)
    | TypeError -> (env, TypeError)
    | _ -> let tmp_a = (step env a) in ((fst tmp_a) @ env , If(snd tmp_a, b, c))
  )
  | IsZero(a) -> (
    match a with 
    | Num(b) -> if b = 0 then (env, True) else (env, False)
    | True -> (env, TypeError)
    | False -> (env, TypeError)
    | Lambda(c,d) -> (env, TypeError)
    | TypeError -> (env, TypeError)
    | _ -> let tmp_a = (step env a) in ( (fst tmp_a) @ env, IsZero(snd tmp_a))
  )
  | Plus(a,b) -> (
    match a with
    | False -> (env, TypeError)
    | True -> (env, TypeError)
    | Lambda(c,d) -> (env, TypeError)
    | TypeError -> (env, TypeError)
    | Num(ta) -> (
      match b with
      | Num(tb) -> (env, Num(ta+tb))
      | False -> (env, TypeError)
      | True -> (env, TypeError)
      | Lambda(c,d) -> (env, TypeError)
      | TypeError -> (env, TypeError)
      | _ -> let tmp_b = (step env b) in ((fst tmp_b) @ env, Plus(a,snd tmp_b))
    )
    | _ -> let tmp_a = (step env a) in ((fst tmp_a) @ env, Plus(snd tmp_a,b))
  )
  | Mult(a,b) -> (
    match a with
    | False -> (env, TypeError)
    | True -> (env, TypeError)
    | Lambda(c,d) -> (env, TypeError)
    | TypeError -> (env, TypeError)
    | Num(ta) -> (
      match b with
      | Num(tb) -> (env, Num(ta*tb))
      | False -> (env, TypeError)
      | True -> (env, TypeError)
      | Lambda(c,d) -> (env, TypeError)
      | TypeError -> (env, TypeError)
      | _ -> let tmp_b = (step env b) in ((fst tmp_b) @ env, Mult(a,snd tmp_b))
    )
    | _ -> let tmp_a = (step env a) in ((fst tmp_a) @ env, Mult(snd tmp_a,b))
  )
  | Apply(a,b) -> (
    match a with
    | Lambda(c,d) ->(
      match b with
      | True -> ((c,True)::env), d
      | False-> ((c,False)::env),d
      | Num(e)->((c,b)::env),d
      | Lambda(e,f) -> ((c,b)::env),d
      | TypeError -> (env, TypeError)
      | notValue -> let tmp = (step env b) in ((c, b)::env), Apply(Lambda(c,d), snd tmp)
    )
    | True -> (env, TypeError)
    | False -> (env, TypeError)
    | Num(c) -> (env, TypeError)
    | TypeError -> (env, TypeError)
    | notLambda -> let tmp = (step env a) in (env, Apply(snd tmp, b))
  )
  | Var(s) -> (
    match env with
    | [] -> (env, TypeError)
    | a :: rest -> if fst a = s then (env, snd a) else step rest (Var(s))
  )
  | Let(a,b,c) -> (env, Apply(Lambda(a,c),b))
  | _ -> (env, TypeError);;


let rec multi_step (env: environment) (e: exp) : (environment * exp) =
  match e with
  | True -> (env, True)
  | False -> (env, False)
  | Num(a) -> (env, Num(a))
  | Lambda(a,b) -> (env, Lambda(a,b))
  | TypeError -> (env, TypeError)
  | _ -> let tmp = (step env e) in multi_step (fst tmp) (snd tmp);;

