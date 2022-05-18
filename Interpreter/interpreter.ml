exception Eval_error
exception Type_error

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
  | Lambda of string * typ * exp
  | Apply of exp * exp
  | LambdaRec of string * typ * typ * string * exp

let rec free_variables (e : exp) =
  match e with
  | Lambda(d, typ, c) -> List.filter (fun x -> x <> Var (d)) (free_variables c)
  | IsZero(e) -> free_variables(e)
  | Num(n) -> []
  | True -> []
  | If(a, b, c) -> (free_variables a) @ (free_variables b) @ (free_variables c)
  | False -> []
  | Mult(a, b) ->  (free_variables a) @ (free_variables b)
  | Var(s) -> [Var(s)]
  | Apply(a, v) -> (free_variables a)
  | Plus(a, b) ->  (free_variables a) @ (free_variables b)
  | LambdaRec (a,b,c,d,e) -> List.filter(fun x -> x <> Var (d)) (free_variables e);;

let rec substitution (e : exp) (x : string) (b : exp) =
  match e with
  | Var(d) -> if d = x then b else Var(d)
  | True -> True
  | If(ifa, ifb, ifc) -> If((substitution ifa x b), (substitution ifb x b), (substitution ifc x b))
  | Plus(pa, pb) -> Plus(substitution pa x b, substitution pb x b)
  | Lambda(d, typ, c) -> if ( (d = x) || (List.length (List.filter (fun x -> x = Var d) (free_variables b)) >= 1) ) then Lambda(d, typ, c) else Lambda(d, typ, (substitution c x b))
  | Num(n) -> Num(n)
  | IsZero(za) -> IsZero (substitution za x b)
  | Apply(aa, ab) -> Apply((substitution aa x b), (substitution ab x b))
  | Mult(pa, pb) -> Mult(substitution pa x b, substitution pb x b)
  | LambdaRec(a,g,c,d,f) -> if(( x = d ) || (List.length (List.filter(fun x -> x = Var d) (free_variables b)) >= 1) ) then LambdaRec(a,g,c,d,f) else LambdaRec(a,g,c,d, (substitution f x b))
  | False -> False;;

let rec step (e : exp) =
  match e with
  | Plus(a, b) -> (
    match a with
    | Num(ta) -> (
      match b with
      | False -> raise Eval_error
      | Num(tb) -> Num(ta+tb)
      | True -> raise Eval_error
      | _ -> Plus(a, step(b)))
    | True -> raise Eval_error
    | False -> raise Eval_error
    | _ -> Plus(step(a), b))
  | Mult(a, b) -> (
    match a with
    | Num(ta) -> (
      match b with
      | True -> raise Eval_error
      | Num(tb) -> Num(ta*tb)
      | False -> raise Eval_error
      | _ -> Mult(a, step(b)))
    | True -> raise Eval_error
    | False -> raise Eval_error
    | _ -> Mult(step(a), b))
  | IsZero(a) ->  (
    match a with
    | Num(n) -> if (n = 0) then True else False
    | True -> raise Eval_error
    | False -> raise Eval_error
    | _ -> IsZero(step(a)))
  | Apply(a, b) -> (
    match a with
    | Lambda(d, typ, c) -> (
      match b with
      | Num(n) -> (substitution c d b)
      | True -> (substitution c d b)
      | False -> (substitution c d b)
      | Lambda(v,t,b) -> (substitution c d (Lambda(v,t,b)))
      | Var(x) -> raise Eval_error
      | notValue -> Apply(a, step b))
    | Var(x) -> raise Eval_error
    | Num(n) -> raise Eval_error
    | True -> raise Eval_error
    | False -> raise Eval_error
    | LambdaRec(aa,ab,ac,ad,ae) -> (
      match b with
      | Num(n) -> (substitution (substitution ae ad b) aa a)
      | True -> (substitution (substitution ae ad b) aa a)
      | False -> (substitution (substitution ae ad b) aa a)
      | Lambda(v,t,b) -> (substitution (substitution ae ad (Lambda(v,t,b))) aa a)
      | Var(x) -> raise Eval_error
      | notValue -> Apply(a, step b)
    )
    | notLambda -> Apply(step a, b))
  | If(a, b, c) -> (
    match a with
    | True -> b
    | False -> c
    | Num(n) -> raise Eval_error
    | _ -> If(step(a), b, c))
  | _ -> raise Eval_error

let rec multi_step (e : exp) =
  match e with
  | Plus(a, b) -> multi_step(step(Plus(a, b)))
  | True -> True
  | Apply(a, b) -> multi_step(step(Apply(a, b)))
  | Lambda(d, typ, c) -> Lambda(d, typ, c)
  | If(a, b, c) -> multi_step(step(If(a,b,c)))
  | Num(n) -> Num(n)
  | Mult(a, b) -> multi_step(step(Mult(a, b)))
  | Var(x) -> Var(x)
  | False -> False
  | IsZero(e) -> multi_step(step(IsZero(e)))
  | LambdaRec(a,b,c,d,f) -> e





let rec type_check (te : type_environment) (e : exp) =
  match e with
  | True -> TBool
  | False -> TBool
  | IsZero(a) -> (
      match (type_check te a) with
      | TInt -> TBool
      | _ -> raise Type_error
  )
  | Lambda(d, typ, c) -> TArrow(typ, (type_check ((d, typ)::te) c))
  | Var(x) -> (
      match te with
      | [] -> raise Type_error
      | a :: rest -> if (fst a) = x then (snd a) else (type_check rest (Var(x)))
  )
  | Apply(a, b) -> (
      match (type_check te a) with
      | TArrow(t1, t2) -> if ((type_check te b) = t1) then t2 else raise Type_error
      | _ -> raise Type_error
  )
  | Num(n) -> TInt
  | If(a, b, c) -> (
      match (type_check te a) with
      | TBool -> (
          match (type_check te b) with
          | TBool -> (
              match (type_check te c) with
              | TBool -> TBool
              | _ -> raise Type_error
          )
          | TInt -> (
              match (type_check te c) with
              | TInt -> TInt
              | _ -> raise Type_error
          )
          | TArrow(t1a,t2a) -> (
              match (type_check te c) with
              | TArrow(t1b,t2b) -> if ((t1a = t1b)&&(t2a = t2b)) then TArrow(t1a,t2a) else raise Type_error
              | _ -> raise Type_error
          )
      )
      | _ -> raise Type_error
  )
  | Plus(a, b) -> (
      match (type_check te a) with
      | TInt -> (
          match (type_check te b) with
          | TInt -> TInt
          | _ -> raise Type_error
      )
      | _ -> raise Type_error
  )
  | Mult(a, b) -> (
      match (type_check te a) with
      | TInt -> (
          match (type_check te b) with
          | TInt -> TInt
          | _ -> raise Type_error
      )
      | _ -> raise Type_error
  )
  | LambdaRec(a,b,c,d,f) -> TArrow(b,c)