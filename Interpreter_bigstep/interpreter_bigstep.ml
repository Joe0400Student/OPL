(* This is an OCaml editor.
   Enter your program here and send it to the toplevel using the "Eval code"
   button. *)

   type exp =
   | True
   | False
   | If of exp * exp * exp
   | Num of int
   | IsZero of exp
   | Plus of exp * exp
   | Mult of exp * exp ;;
 
 
 let rec string_of_exp ( e : exp ) =
   match e with
     True -> "true"
   | False -> "false"
   | If(test, truth, fals) -> "if " ^ string_of_exp(test) ^ " then " ^ string_of_exp(truth) ^ " else " ^ string_of_exp(fals)
   | Num(v) -> string_of_int v
   | IsZero(v) -> "(isZero " ^ string_of_exp(v) ^ ")"
   | Plus(l,r) -> "(" ^ string_of_exp(l) ^ " + " ^ string_of_exp(r) ^ ")"
   | Mult(l,r) -> "(" ^ string_of_exp(l) ^ " * " ^ string_of_exp(r) ^ ")";;
 
 let tests = [
   Num (3);
   True;
   False;
   Plus (
     Num 3,
     Num 2
   );
   Mult (
     Num 3,
     Num 2
   );
   Plus (
     Num 3,
     Plus (
       Num 3,
       Mult (
         Num 2,
         Plus (
           Num 3,
           Num 2
         )
       )
     )
   );
   If (
     True,
     Num 3,
     Num 5
   );
   If (
     False,
     Plus (
       Num 3,
       Num 2
     ),
     Plus (
       Num 5,
       Num 1
     )
   );
   If (
     Plus (
       False,
       True
     ),
     Plus (
       Num 3,
       False
     ),
     Mult (
       Num 3,
       Num 1
     )
   );
   If (
     IsZero (
       Num 1
     ),
     Plus (
       Num 3,
       Num 2
     ),
     Plus (
       Num 5,
       Num 1
     )
   );
   IsZero (
     Mult (
       Num 3,
       Num 5
     )
   );
   IsZero (
     If (
       IsZero (
         Num 1
       ),
       Plus (
         Num 3,
         Num 2
       ),
       Plus (
         Num 5,
         Num 1
       )
     )
   );
   Plus (
     Num 3,
     If (
       IsZero (
         Num 1
       ),
       Plus (
         Num 3,
         Num 2
       ),
       Plus (
         Num 5,
         Num 1
       )
     )
   );
   Plus (
     Num 3,
     If (
       IsZero (
         Num 1
       ),
       Plus (
         Num 3,
         Num 2
       ),
       Mult (
         Plus (
           Num 5,
           Num 1
         ),
         IsZero (
           True
         )
       )
     )
   );
   If (
     If (
       True,
       True,
       False
     ),
     Plus (
       Num 3,
       Num 2
     ),
     Plus (
       Num 5,
       Num 1
     )
   );
   If (
     True,
     If (
       IsZero (
         Mult (
           Num 3,
           Num 5
         )
       ),
       Plus (
         Num 3,
         Num 2
       ),
       Plus (
         Num 5,
         Num 1
       )
     ),
     If (
       True,
       Mult (
         Num 3,
         Num 2
       ),
       Mult (
         Num 2,
         Plus (
           Num 3,
           Num 2
         )
       )
     )
   )
 ];;
 exception Eval_error

 let rec eval ( e: exp) = 
  match e with 
  | True -> True
  | False -> False
  | Num(a) -> Num(a)
  | If(a,b,c) -> (
    match eval(a) with
    | True -> eval(b)
    | False -> eval(c)
    | _ -> raise Eval_error
  )
  | IsZero(a) -> (
    match eval(a) with
    | Num(b) -> if b == 0 then True else False
    | _ -> raise Eval_error
  )
  | Plus(a,b) -> (
    match eval(a) with
    | Num(c) -> (
      match eval(b) with
      | Num(d) -> Num(c + d)
      | _ -> raise Eval_error
      )
    | _ -> raise Eval_error
  )
  | Mult(a,b) -> (
    match eval(a) with
    | Num(c) -> (
      match eval(b) with
      | Num(d) -> Num(c * d)
      | _ -> raise Eval_error
    )
    | _ -> raise Eval_error
  );;

 (*
   print_string "one\n";;
 
   let rec forEach lst =
     match lst with
     | [] -> ()
     | x :: xs -> print_string string_of_exp(x); print_string "\n"; forEach xs;;
 
   forEach tests;;
 *)
 
 open Printf
 let (|>) v f = f v;;
 
 let rec reverse = function [] -> []
                          | x :: l -> (reverse l) @ [x];;
 
 let rec map f =
   function [] -> []
          | x :: l -> (f x)::(map f l);;
 
 let join_endl e = ( string_of_exp e ) ^ "\n";;
 let human_readable = map join_endl (reverse tests);;
 
 map print_string human_readable;;
 