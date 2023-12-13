(* === 
   Parser, Type Checker and Interpreter for the language (subset of OCaml)
     given as graduate credit project in CIS505/705, Kansas State University, Fall 2023

  Selected Parts of an OCaml Implementation
*)

(* -- CONCRETE SYNTAX

T ::= int
   | (T -> T)
   | (T list)

- Expressions E are given by the syntax

E ::= n    
   |  x
   | fun x:T -> E
   | if E relop E then E else E
   | (E op E)
   | ([] : T)
   | (E :: E)
   | (E E)
   | match E with
      | [] -> E
      | (x :: xs) -> E

op ::= 
       +
    |  -
    |  *

relop ::=   
       =
    |  <

- Programs P are given by the syntax

P ::= let x = E in P
   |  let f (x:T) = E in P
   |  let rec f (x:T) = (E : T) in P
   |  E

*)

(* -- EXAMPLES *)

(*
let test1 = "let g (ys:(int list)) = match ys with | [] -> 8 | (x :: xs) -> x 
     in (g (3 :: (7 :: ([] : (int list)))))"

let test2 = "let rec h (n:int) = (if 0 < n then (n * (h (n - 1))) else 1 : int)
     in (h 5)" *)

(* -- EXCEPTIONS *)

exception InputEndsTooEarly
exception InputEndsTooLate
exception IndentifierExpectedBut of string 
exception NotDeclared of string
exception FunInputTypeMismatch
exception IfWrongArgs
exception TestNotInteger
exception ApplyNotClosure
exception ExpectedList
exception InvalidConsArguments
exception InvalidMatchStatementInput
exception OperandNotInteger
exception BranchMismatchType
exception ConsMismatchType
exception FunNotFunType
exception PlusWrongArgs
exception MinusWrongArgs
exception TimesWrongArgs
exception InvalidListType
exception OutputClosure
exception InvalidCharacter of string
exception TypeMismatch
exception InvalidToken
exception ParseError
exception ReturnTypeNotMatchRecExpType


(* -- ABSTRACT SYNTAX *)

type identifier = string

type typeY =
  | IntY
  | FunY of typeY * typeY
  | ListY of typeY

let rec print_type : typeY -> string =
  fun t ->
   match t with
   | IntY -> "int"
   | FunY (t1,t2) ->
       "("^(print_type t1)^" -> "^(print_type t2)^")"
   | ListY t1 -> (print_type t1)^" list"

let rec checkY t1 t2 = 
  match (t1, t2) with
  | (IntY, IntY) -> true 
  | (ListY t1, ListY t2) -> (checkY t1 t2)
  | (FunY (t1, t2), FunY (t3, t4)) -> (checkY t1 t3) && (checkY t2 t4)
  | _ -> false 

type expE =
  | NumE of int
  | IdE of identifier
  | FunE of identifier * typeY * expE
  | ApplyE of expE * expE
  | IfEqE of expE * expE * expE * expE
  | IfLtE of expE * expE * expE * expE
  | PlusE of expE * expE
  | MinusE of expE * expE
  | TimesE of expE * expE
  | NilE of typeY
  | ConsE of expE * expE
  | MatchE of expE * expE * identifier * identifier * expE

type progP =
  | LetVarP of identifier * expE * progP
  | LetFunP of identifier * identifier * typeY * expE * progP
  | LetRecP of identifier * identifier * typeY * expE * typeY * progP
  | ExpP of expE

(* -- SCANNER
    converts the input string into a list of "tokens" *)

type tokenT = 
 | PlusT
 | MinusT
 | TimesT
 | EqualT
 | LessT
 | LparenT
 | RparenT
 | LbracketT
 | RbracketT
 | VbarT
 | ColonT
 | ConsT
 | ArrowT
 | IntT
 | ListT
 | FunT
 | IfT
 | ThenT
 | ElseT
 | MatchT
 | WithT
 | LetT
 | RecT
 | InT
 | IdT of identifier
 | NumT of int

let print_token token = match token with
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | EqualT -> "="
 | LessT -> "<"
 | LparenT -> "("
 | RparenT -> ")"
 | LbracketT -> "["
 | RbracketT -> "]"
 | VbarT -> "|"
 | ColonT -> ":"
 | ConsT -> "::"
 | ArrowT -> "->"
 | IntT -> "int"
 | ListT -> "list"
 | FunT -> "fun"
 | IfT -> "if"
 | ThenT -> "then"
 | ElseT -> "else"
 | MatchT -> "match"
 | WithT -> "with"
 | LetT -> "let"
 | RecT -> "rec"
 | InT -> "in"
 | (IdT id) -> ("identifier "^id)
 | (NumT n) -> "number"
 


let is_digit(ch) = 
   Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let char2digit(ch) = Char.code ch - Char.code '0'

let is_letter(ch) = 
    (Char.code ch >= Char.code 'a' && Char.code ch <= Char.code 'z')
 || (Char.code ch >= Char.code 'A' && Char.code ch <= Char.code 'Z')

let is_next : string -> char -> bool = fun str -> fun ch ->
  if str = ""
  then false
  else if String.get str 0 = ch
  then true
  else false

let scanNum : string -> (int * string) = fun str ->
  let rec get_num acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_digit c
      then get_num (10 * acc + (char2digit c)) str' 
      else (acc, str)
 in get_num 0 str

let scanId : string -> (string * string) = fun str ->
  let rec get_id acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_letter c || is_digit c || c = '_'
      then get_id (acc ^ (String.make 1 c)) str'
      else (acc, str)
 in get_id "" str

 let string_of_char c = String.make 1 c

 let explode str = 
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in 
    explode_inner 0 []

  let rec implode chars = 
    match chars with
    [] -> ""
    | h::t -> string_of_char h ^ (implode t)

  let checkLongOp : string -> (tokenT * string) = fun str ->
    match (explode str) with
    | '-' :: '>' :: str' -> (ArrowT, (implode str'))
    | '-' :: str' -> (MinusT, (implode str'))
    | ':' :: ':' :: str' -> (ConsT, (implode str'))
    | ':' :: str' -> (ColonT , (implode str'))
    | _ -> raise (InvalidCharacter "not \"-\" or \":\"")

let rec scan : string -> tokenT list = 
  fun str -> 
   if str = ""
   then []
   else let c = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
   if is_digit c
   then let (n,str') = scanNum str
         in (NumT n :: (scan str'))
   else if is_letter (c)
   then let (s,str') = scanId str
     in let token =
       if s = "int" then IntT
       else if s = "list" then ListT
       else if s = "fun" then FunT
       else if s = "if" then IfT
       else if s = "then" then ThenT
       else if s = "else" then ElseT
       else if s = "match" then MatchT
       else if s = "with" then WithT
       else if s = "let" then LetT
       else if s = "rec" then RecT
       else if s = "in" then InT
       else IdT s
     in (token :: scan str')
   else match c with
     | '+' -> PlusT :: (scan str1)
     | '*' -> TimesT :: (scan str1)
     | '=' -> EqualT :: (scan str1)
     | '<' -> LessT :: (scan str1)
     | '(' -> LparenT :: (scan str1)
     | ')' -> RparenT :: (scan str1)
     | '[' -> LbracketT :: (scan str1)
     | ']' -> RbracketT :: (scan str1)
     | '|' -> VbarT :: (scan str1)
     | '-' -> let (token, str1) = (checkLongOp str) in (token :: scan str1)
     | ':' -> let (token, str1) = (checkLongOp str) in (token :: scan str1)
     | ' ' -> scan str1
     | '\x0c' -> scan str1
     | '\n' -> scan str1
     | '\r' -> scan str1
     | '\t' -> scan str1
     | _ -> raise (InvalidCharacter (String.make 1 c))

(* -- Parser Module *)
let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise InputEndsTooEarly
   | (IdT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (IndentifierExpectedBut (print_token token))

(*    Type Parsing    *)
let rec parseType : tokenT list -> typeY * tokenT list =  
   fun tokens ->
    match tokens with
    | [] -> raise InputEndsTooEarly
    | (IntT :: tokens1) ->
         (IntY , tokens1)
    | (LparenT :: tokens1) ->
      (match parseType tokens1 with
        | (t1, ArrowT :: tokens2) ->
          (match (parseType tokens2) with
            | (t2, RparenT :: tokens3) ->
              (FunY (t1, t2), tokens3)
            | _ -> raise ParseError
          )
        | (t, ListT :: RparenT :: tokens2) -> (ListY t, tokens2)
        | _ -> raise ParseError
      )
    | _ -> raise ParseError

(*    Expression Parsing    *)
let rec parseExp : tokenT list -> expE * tokenT list =   
   fun tokens ->
    match tokens with
    | [] -> raise InputEndsTooEarly
    | ((IdT s) :: tokens1) -> (* Identifier *)
         (IdE s, tokens1)
    | (NumT z) :: tokens1 -> (* Number *)
         (NumE z, tokens1)
    | (IfT :: tokens1) ->     (*If statement clause*)
      (match (parseExp tokens1) with
        | (e1, LessT :: tokens2) ->
          (match parseExp (tokens2) with
            | (e2, ThenT :: tokens3) ->
              (match parseExp (tokens3) with
                | (e3, ElseT :: tokens4) ->
                  (match parseExp (tokens4) with
                    | (e4, tokens5) ->
                      (IfLtE (e1, e2, e3, e4), tokens5)
                  )
                | _ -> raise ParseError
              )
            | _ -> raise ParseError  
          )
        | (e1, EqualT :: tokens2) ->
          (match parseExp (tokens2) with
            | (e2, ThenT :: tokens3) ->
              (match parseExp (tokens3) with
                | (e3, ElseT :: tokens4) ->
                  (match parseExp (tokens4) with
                    | (e4, tokens5) ->
                      (IfEqE (e1, e2, e3, e4), tokens5)
                  )
                | _ -> raise ParseError      
              )
            | _ -> raise ParseError
          )
        | _ -> raise ParseError
      )
    | (LparenT :: LbracketT :: RbracketT :: ColonT :: tokens1) -> (*Empty List clause*)
      (match parseType (tokens1) with
        | (t1, RparenT :: tokens2) ->
          (NilE t1, tokens2)
        | _ -> raise ParseError
      )
    | (LparenT :: tokens1) ->
      (match (parseExp tokens1) with
        | (e1, PlusT :: tokens2) ->
          (match (parseExp tokens2) with
            | (e2, RparenT :: tokens3) ->
              (PlusE (e1, e2), tokens3)
            | _ -> raise ParseError
          )
        | (e1, MinusT :: tokens2) ->
          (match (parseExp tokens2) with
            | (e2, RparenT :: tokens3) ->
              (MinusE (e1, e2), tokens3)
            | _ -> raise ParseError
          )
        | (e1, TimesT :: tokens2) ->
          (match (parseExp tokens2) with
            | (e2, RparenT :: tokens3) ->
              (TimesE (e1, e2), tokens3)
            | _ -> raise ParseError
          )
        | (e1, ConsT :: tokens2) ->
          (match parseExp (tokens2) with
            | (e2, RparenT :: tokens3) ->
              (ConsE (e1, e2), tokens3)
            | _ -> raise ParseError
          )
        | (e1, tokens2) ->
          (match parseExp (tokens2) with
            | (e2, RparenT :: tokens3) -> 
              (ApplyE (e1, e2), tokens3)
            | _ -> raise ParseError
          )
      )

    | (MatchT :: tokens1) -> (*Match clause*)
      (match (parseExp tokens1) with
        | (e1, WithT :: VbarT :: LbracketT :: RbracketT :: ArrowT :: tokens2) ->
          (match (parseExp tokens2) with
            | (e2, VbarT :: LparenT :: tokens3) ->
              (match (getIdT tokens3) with
                | (id1, ConsT :: tokens4) -> 
                  (match (getIdT tokens4) with
                    | (id2, RparenT :: ArrowT :: tokens5) ->
                      (match (parseExp tokens5) with
                        | (e3, tokens6) ->
                          (MatchE (e1, e2, id1, id2, e3), tokens6)
                      )
                    | _ -> raise ParseError
                  )
                | _ -> raise ParseError
              )
            | _ -> raise ParseError
          )
        | _ -> raise ParseError
      )
    | (FunT :: tokens1) -> (*Function clause*)
      (match (getIdT tokens1) with
        | (fp, ColonT :: tokens2) ->
          (match (parseType tokens2) with 
            | (t, ArrowT :: tokens3) ->
              (match (parseExp tokens3) with
                | (e, tokens4) ->
                  (FunE (fp, t, e), tokens4)
              )
            | _ -> raise ParseError
          )
        | _ -> raise ParseError
      )
    | _ -> raise ParseError    

(*    Program Parsing    *)
let rec parseProg : tokenT list -> progP * tokenT list =   
   fun tokens ->
    match tokens with
    | (LetT :: RecT :: tokens1) ->
      (match (getIdT tokens1) with
        | (fn, LparenT :: tokens2) ->
          (match (getIdT tokens2) with 
            | (fp, ColonT :: tokens3) ->
              (match (parseType tokens3) with 
                | (fpType, RparenT :: EqualT :: LparenT :: tokens4) ->
                  (match (parseExp tokens4) with 
                    | (e, ColonT :: tokens5) ->
                      (match (parseType tokens5) with 
                        | (eType, RparenT :: InT :: tokens6) ->
                          (match (parseProg tokens6) with 
                            | (p, tokens7) ->
                              (LetRecP (fn, fp, fpType, e, eType, p), tokens7)
                          )
                        | _ -> raise ParseError  
                      )
                    | _ -> raise ParseError  
                  )
                | _ -> raise ParseError 
              )
            | _ -> raise ParseError
          )
        | _ -> raise ParseError
      )
    | (LetT :: tokens1) ->
      (match (getIdT tokens1) with 
        | (fn, LparenT :: tokens2) ->
          (match (getIdT tokens2) with 
            | (fp, ColonT :: tokens3) ->
              (match (parseType tokens3) with 
                | (fpType, RparenT :: EqualT :: tokens4) ->
                  (match (parseExp tokens4) with 
                    | (e, InT :: tokens5) ->
                      (match (parseProg tokens5) with
                        | (p, tokens6) ->
                          (LetFunP (fn, fp, fpType, e, p), tokens6)
                      )
                    | _ -> raise ParseError
                  )
                | _ -> raise ParseError
              )
            | _ -> raise ParseError
          )
        | (fp, EqualT :: tokens2) ->
          (match (parseExp tokens2) with 
            | (e, InT :: tokens3) ->
              (match (parseProg tokens3) with 
                | (p, tokens4) ->
                  (LetVarP (fp, e, p)), tokens4
              )
            | _ -> raise ParseError   
          )
        | _ -> raise ParseError
      )
    | _ -> 
      (match (parseExp tokens) with
        | (e1, tokens2) -> 
          (ExpP e1, tokens2)
      )

let parse : string -> progP =
  fun input_string ->
    let tokens = scan input_string in
    let (prog,tokens1) = parseProg tokens
    in if tokens1 = []
       then prog
       else raise InputEndsTooLate

(* -- ENVIRONMENTS *)
type 'a environment =  identifier -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : identifier -> 'a -> 'a environment -> 'a environment =
  fun new_id a env ->
    fun id -> if id = new_id then a else env id

let retrieveEnv : 'a environment -> identifier -> 'a =
  fun env id -> env id

(* -- VALUES *)

type value =
   NumV of int
 | ListV of value list
 | ClosureV of identifier * expE * value environment
 | RecClosureV of identifier * identifier * expE * value environment

let rec print_value : value -> string =
  fun v -> 
   match v with
   | NumV v -> string_of_int v
   | ClosureV _ -> raise OutputClosure
   | ListV vs ->
       "[ "^(String.concat " ; " (List.map print_value vs))^" ]"

   | RecClosureV _ -> raise OutputClosure

(* -- Type Checker Module *)

let rec typeE exp env = 
  match exp with 
  | NumE n -> IntY
  | IdE id -> retrieveEnv env id 
  | FunE (id, typeY, exp1) ->
    let t1 = (typeE exp1 (insertEnv id typeY env)) in 
    FunY (typeY, t1)
  | IfEqE (exp0, exp1, exp2, exp3) ->
    let t0 = (typeE exp0 env) in
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    let t3 = (typeE exp3 env) in
    if (checkY t0 IntY) && (checkY t1 IntY)
      then (if (checkY t2 t3)
        then t2
        else raise BranchMismatchType)
      else raise OperandNotInteger

  | IfLtE (exp0, exp1, exp2, exp3) ->
    let t0 = (typeE exp0 env) in
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    let t3 = (typeE exp3 env) in 
    if (checkY t0 IntY) && (checkY t1 IntY)
      then (if (checkY t2 t3)
        then t2
        else raise BranchMismatchType)
      else raise OperandNotInteger

  | PlusE (exp1, exp2) -> 
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    if (checkY t1 IntY) && (checkY t2 IntY)
      then IntY
      else raise OperandNotInteger
  
  | MinusE (exp1, exp2) ->
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    if (checkY t1 IntY) && (checkY t2 IntY)
      then IntY
      else raise OperandNotInteger

  | TimesE (exp1, exp2) ->
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    if (checkY t1 IntY) && (checkY t2 IntY)
      then IntY
      else raise OperandNotInteger

  | NilE typeY ->
    (match typeY with 
      | ListY typ -> typeY
      | _ -> raise InvalidListType)

  | ConsE (exp1, exp2) ->
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    (match t2 with 
      | ListY (inTyp) ->
        if (checkY inTyp t1)
          then t2
          else raise TypeMismatch
      | _ -> raise ConsMismatchType)
      
  | ApplyE (exp1, exp2) ->
    let t1 = (typeE exp1 env) in
    let t2 = (typeE exp2 env) in
    (match t1 with 
      | FunY (inTyp, outTyp) ->
        if (checkY inTyp t2)
          then outTyp
          else raise FunInputTypeMismatch
      | _ -> raise FunNotFunType)

  | MatchE (exp0, exp1, id1, id2, exp2) ->
    let t0 = (typeE exp0 env) in
    let t1 = (typeE exp1 env) in
    let t2 = (match t0 with 
                | (ListY lt0) ->
                    (typeE exp2 (insertEnv id1 lt0 (insertEnv id2 t0 env)))
                | _ -> raise InvalidMatchStatementInput
              ) in
    if (checkY t1 t2)
      then t1
      else raise BranchMismatchType

let rec typeP prog env = 
  match prog with 
    | (ExpP e) -> (typeE e env)
    
    | (LetVarP (fp, exp, prog)) ->
      let et = (typeE exp env) in 
        (typeP prog (insertEnv fp et env))
    
        | (LetFunP (fn, fp, t1, exp, prog)) -> 
      let et = (typeE exp (insertEnv fp t1 env)) in 
        (typeP prog (insertEnv fn (FunY (t1, et)) env))
    
    | (LetRecP (fn, fp, t1, exp, te, prog)) ->
      let rt = (typeE exp (insertEnv fp t1 (insertEnv fn (FunY (t1, te)) env))) in
        if (checkY rt te)
          then (typeP prog (insertEnv fn (FunY (t1, rt)) env))
          else raise ReturnTypeNotMatchRecExpType
    

(* -- Evaluation Module *)
let rec evalE exp env = 
  match exp with 
  | NumE n -> NumV n
  | IdE id -> retrieveEnv env id
  | FunE (id, typeY, exp1) -> ClosureV (id, exp1, env)
  | IfEqE (exp0, exp1, exp2, exp3) ->
      (match (evalE exp0 env, evalE exp1 env) with
        | (NumV n1, NumV n2) ->
          if n1 = n2
          then evalE exp2 env 
          else evalE exp3 env 
        | _ -> raise TestNotInteger)
  | IfLtE (exp0, exp1, exp2, exp3) ->
    (match (evalE exp0 env, evalE exp1 env) with 
      | (NumV n1, NumV n2) ->
        if n1 < n2 
        then evalE exp2 env
        else evalE exp3 env
      | _ -> raise TestNotInteger)
  | PlusE (exp1, exp2) ->
    (match (evalE exp1 env, evalE exp2 env) with 
      | (NumV n1, NumV n2) ->
          NumV (n1 + n2)
      | _ -> raise PlusWrongArgs)
  | MinusE (exp1, exp2) ->
    (match (evalE exp1 env, evalE exp2 env) with 
      | (NumV n1, NumV n2) ->
          NumV (n1 - n2)
      | _ -> raise MinusWrongArgs)
  | TimesE (exp1, exp2) ->
    (match (evalE exp1 env, evalE exp2 env) with 
      | (NumV n1, NumV n2) ->
          NumV (n1 * n2)
      | _ -> raise TimesWrongArgs)
  | NilE typeY -> (ListV [])
  | ConsE (exp1, exp2) ->
    (match (evalE exp1 env, evalE exp2 env) with 
      | (v1, ListV l1) -> (ListV (v1 :: l1))
      | _ -> raise InvalidConsArguments)
  | ApplyE (exp1, exp2) ->
    (match (evalE exp1 env, evalE exp2 env) with 
      | (ClosureV (x, exp0, env0), v2) ->
          evalE exp0 (insertEnv x v2 env0)
      | (RecClosureV (x, f, exp0, env0), v2) ->
          evalE exp0 (insertEnv x v2 (insertEnv f (RecClosureV (x, f, exp0, env0)) env0))
      | _ -> raise ApplyNotClosure)
  | MatchE (exp0, exp1, id1, id2, exp2) ->
    (match (evalE exp0 env) with
      | (ListV l1) ->
        (match l1 with 
          | [] -> evalE exp1 env
          | (x :: xs) -> evalE exp2 (insertEnv id1 x (insertEnv id2 (ListV xs) env)))
      | _ -> raise ExpectedList)

let rec evalP prog env = 
  match prog with 
    | LetVarP (fp, e1, body) ->
      (match (evalE e1 env) with 
        | v1 -> evalP body (insertEnv fp v1 env))
    | LetFunP (fn, fp, fpType, e, body) ->
      let v = ClosureV (fp, e, env) in 
        evalP body (insertEnv fn v env)
    | LetRecP (fn, fp, fpType, e, eType, body) ->
      let v = RecClosureV (fp, fn, e, env) in 
        evalP body (insertEnv fn v env)
    | ExpP e -> (evalE e env)


let run input = 
 try (
  let p = parse input in 
  let t = typeP p initEnv in
  let _ = print_string ("Type is: "^(print_type t)^"\n") in
  let v = evalP p initEnv in
 print_value v )
  with
  | ParseError -> "parse error"
  | InputEndsTooEarly -> "input was exhausted before parsing was completed"
  | InputEndsTooLate -> "input continous after program is parsed"
  | IndentifierExpectedBut s -> "Identifier expected but found "^s
  | NotDeclared s -> "Identifier"^s^" used but not declared"
  | FunInputTypeMismatch -> "argument type does not match function type"
  | IfWrongArgs -> "types of compared values are invalid"
  | TestNotInteger -> "expected integer value"
  | ApplyNotClosure -> "expected closure but not found"
  | ExpectedList -> "match expects list as input"
  | InvalidConsArguments -> "expected second argument to be a list"
  | InvalidMatchStatementInput -> "expected list input for match statement"
  | FunNotFunType -> "function part of application not of function type"
  | PlusWrongArgs -> "+ expects two integers as input"
  | MinusWrongArgs -> "- expects two integers as input"
  | TimesWrongArgs -> "* expects two integers as input"
  | InvalidListType -> "empty list need to be a list type"
  | OutputClosure -> "closure part of what is being returned"
  | InvalidCharacter s -> "expected valid character but found "^s
  | TypeMismatch -> "expected two or more expressions to have the same type"
  | InvalidToken -> "invalid token appeared during evaluation"
  | ReturnTypeNotMatchRecExpType -> "return type of function does not match expected type of recursive expression"
  | OperandNotInteger -> "operation expects integers as input"
  | BranchMismatchType -> "branches do not have the same type"
  | ConsMismatchType -> "expected cons arguments to have types that look like: (type :: (type list))"
