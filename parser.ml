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

let test1 = "let g (ys:(int list)) = match ys with | [] -> 8 | (x :: xs) -> x 
     in (g (3 :: (7 :: ([] : (int list)))))"

let test2 = "let rec h (n:int) = (if 0 < n then (n * (h (n - 1))) else 1 : int)
     in (h 5)"

(* -- EXCEPTIONS *)

(* for parsing *)
exception InputEndsButExpected of string
exception TokenSeenButExpected of string * string
exception ExtraInput
exception ParserIncomplete

(* for typing and interpretation *)
exception NotDeclared of string

(* for typing *)
exception OperandNotIntegerType
exception FunNotFunType
exception FunMismatchType
exception BranchMismatchType
exception MatchNotListType
exception ConsMismatchType
exception RecConflictType
exception TypeCheckerIncomplete

(* for interpretation *)
exception OperandNotInteger
exception ApplyNotClosure
exception MatchNotList
exception ConsWithNonList
exception OutputClosure
exception InterpreterIncomplete

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

let rec scan : string -> tokenT list = 
  fun str -> 
   if str = ""
   then []
   else let c = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
   if is_digit c
   then let (n,str') = scanNum str
         in (NumT n :: (scan str'))
   else if is_letter c
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
     | '-' -> if is_next str1 '>'
              then ArrowT :: (scan (String.sub str1 1 (String.length str1 - 1)))
              else MinusT :: (scan str1)
     | ':' -> if is_next str1 ':'
              then ConsT :: (scan (String.sub str1 1 (String.length str1 - 1)))
              else ColonT :: (scan str1)
     | _ -> scan str1

(* -- PARSER *)

let expectToken : tokenT -> tokenT list -> tokenT list = 
  fun expected tokens -> 
   match tokens with
   |  [] -> raise (InputEndsButExpected (print_token expected))
   | token1 :: tokens' -> 
       if token1 = expected
       then tokens'
       else raise (TokenSeenButExpected (print_token expected, print_token token1))

let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise (InputEndsButExpected "an identifier")
   | (IdT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (TokenSeenButExpected ("an identifier", print_token token))

let rec parseType : tokenT list -> typeY * tokenT list =  (*incomplete*)
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "a type")
    | (IntT :: tokens1) ->
         (IntY , tokens1)
    | (LparenT :: tokens1) ->
         let (typ1, tokens2) = parseType tokens1 in
        (match tokens2 with
         | [] -> raise (InputEndsButExpected "'list' or '->'")
         | (ListT :: tokens3) -> raise ParserIncomplete
         | (ArrowT :: tokens3) ->
               let (typ2, tokens4) = parseType tokens3 in
                  (FunY (typ1,typ2), expectToken RparenT tokens4)
         | (token :: _) -> raise (TokenSeenButExpected ("'list' or '->'", print_token token)))
    | (token :: _) -> raise (TokenSeenButExpected ("a type", print_token token))

let rec parseExp : tokenT list -> expE * tokenT list =   (*incomplete*)
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "an expression")
    | (IdT s) :: tokens1 ->
         (IdE s, tokens1)
    | (NumT z) :: tokens1 ->
         (NumE z, tokens1)
    | FunT :: tokens1 ->
        let (x, tokens2) = getIdT tokens1 in
        let (t, tokens3) = parseType (expectToken ColonT tokens2) in
        let (e, tokens4) = parseExp (expectToken ArrowT tokens3) in
       (FunE (x,t,e), tokens4)
    | IfT :: tokens1 ->
        let (e0L, tokens2) = parseExp tokens1 in
       (match tokens2 with
        | [] -> raise (InputEndsButExpected "a comparison operator")
        | (EqualT :: tokens3) ->
              let (e0R, tokens4) = parseExp tokens3 in
              let (e1, tokens5) = parseExp (expectToken ThenT tokens4) in
              let (e2, tokens6) = parseExp (expectToken ElseT tokens5) in
             (IfEqE (e0L,e0R,e1,e2), tokens6)
        | (LessT :: tokens3) ->
              let (e0R, tokens4) = parseExp tokens3 in
              let (e1, tokens5) = parseExp (expectToken ThenT tokens4) in
              let (e2, tokens6) = parseExp (expectToken ElseT tokens5) in
             (IfLtE (e0L,e0R,e1,e2), tokens6)
        | (token :: _) -> raise (TokenSeenButExpected ("'=' or '<'", print_token token)))
     | LparenT :: LbracketT :: RbracketT :: ColonT :: tokens1 -> (*Empty List*)
         let (t, tokens2) = parseType tokens1
         in (NilE t, expectToken RparenT tokens2)
     | MatchT :: tokens1 -> raise ParserIncomplete
     | LparenT :: tokens1 -> raise ParserIncomplete
     | token :: _ -> raise (TokenSeenButExpected 
                       ("the start of an expression", print_token token))

let rec parseProg : tokenT list -> progP * tokenT list =   (*incomplete*)
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "a program")
    | LetT :: (IdT x) :: EqualT :: tokens1 ->
         let (e1, tokens2) = parseExp tokens1 in
         let (p, tokens3) = parseProg (expectToken InT tokens2) in
        (LetVarP (x, e1, p), tokens3)
    | LetT :: (IdT f) :: LparenT :: (IdT x) :: ColonT :: tokens1 ->
         let (t, tokens2) = parseType tokens1 in
         let (e, tokens3) = parseExp 
                              (expectToken EqualT 
                                 (expectToken RparenT tokens2)) in
         let (p, tokens4) = parseProg (expectToken InT tokens3) in
       (LetFunP (f, x, t, e, p), tokens4)
    | LetT :: RecT :: (IdT f) :: LparenT :: (IdT x) :: ColonT :: tokens1 ->
         raise ParserIncomplete
    | _ -> let (e, tokens1) = parseExp tokens in
        (ExpP e, tokens1)

let parse : string -> progP =
  fun input_string ->
    let tokens = scan input_string in
    let (prog,tokens1) = parseProg tokens
    in if tokens1 = []
       then prog
       else raise ExtraInput


(* -- ENVIRONMENTS *)

type 'a environment =  identifier -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : identifier -> 'a -> 'a environment -> 'a environment =
  fun new_id a env ->
    fun id -> if id = new_id then a else env id

let retrieveEnv : 'a environment -> identifier -> 'a =
  fun env id -> env id

(* -- TYPING *)

let rec typeE : expE -> (typeY environment) -> typeY =   (*incomplete*)
   fun exp tenv -> 
    match exp with
   | IdE x -> retrieveEnv tenv x
   | NumE n -> IntY
   | FunE (x, tx, e) ->
        let t = typeE e (insertEnv x tx tenv) in
       FunY(tx, t)
   | ApplyE(e1, e2) -> 
        let t1 = typeE e1 tenv in
        let t2 = typeE e2 tenv in
      (match t1 with
       | FunY(t0,t) -> 
            if t2 = t0 then t else raise FunMismatchType
       | _ -> raise FunNotFunType)
   | _ -> raise TypeCheckerIncomplete

let rec typeP : progP -> (typeY environment) -> typeY =   (*incomplete*)
   fun p tenv -> 
    match p with
    | ExpP e -> typeE e tenv
    | LetFunP(f,x,tx,e,p) ->
        let t1 = typeE e (insertEnv x tx tenv) in
       typeP p (insertEnv f (FunY (tx, t1)) tenv) 
    | LetVarP(x,e,p) -> raise TypeCheckerIncomplete
    | LetRecP(f,x,tx,e,t,p) -> raise TypeCheckerIncomplete

(* -- VALUES *)

type value =
   NumV of int
 | ListV of value list
 | ClosureV of identifier * expE * value environment

let rec print_value : value -> string =
  fun v -> 
   match v with
   | NumV v -> string_of_int v
   | ClosureV _ -> raise OutputClosure
   | ListV vs ->
       "[ "^(String.concat " ; " (List.map print_value vs))^" ]"

(* -- EVALUATING *)

let rec evalE : expE -> (value environment) -> value =   (*incomplete*)
  fun exp env ->
   match exp with
   | NumE n -> NumV n
   | IdE x -> retrieveEnv env x
   | FunE (x, tx, exp0) -> ClosureV (x, exp0, env)
   | ApplyE(exp1, exp2) -> 
       (match (evalE exp1 env, evalE exp2 env) with
         | (ClosureV(x,exp0,env0), v2) ->
               evalE exp0 (insertEnv x v2 env0)
         | _ -> raise ApplyNotClosure)
   | _ -> raise InterpreterIncomplete

let rec evalP : progP -> (value environment) -> value =   (*incomplete*)
   fun p env -> 
    match p with
    | LetVarP(x,e,p) ->
        evalP p (insertEnv x (evalE e env) env)
    | ExpP e -> evalE e env
    | _ -> raise InterpreterIncomplete

let run input = 
 try (
  let p = parse input in 
  let t = typeP p initEnv in
  let _ = print_string ("Type is: "^(print_type t)^"\n") in
  let v = evalP p initEnv in
 print_value v )
  with
   | InputEndsButExpected s ->
       "input ends though had expected "^s^"\n"
   | TokenSeenButExpected (s1,s2) ->
       "saw "^s2^" but had expected "^s1^"\n"
   | ExtraInput ->
       "input continues after program is parsed\n"
   | ParserIncomplete ->
       "the parser doesn't yet handle this construct\n"
   | NotDeclared s ->
        "identifier "^s^" used but not declared\n"
   | OperandNotIntegerType ->
       "operand not of integer type\n"
   | FunNotFunType ->
       "function part of application not of function type\n"
   | FunMismatchType ->
       "argument type does not match function type\n"
   | BranchMismatchType ->
       "types of branches do not match\n"
   | MatchNotListType ->
       "what is being matched does not have a list type\n"
   | ConsMismatchType ->
       "second argument to 'cons' not of appropriate type\n"
   | RecConflictType ->
       "body of recursive function not of declared type\n"
   | TypeCheckerIncomplete ->
       "the type checker doesn't yet handle this construct\n"
   | OperandNotInteger ->
       "operand not integer\n"
   | ApplyNotClosure ->
       "function part of application does not evaluate to closure\n"
   | MatchNotList ->
       "what is being matched is not a list\n"
   | ConsWithNonList ->
       "second argument to 'cons' not a list\n"
   | OutputClosure ->
       "a closure is part of what is being returned\n"
   | InterpreterIncomplete ->
       "the type checker doesn't yet handle this construct\n"
