# OCaml Program README

This repository contains an OCaml project that implement a non-trivial system in Ocaml. Including a parser, type checker, and interpreter. We handle different exceptions 

## Prerequisites

Before running the program, ensure that you have the following prerequisites installed on your system:

- OCaml: [Installation instructions](https://ocaml.org/docs/install.html)

## Getting Started

Follow these steps to run the OCaml program:

1. **Download the program and run Ocaml on your terminal**

    ```bash
    ocaml
    ```

2. **Use interpreter.ml in Ocaml(Pay attention to your cmd path be same as the program file path)**

    ```bash
    #use "interpreter.ml" ;;
    ```



## Usage

**Then you are ready to enter some examples same as bellow:**

```bash
let testBinOp = 
   "(2 + 3)" ;;
(*
# run testBinOp ;;
Type is: int
- : string = "5"
*)
  
let testCond = 
   "if 2 < 3 then if 5 < 4 then 7 else 8 else 9" ;;
(*
# run testCond ;;
Type is: int
- : string = "8"
*)

let testRecNat = "let rec h (n:int) = (if 0 < n then (n * (h (n - 1))) else 1 : int) in (h 5)"
run testRecNat ;;
(*
# run testRecNat ;;
Type is: int
- : string = "120"
*)

let testRecList =
"let rec map (f:(int -> int)) =
(fun l:(int list) ->
match l with
| [] -> ([] : (int list))
| (h :: t) -> ((f h) :: ((map f) t))
: ((int list) -> (int list)))
in let add2 (x:int) = (x + 2)
in ((map add2) (7 :: (4 :: ([] : (int list)))))"

run testRecList ;;
(*
# run testRecList ;;
Type is: int list
- : string = "[ 9 ; 6 ]"
*)

```
