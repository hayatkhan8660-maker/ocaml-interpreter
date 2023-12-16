# OCaml Program README

This repository contains an OCaml program that implement a non-trivial system in Ocaml. Including a parser, type checker and interpreter.

## Prerequisites

Before running the program, ensure that you have the following prerequisites installed on your system:

- OCaml: [Installation instructions](https://ocaml.org/docs/install.html)

## Getting Started

Follow these steps to run the OCaml program:

1. **Download the Ocaml program and run Ocaml on your terminal**

    ```bash
    ocaml
    ```

2. **Use interpreter.ml(Pay attention to your cmd path be same as the program file path)**

    ```bash
    #use "interpreter.ml"
    ```



## Usage

Explain how to use the program, including any command-line options, input formats, or example commands.

**Then you are ready to enter some examples same as bellow:**

```bash
let testRecNat = "let rec h (n:int) = (if 0 < n then (n * (h (n - 1))) else 1 : int) in (h 5)"

run testRecNat ;;

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
```
