open BasicOperation
open Matrix

type const = Pi | E
type value = 
  | Float of float
  | Matrix of float list list 
  | MatrixSVD of float list list * float list list * float list list 
  | MatrixDiag of float list list * float list list * float list list 
  | Const of const
  | Emp
  | Vari of string 

type boperation = 
  (* Basic Binary operation*)
  | Neg
  | Plus
  | Minus
  | Times
  | Div
  | Power
  | Rem
  (* Calculus operation*)
  | Deriv (*Multiple*)
  (* Stats operation*)
  | Combin
  | Permut
type uoperation = 
  (*Basic Unary Operation *)
  | Log 
  | Sin 
  | Cos 
  | Tan 
  | Exp 
  (* Linear Algebra operation*)
  | Det
  | Inv
  | Trans
  | Eigenval
  | Eigenvec
  | Diag
  | SVD
  | TR
  | Adjoint
  | Integral (*Multiple*)

type expr  = 
  |Value of value
  |BOperation of boperation
  |UOperation of uoperation
  |LPar
  |RPar

type 'a t = Empty|Val of value| Node of 'a *'a t * 'a t |Par of 'a t

(** [to_rowlst n] transforms string [n] representing a matrix's columns into 
    string list, which separates contents of different rows of matrix
    Requires: [n] is a valid expression of matrix, only contains 
             float and '[' and ']'
    Examples: to_rowlst "" is []
             to_rowlst "[1 2][5]" is ["1 2";"5"]
*)
let to_rowlst n= 
  let trim = String.trim n in 
  (*matrix of rank 0*)
  match trim with 
  |"" -> []
  |_ -> (let split_lst = String.split_on_char '['  trim in 
         match split_lst with 
         |"" :: t -> (let trm_lst = List.map String.trim t in 
                      let f ele = 
                        let to_len = String.length ele -1 in 
                        if (ele.[to_len] = ']') then String.sub ele 0 to_len 
                        else failwith "no ending bracket ]" in 
                      List.map f trm_lst
                     )
         | _ -> failwith "invalid matrix representation -- many [ ")
(** [to_matrix n] transforms string [n] representing a matrix's columns into 
    an expression of type expre, Value(Matrix()) 
    Requires: [n] is a valid expression of matrix, only contains 
             float and '[' and ']'
    Examples: to_matrix "" is Value (Matrix [])
             to_matrix "[1 2 4][9]" is Value (Matrix [[1.; 2.; 4.]; [9.]])
*)
let to_matrix n = 
  let row_lst = to_rowlst n in 
  let split = List.map (fun x -> String.split_on_char ' ' x) row_lst in 
  let trim = List.map (List.filter (fun x -> x <> ""))split in 
  Value(Matrix (List.map (List.map float_of_string )trim))

(**[trans] is the higher order function that takes in a string and return the
   corresponding Operation type. 
   Requires: 
   the input string has to correspond to type [expr]
   Examples:
   [trans "+"] is [BOperation Plus]
   [trans "1"] is [Value 1]*)
let trans = function 
  | "+" -> BOperation Plus
  | "*" -> (BOperation Times)
  | "/" -> (BOperation Div)
  | "-" -> (BOperation Minus) 
  | "C" -> (BOperation Combin)
  | "P" -> (BOperation Permut)
  | "^" -> (BOperation Power)
  | "%"-> (BOperation Rem)
  | "Det" -> (UOperation Det)
  | "Inv" -> (UOperation Inv)
  | "Trans" -> (UOperation Trans)
  | "Eigenval" -> (UOperation Eigenval)
  | "Eigenvec" -> (UOperation Eigenvec)
  | "Diag" -> (UOperation Diag)
  | "SVD" -> (UOperation SVD)
  | "TR" -> (UOperation TR)
  | "Adjoint" -> (UOperation Adjoint)
  | "log" -> (UOperation Log)
  | "sin" -> (UOperation Sin)
  | "cos" -> (UOperation Cos)
  | "tan" -> (UOperation Tan)
  | "exp" -> (UOperation Exp)
  | "Deriv" -> (BOperation Deriv)
  | "Integral" -> (UOperation Integral)
  | "e" -> Value(Float(exp 1.))
  | "pi" -> Value(Float(2. *. acos 0.))
  | "(" -> LPar
  | ")" -> RPar
  | "x" -> Value(Vari("x"))
  | n -> (let len = String.length n in 
          if n.[0] = '[' && n.[len-1] = ']' 
          then to_matrix (String.sub n 1 (len-2))
          else (Value (Float(float_of_string n))) )

(** [has_empty] is the higher order function that takes in [t] and returns
    the boolean value of determining whether the input is empty.
    Requires:
    input is of type [t]
    Examples:
    [has_empty Empty] is true
    [has_empty Value(1)] is false*)
let rec has_empty = function
  | Empty -> true
  | Val(_) -> false
  | Par(n) -> has_empty n
  | Node(v,l,r) -> (has_empty l) || (has_empty r)

(** [has_par tree] is the boolean value of determining whether the input [tree]
    contains parenthesis in it.
    Requires:
    [tree] is a valid tree of type t
    Examples:
    [has_par Empty] is false
    [has_par Par(Value(1))] is true*)
let rec has_par tree = 
  match tree with 
  | Empty -> false
  | Val(_) -> false
  | Par(_) -> true
  | Node(v,l,r)-> (has_par l) || (has_par r)

(** [insert_value acc h] inserts expr [h] into the tree [acc]
 *  requires: [h] is Value(n) of type expr
 *  Examples:
 *    [insert_value Value(1) Empty] is [Value(1)]
 *    [insert_value Value(1) Node(UOperation(Log), Empty, Empty)] is
 *      [Node(UOperation(Log), Value(1), Empty)]*)
(*(acc:expr t) (Value(n):expr) : expr t*)
let rec insert_value accu valu= 
  match valu with 
  |(Value(n)) -> 

    (match accu with 
     |Empty -> (Val(n))
     |(Par(Empty)) -> Par(Val(n))
     |(Par(Node(v,l,Empty))) -> Par(Node(v,l,Val(n)))
     |(Par(x)) -> (Par(insert_value x (Value(n))))
     |(Node(v,l,Empty)) -> Node(v,l,Val(n))
     |(Node(v,l,r)) -> Node(v,l,(insert_value r (Value(n))))
     | _ -> failwith "consecutive value")
  | _ -> failwith "no other datatype"

(**  [insert_par acc] inserts LPar of type expr into the tree [acc]
     Raises:
     ["no empty to insert par"] if there is no empty space inside the 
     inner tree within a pair of parenthesis.
     ["consecutive value & Left-parenthesis"] if insertion is not valid
     Examples:
     [insert_par Node(BOperation(Plus), Value(1), Empty)] is 
      [Node(BOperation(Plus), Value(1), Par(Empty))] 
     [insert_par Empty] is [Par(Empty)]*)
let rec insert_par (acc:expr t) : expr t= 
  match acc with 
  |Empty -> Par(Empty)
  |Par(n) -> 
    if has_empty n then 
      (if n = Empty then Par(Par Empty) else Par(insert_par n))
    else 
      failwith "no empty to insert par"
  (* n *)
  |Node(v,l,Empty) -> Node(v,l,Par(Empty))
  |Node(v,l,r) -> Node(v,l,(insert_par r))
  |_-> failwith "consecutive value & Left-parenthesis"

(**  [remove_par acc] removes Par of type expr from the tree [acc]
 * Requires: [acc] contains Par*)
let rec move_par acc = 
  if not (has_par acc) then failwith "no left par before" else 
    (match acc with 
     | (Par (n)) -> if (not (has_par n)) then n else (Par(move_par n))
     | (Node (v,l,r)) -> (Node(v,l,(move_par r)))
     |_ -> failwith "invalid representation: par should in right"
    )
(** [insert_Uoperation acc p] inserts unary operation [op] 
    into the previous expression tree [acc]
    Requires: 1. [op] is an UOperation 2. [acc] has an Empty space
    Raises: 1. ["not unary operation to be inserted"] if [op] is 
              not an of type UOperation
          2. ["no empty space for unary operation"] if [acc] doesn't 
              contain Empty 
    Examples: insert_Uoperation Emp UOperation(Tan) is 
            Node(UOperation(Tan),Val(Emp),Empty) *)
let rec insert_Uoperation acc p = 
  match p with 
  |(UOperation(op)) -> 
    (match acc with 
     | Empty -> Node(p,Val(Emp),Empty)
     | Par(Empty) -> Par(Node(p,Val(Emp),Empty))
     | Par(Node(v,l,Empty)) -> Par(Node(v,l,Node(p,Val(Emp),Empty)))
     | Par(x) -> Par(insert_Uoperation x p)
     | Node(v,l,Empty) -> Node(v,l,Node(p,Val(Emp),Empty))
     | Node(v,l,r) -> Node(v,l,(insert_Uoperation r p))
     |_ -> failwith "no empty space for unary operation")
  |_ -> failwith "not unary operation to be inserted"

(** [insert_option acc BOperation(op)] inserts expr 
    [BOperation(op)] into the tree [acc]

 *  Requires:[acc] can't contain Empty 
 *  Raises:
    ["invalid representation"] if [acc] is not a valid representation of tree
    ["not binary operation to be inserted"] if [p] is not a binary operation
 *  Example:
    [insert_Boperation Val(1) BOperation(Plus)] is 
      [Node(BOperation(Plus), Val(1), Empty)]*)
let rec insert_Boperation acc p = 
  match p with 
  | (BOperation(op)) -> 
    (match acc with
     | Val(n) -> Node(p,acc,Empty)
     | Par(n) -> if (has_par n) then Par(insert_Boperation n p)
       else Par(Node(p,n,Empty)) 
     | Node(v,l,r) -> if (not(has_par acc)) 
       then (Node(p,acc,Empty))
       else (Node(v,l,(insert_Boperation r p)))
     | _-> failwith "invalid representation")
  | _ ->failwith "not binary operation to be inserted"

let number_lst = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '.']

let op_lst = ["C"; "P"; "+"; "+." ; "-"; "-."; "*"; "*."; "/"; "/."; 
              "^"; "^."; "log"; "sin"; "cos"; "tan"; "("; ")"; "det"] 
let op_3_start_lst = ['s'; 'c'; 'l'; 't'; 'S'; 'I']

(** [extend_num s acc] is the length of the initial number in [s] 
    so that [s].sub 0 idx is the initial number

    Examples: [extend_num "1.23+3"] is 4 *)
let rec extend_num s acc = 
  let len = String.length s in 
  if len = 0 then acc 
  else
    let cur = s.[0] in 
    if List.mem cur number_lst then extend_num (String.sub s 1 (len-1)) (acc+1)
    else acc 

(** [extend_matrix s acc st] is the length of the initial matrix in [s] 
    so that [s].sub 0 idx is the ']' character

    Examples: [extend_matrix "1,2,3]" 0 1] is 6
            [extend_matrix "[1,2,3]]" 0 1] is 8 
    Requires: there must be a ']' in the string *)
let rec extend_matrix s acc st= 
  let len = String.length s in 
  if st = 0 then acc
  else if len = 0 then failwith "wrong matrix format"
  else
    let cur = s.[0] in 
    if Char.equal cur '[' then 
      extend_matrix (String.sub s 1 (len-1)) (acc+1) (st+1)
    else if Char.equal cur ']' then 
      extend_matrix (String.sub s 1 (len-1)) (acc+1) (st-1)
    else extend_matrix (String.sub s 1 (len-1)) (acc+1) st 
let empty_char_lst = [' '; '\t'; '\n']

(** [preprocess s acc] is a list of numbers or operations in [s]. Order is 
    preserved. [acc] is the accumulator.

    Examples: [preprocess "1+2"] is ["1"; "+"; "2"] *)
let rec preprocess s acc =
  let len = String.length s in 
  if len = 0 then List.rev acc 
  else
    let cur = s.[0] in 
    let rest = String.sub s 1 (len-1) in 
    if List.mem cur empty_char_lst then preprocess rest acc
    else if List.mem cur number_lst then
      let end_idx = extend_num s 0 in 
      let new_rest = String.sub s end_idx (len - end_idx) in 
      let item = String.sub s 0 end_idx in 
      preprocess new_rest (item::acc)
    else if List.mem cur op_3_start_lst then
      let new_rest = String.sub s 3 (len-3) in 
      let item = String.sub s 0 3 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur 'p' then 
      let new_rest = String.sub s 2 (len-2) in 
      let item = String.sub s 0 2 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur 'T' then 
      let new_rest = String.sub s 2 (len-2) in 
      let item = String.sub s 0 2 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur 'A' then 
      let new_rest = String.sub s 7 (len-7) in 
      let item = String.sub s 0 7 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur 'E' then
      let new_rest = String.sub s 8 (len-8) in 
      let item = String.sub s 0 8 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur 'T' then
      let new_rest = String.sub s 5 (len-5) in 
      let item = String.sub s 0 5 in 
      preprocess new_rest (item::acc)
    else if Char.equal cur '[' then
      let end_idx = 1 + extend_matrix (String.sub s 1 (len-1)) 0 1 in 
      let new_rest = String.sub s end_idx (len - end_idx) in 
      let item = String.sub s 0 end_idx in 
      preprocess new_rest (item::acc)
    else if len > 1 && Char.equal cur 'e' && Char.equal s.[1] 'x' then
      let new_rest = String.sub s 3 (len-3) in 
      let item = String.sub s 0 3 in 
      preprocess new_rest (item::acc)
    else if len > 2 && Char.equal cur 'D' && Char.equal s.[1] 'e'
            && Char.equal s.[2] 't' then
      let new_rest = String.sub s 3 (len-3) in 
      let item = String.sub s 0 3 in 
      preprocess new_rest (item::acc)
    else if len > 2 && Char.equal cur 'D' && Char.equal s.[1] 'e'
            && Char.equal s.[2] 'r' then
      let new_rest = String.sub s 5 (len-5) in 
      let item = String.sub s 0 5 in 
      preprocess new_rest (item::acc)
    else if len > 1 && Char.equal cur 'D' && Char.equal s.[1] 'i' then
      let new_rest = String.sub s 4 (len-4) in 
      let item = String.sub s 0 4 in 
      preprocess new_rest (item::acc)
    else
      preprocess rest ((Char.escaped cur)::acc)

(** [find_fst lst acc strlst] is a tuple [l], [r], [c]. [lst] is a list of 
    elements to find in string list [strlst]. [acc] is the accumulator. [c] is
    one of the character in [lst] that first occurs in [strlst]. [l] is the 
    sublist of [strlst] to the left of [c], and [r] is the sublist to the right
    of [c].

    Examples: [find_fst ["+"; "-"] [] ["1"; "+"; "2"; "-"; "3"]] is 
            ["1"], "+", ["2"; "-"; "3"] 

    Raises: Failure("violate find_fst precondition") if there is no elements 
          of [lst] in [strlst] *)
let rec find_fst lst acc strlst = 
  match strlst with
  | [] -> failwith "violate find_fst precondition"
  | h::t -> 
    if List.mem h lst then acc, t, h
    else find_fst lst (List.rev(h::(List.rev acc))) t 

(** [put_par target lst left stack] is inserting [target] in the correct place
    in [lst]. [left] is the accumulator. [stack] record the number of the 
    opposite parenthesis left to find. Stack should be initialized to -1.

    Examples: [put_par "(" ["(";"2";"+";"3";")";"+";"4"] [] -1] is 
            ["(";"2";"+";"3";")";")";"+";"4"]

    Requires: [target] and [lst] are not empty lists.
            [lst] is in valid input format
            [lst] contains at least one opposite parenthesis

    Raises: Failure("violate put_par precondition") if [lst] is empty *)
let rec put_par target lst left stack = 
  let other_par = if target = "(" then ")" else "(" in 
  match lst with 
  | [] -> failwith "violate put_par precondition"
  | h::t -> 
    let new_left = List.rev (h::(List.rev left)) in
    if String.equal target h then 
      if stack = 0 then left@(h::target::t)
      else put_par target t new_left (stack-1)
    else if String.equal other_par h then 
      put_par target t new_left (stack+1)
    else put_par target t new_left stack

(** [put_left_par left] is putting "(" in the correct place in list [left]

    Examples: [put_left_par ["(";"2";"+";"3";")";"+";"4"]] is 
            ["(";"2";"+";"3";")";"+";"(";"4"]
            [put_left_par ["2";"+";"(";"3";"+";"4";")"]] is 
            ["2";"+";"(";"(";"3";"+";"4";")"]

    Requires: [left] is in valid input format and is not empty 

    Raises: Failure("violate put_left_par precondition") if [left] contains no
          ")" or is empty
*)
let put_left_par left = 
  let new_left = List.rev left in 
  match new_left with 
  | [] -> failwith "violate put_left_par precondition"
  | h::t ->
    if not (String.equal h ")") then List.rev (h::"("::t)
    else put_par "(" new_left [] (-1) |> List.rev

(** [put_right_par right] is putting ")" in the correct place in list [right]

    Examples: [put_right_par ["(";"2";"+";"3";")";"+";"4"]] is 
            ["(";"2";"+";"3";")";"(";"+";"4"]
            [put_right_par ["2";"+";"(";"3";"+";"4";")"]] is 
            ["2";")";"+";"(";"3";"+";"4";")"]]

    Requires: [right] is in valid input format and is not empty 

    Raises: Failure("violate put_right_par precondition") if [left] contains no
          "(" or is empty
*)
let put_right_par right = 
  match right with 
  | [] -> failwith "violate put_left_par precondition"
  | h::t ->
    if not (String.equal h "(") then h::")"::t
    else put_par ")" right [] (-1)

(** [organize str_lst lst] is the orgainized string list [lst] according to the
    characters in [str_lst], changing the organized characters [x] into [x.]

    Examples: [organize ["*"] ["1";"+";"2";"*";"3"]] is 
            ["1";"+";"(";"2";"*";"3";")"]

    Requires: [str_lst] and [lst] are not empty.
            [lst] is in valid input format.   *)
let rec organize str_lst lst =
  let is_high_order acc str = 
    if List.mem str str_lst then acc + 1
    else acc 
  in 
  let total = List.fold_left is_high_order 0 lst in 
  if total = 0 then lst 
  else 
    let l,r,c = find_fst str_lst [] lst in 
    let new_l = put_left_par l in 
    let new_r = put_right_par r in 
    organize str_lst (new_l@((c^".")::new_r))

(** [organize_minus lst] is the orgainized string list [lst] changing the 
    organized unary minus - into -.

    Examples: [organize_minus ["1";"+";"-";"3"]] is 
            ["1";"+";"(";"0";"-";"3";")"]

    Requires: [lst] is not empty.
            [lst] is in valid input format. *)
let rec organize_minus (lst:string list) =
  let is_high_order acc str = 
    if List.mem str ["-"] then acc + 1
    else acc 
  in 
  let total = List.fold_left is_high_order 0 lst in 
  if total = 0 then lst 
  else 
    let l,r,c = find_fst ["-"] [] lst in 
    match List.rev l with 
    | [] -> 
      let new_l = ["("; "0"] in 
      let new_r = put_right_par r in 
      organize_minus (new_l@(("-"^".")::new_r))
    | h::t ->
      if List.mem h op_lst && not (String.equal h ")") then
        let new_l = List.rev ("0"::"("::(List.rev l)) in 
        let new_r = put_right_par r in 
        organize_minus (new_l@(("-"^".")::new_r))
      else
        organize_minus (l@(("-"^".")::r))

(** [organize_op lst] is the orgainized string list [lst] changing the 
    organized sin cos tan and log into sin. cos. tan. and log. 

    Examples: [organize_minus ["sin";"1";"-";"3"]] is 
            ["(";"sin";"1";")";"-";"3"]

    Requires: [lst] is not empty.
            [lst] is in valid input format. *)
let rec organize_op str_lst lst =
  let is_high_order acc str = 
    if List.mem str str_lst then acc + 1
    else acc 
  in 
  let total = List.fold_left is_high_order 0 lst in 
  if total = 0 then lst 
  else 
    let l,r,c = find_fst str_lst [] lst in 
    let new_l = List.rev ("("::(List.rev l)) in 
    let new_r = put_right_par r in 
    organize str_lst (new_l@((c^".")::new_r))

(** [change_back lst] changes the "*.", "/.", "^.", "-." back into "*", "/"
    "^", and "-" in list [lst]

    Examples:
    [change_back ["*."]] is [*]
    [change_back []] is []
    [change_back ["2", "/."]] is ["2", "/"]
*)
let change_back lst = 
  let f str = 
    if String.equal "*." str then "*"
    else if String.equal "/." str then "/"
    else if String.equal "^." str then "^"
    else if String.equal "-." str then "-"
    else if String.equal "C." str then "C"
    else if String.equal "P." str then "P"
    else if String.equal "sin." str then "sin"
    else if String.equal "cos." str then "cos"
    else if String.equal "tan." str then "tan"
    else if String.equal "log." str then "log"
    else if String.equal "det." str then "det"
    else str 
  in 
  List.map f lst 

let parse (s:string) : expr t  =
  let match_lst = 
    preprocess s [] 
    |> organize ["C"; "P"]
    |> organize ["^"]
    |> organize ["*"; "/"]
    |> organize_op ["sin"; "cos"; "tan"; "log"]
    |> organize_minus
    |> organize_op ["det"]
    |> change_back 
    |> List.map trans 
  in
  let op acc (h:expr):'a t = 
    match h with 
    | Value(n) -> insert_value acc h
    | LPar -> insert_par acc
    | RPar -> move_par acc
    | BOperation(n) -> insert_Boperation acc h 
    | UOperation(n) -> insert_Uoperation acc h
  in 
  List.fold_left (op) (Empty) (match_lst) 

(** [discard_dot i] discards the last character if it is '.'
    Examples:
    discard_dot "1." = "1"
    discard_dot "1.2" = "1.2"
*)
let discard_dot i = 
  let len_i = String.length i in
  if Char.equal i.[len_i-1] '.' then String.sub i 0 (len_i-1)
  else i

(** [matrix_step_match op lst fst_s snd_s] is a higher level function 
    which executes the operation [op] to each element of the list [lst] 
    and concats the result of each element with [fst_s] at the front 
    and [snd_s] at the end
    Examples: let v1 = Float 1.
            let v2 = Float 2.
    matrix_step_match match_value [v1;v2] "(" ")"  is "(1)(2)"*)
let matrix_step_match op lst fst_s snd_s= 
  let f acc x = acc ^ " "^(op x) in 
  let rst = List.fold_left f "" lst in 
  fst_s ^ rst ^ snd_s

(** [match_value v] is matching value [v] into a string expression
    Examples: match_value Float(2.) is "2."
            match_value Emp is ""*)
let rec match_value = function
  | Float(n) when n = exp 1. -> "e"
  | Float(n) -> string_of_float n |> discard_dot
  | Matrix(n) ->(
      let match_list lst = matrix_step_match string_of_float lst "[" " ]"in
      matrix_step_match match_list n "|" " |"
    )
  | Emp -> ""
  | Vari(x) -> x
  | _ -> failwith "undefined value"

(** [match_uoperation u] is matching uoperation [u] into a string expression
    Examples: match_uoperation Log is "log"
            match_uoperation Eigenval is "Eigenval"*)
let match_uoperation = function
  | Log -> "log"
  | Sin -> "sin"
  | Cos -> "cos"
  | Tan -> "tan"
  | Exp -> "exp"
  (* Linear Algebra operation*)
  | Det -> "Det" 
  | Inv -> "Inv"
  | SVD -> "SVD"
  | Trans -> "Trans"
  | Eigenval -> "Eigenval"
  | Eigenvec -> "Eigenvec"
  | Diag -> "Diag"
  | TR -> "TR"
  | Adjoint -> "Adjoint"
  | Integral -> "Integral"

(** [match_boperation b] is matching boperation [b] into a string expression
    Examples: match_boperation Neg is "-"
            match_boperation Rem is "%"*)
let match_boperation = function
  | Neg -> "-"
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Div -> "/"
  | Power -> "^"
  | Rem -> "%"
  | Deriv -> "Deriv"
  (* Calculus operation
     | Deriv (*Multiple*)
     | Integral (*Multiple*)
  *)
  (* Stats operation*)
  | Combin -> "C"
  | Permut -> "P"
(* |_ -> failwith "undefined boperation now" *)

(** [match_op op] is matching operation [op] into a string expression
    Examples: match_boperation Neg is "-"
            match_boperation Rem is "%"
            match_uoperation Log is "log"
            match_uoperation Eigenval is "Eigenval"*)
let match_op = function
  | UOperation(n) -> match_uoperation n 
  | BOperation(n) -> match_boperation n 
  | _ -> failwith "invalid node value other than operation"

(** [need_par op1 op2] checks if two perations [op1] and [op2]
    needs parentheses to connect 
    Examples: need_par BOperation Times BOperation Power is false
            need_par BOperation Times, BOperation Times is true*)
let need_par op1 op2 = 
  match op1, op2 with 
  | BOperation Times, BOperation Power -> false
  | BOperation Div, BOperation Power -> false
  | BOperation Plus, BOperation Power -> false
  | BOperation Plus, BOperation Times -> false
  | BOperation Plus, BOperation Div -> false
  | BOperation Minus, BOperation Power -> false
  | BOperation Minus, BOperation Times -> false
  | BOperation Minus, BOperation Div -> false
  | _,_ -> true

(** [format tree] is formating the AST [tree] into string 
    Examples: format Val(Value(Float 2.)) is "2."
            format Node(UOperation Tan, Val(Value(Emp)),Val(Value(Float 3.))) 
            is tan 3.*)
let rec format (tree: expr t) : string = 
  match tree with
  | Val(n) -> match_value n
  | Node(v,l,r) -> begin
      match l,r with 
      | Node(op,a,b),Val _ -> 
        if need_par v op then "("^(format l) ^")"^ (match_op v) ^ (format r)
        else (format l) ^ (match_op v) ^ (format r)
      | Val _, Node(op,a,b) -> 
        if need_par v op then (format l) ^ (match_op v) ^"("^ (format r)^")"
        else (format l) ^ (match_op v) ^ (format r)
      | Node(op1,a1,b1),Node(op2,a2,b2) ->
        let need_1, need_2 = need_par v op1, need_par v op2 in
        if need_1 && need_2 then
          "("^(format l) ^")" ^ (match_op v) ^"("^ (format r)^")"
        else if need_1 then 
          "("^(format l) ^")"^ (match_op v) ^ (format r)
        else if need_2 then 
          (format l) ^ (match_op v) ^"("^ (format r)^")"
        else (format l) ^ (match_op v) ^ (format r)
      | Val _, Val _ -> (format l) ^ (match_op v) ^ (format r)
      | _, _ -> "("^(format l) ^")" ^ (match_op v) ^"("^ (format r)^")"
    end
  | _ -> failwith "involving Empty or Par in final tree "

(**[evaluate_helper t] evaluates the AST [t] into a simplified
   AST representing the final results by calling the corresponding 
   functions of operation
   Examples: evaluate_helper Val((Float 3.)) is Val((Float 3.)) 
           let t2 = Node(BOperation Rem, Val (Float 5.), Val (Float 3.))
           evaluate_helper t2 is Val((Float 0.) *)
let rec evaluate_helper t = 
  (match t with 
   | Val Vari(x) -> failwith "cannot evaluate variable"
   | Val v -> Val v 
   (* | Node ((BOperation Deriv), _, Val(n)) -> deriv1 t1 t2 *)
   | Node (BOperation op, Val (Float val1), Val (Float val2)) -> 
     (match op with 
      | Plus -> Val (Float (plus val1 val2))
      | Minus -> Val (Float (minus val1 val2))
      | Times -> Val (Float (times val1 val2))
      | Div -> Val (Float (divide val1 val2))
      | Power -> Val (Float (power val1 val2))
      | Combin -> Val (Float (combination val1 val2))
      | Permut -> Val (Float (permutation val1 val2))
      | Rem -> Val (Float (remainder val1 val2))
      | _ -> failwith "not support this function " )
   (*Basic Operation of Matries with 2 args of Matrix *)
   | Node (BOperation op, Val (Matrix m1), Val (Matrix m2)) -> 
     (match op with 
      | Plus -> Val (Matrix(mplus m1 m2))
      | Minus -> Val (Matrix(mminus m1 m2))
      | Times -> Val (Matrix (mtimes_mm m1 m2))
      | _ -> failwith "not support this function " )
   (*Basic Operation of Matries with 1 arg of Matrix and 1 arg of Float*)
   | Node (BOperation op, Val (Matrix m1), Val (Float val2)) -> 
     (match op with 
      | Times -> Val (Matrix (mtimes_mv m1 val2))
      | Div -> Val (Matrix (mdivide_mv m1 val2))
      | Power -> Val (Matrix (mpower m1 val2))
      | _ -> failwith "not support this function " )
   (*Basic Operation of Matries with 1st arg of Float and 2nd arg of Matrix*)
   | Node (BOperation op, Val (Float val1), Val (Matrix m2)) -> 
     (match op with 
      | Times -> Val (Matrix (mtimes_vm val1 m2))
      | _ -> failwith "not support this function " ) 

   | Node (BOperation op, t1, t2) -> 
     evaluate_helper (Node 
                        (BOperation op, evaluate_helper t1, evaluate_helper t2))
   (* | Node((UOperation Integral),Val(Emp),tr) -> integrate tr *)
   | Node (UOperation op, Val(Emp), Val(Float val2)) -> 
     (match op with 
      | Log -> Val(Float(log val2))
      | Sin -> Val(Float(sin val2))
      | Cos -> Val(Float(cos val2))
      | Tan -> Val(Float(tan val2))
      | Exp -> Val(Float(exp val2))
      | _ -> failwith "matrix unary operation shouldn't be here")

   | Node (UOperation op, Val(Emp), Val(Matrix m2))->
     (match op with
      | Det -> Val(Float(det m2))
      | Trans -> Val(Matrix(transpose m2))
      | Inv -> Val(Matrix(inv m2))
      | Eigenval -> Val(Matrix(eigval m2))
      | Eigenvec -> Val(Matrix(eigvec m2))
      | Diag -> 
        (match diag m2 with 
         | (p,l,invp) -> Val(MatrixDiag(p,l,invp)))
      | SVD -> (match diag m2 with 
          | (s,v,d) -> Val(MatrixSVD(s,v,d)))
      | TR -> Val(Float(tr m2))
      | Adjoint -> Val(Matrix(adjoint m2))
      | _ -> failwith "algebra unary operation shouldn't be here")

   | Node (UOperation op, Val(Emp), t) ->
     evaluate_helper (Node (UOperation op, Val(Emp), evaluate_helper t))
   | _ -> failwith "shouldn't have this kind of tree ! ")

(** [evaluate ast] turns ast of type expr t into a value type
    Examples: evaluate Val (Float 3.) is Float 3.
            evaluate Val(Matrix [[1 2 3][6 9 10]]) is 
            Matrix [[1 2 3][6 9 10]]*)
let evaluate ast = 
  match evaluate_helper ast with 
  | Val (Float num) -> Float num
  | Val (Matrix matrix) -> Matrix matrix
  | Val(MatrixSVD(s,v,d)) -> MatrixSVD(s,v,d)
  | Val(MatrixDiag(s,v,d)) -> MatrixDiag(s,v,d)
  | _ -> failwith "invalid simplified tree! "

(** [match_num n] is matching number [n] from a string expression into int
    Requires: [n] is a string expression of int 
    Examples: match_num "1" is 1
            match_num "9" is 9*)
let match_num = function 
  | "1" -> 1
  | "2" -> 2 
  | "3" -> 3 
  | "4" -> 4
  | "5" -> 5
  | "6" -> 6
  | "7" -> 7 
  | "8" -> 8
  | "9" -> 9
  | _ -> 0

(** [truncate n p'] is the truncated result of n to pth decimal place.
    Examples: 
    truncate 1.1  1  = "1.1"
    truncate 1.89 1  = "1.8"
    truncate 1.   99 = "1." 
*)
let truncate n p' = 
  let s = string_of_float n in
  if not (String.contains s '.') then s
  else 
    let pos = String.index s '.' in
    if pos + p' + 1 >= String.length s then s 
    else String.sub s 0 (pos + p' + 1)

let trim_result_precision num p = 
  let s = string_of_float num in
  if not (String.contains s '.') then s
  else if not (String.contains s 'e') then
    (let pos = String.index s '.' in
     let rest_len = (String.length s) - (pos + 1) in
     if (rest_len <= p) then s 
     else 
       let next = (String.sub s (pos + p + 1) 1) |> int_of_string in
       if next < 5 then String.sub s 0 (pos + p + 1) 
       else truncate (num +. 0.1 ** (float_of_int p)) p)
    (** the given input has e *)
  else 
    let e_pos = String.index s '-' in
    let after_e = String.sub s (e_pos + 1) ((String.length s)- e_pos - 1)
                  |> int_of_string in
    if p < after_e - 1 then "0."
    else 
      string_of_float (num +. (0.1 ** (float_of_int ((after_e + 1)))))

(** [largest_len m] is the largest string length of all element in 
    the matrix [m]. 

    Example: 
              | ̅  -2333 9   ̅|
    largest_len |_  8     0  _| = 4
    largest_len []              = 0
*)
let largest_len m = 
  let largest_len_row r = 
    List.fold_left (fun acc x -> 
        let cur_len = (String.length (string_of_float x)) in
        if cur_len > acc then cur_len 
        else acc) 0 r in
  List.fold_left (fun acc x -> 
      let cur_len = largest_len_row x in
      if cur_len > acc then cur_len 
      else acc) 0 m

(** [add_to_len s target_len] add additional space after [s] to make the 
    new string of length [target_len]. 

    Requires: 
    the length of [s] should be less than or equal to [target_len].

    Examples: 
    add_to_len "add" 5 = "add  "
    add_to_len "something" 9 = "something"
*)
let add_to_len s target_len = 
  let len_s = String.length s in
  let add_on = String.make (target_len - len_s) ' ' in
  s ^ add_on 

(** [matrix_formatter nested_lst] is the hand-written style of [nested_lst].

    Examples: 
                                              |̅ 2313214  -2312     ̅|
    matrix_formatter [[2313214 -2312] [2 0]] =  |_ 2        0       _|

*)
let matrix_formatter nested_lst accuracy = 
  let ele_len = largest_len nested_lst in
  let row_helper lst : string = 
    List.fold_left (fun acc x -> 
        (acc^(add_to_len 
                (discard_dot (trim_result_precision x accuracy)) ele_len)^"  ")) " " lst 
  in
  let len = List.fold_left (fun acc x -> acc + 1) 0 nested_lst in
  let first_line first = "| ̅" ^ (row_helper first) ^" ̅|\n" in
  let last_line last = "|_" ^ (row_helper last) ^"_|\n" in
  let mid_line mid = "| " ^ (row_helper mid) ^" |\n" in
  let only_line first = "[" ^ (row_helper first) ^"]\n" in
  let temp = List.fold_left (fun init x -> 
      let correct_line x = 
        if (snd init = 0) && (len = 1) then only_line x
        else if ((snd init = 0) && (len <> 1)) then first_line x 
        else if snd init = len - 1 then last_line x 
        else mid_line x in
      ((fst init^( correct_line x)), ((snd init) + 1))) ("", 0) nested_lst in
  fst temp

let result_formatter x accuracy print_tag = 
  match x with 
  | (Float f) -> 
    let flt = trim_result_precision f accuracy in 
    discard_dot flt
  | (Matrix n) -> matrix_formatter n accuracy
  | (MatrixDiag (s,v,d)) -> "\nP = \n"^(matrix_formatter s accuracy) ^
                            "\nA = \n"^(matrix_formatter v accuracy) ^
                            "\nInvP = \n"^(matrix_formatter d accuracy) 
  | (MatrixSVD (s,v,d)) -> "\nS = \n"^(matrix_formatter s accuracy) ^
                           "\nV = \n"^(matrix_formatter v accuracy) ^
                           "\nD = \n"^(matrix_formatter d accuracy) 
  | _ -> failwith "unimplemented"

let substitute eq num = 
  let f acc x = 
    if String.equal "x" x then acc^num 
    else if String.equal "C" x then acc^"0"
    else acc^x
  in 
  preprocess eq []
  |> List.fold_left f ""

let rec simplify_1 tree = 
  match tree with
  | Par(t) -> begin 
      match simplify_1 t with 
      | Node(a,b,c) -> Par(Node(a,b,c))
      | t -> t
    end
  | Node(op,l,r) -> begin
      try evaluate_helper (Node(op,l,r))
      with 
        _ -> 
        Node(op, simplify_1 l, simplify_1 r)
    end
  | t -> t

let rec simplify = function
  | Par(t) -> begin
      match simplify t with 
      | Node(a,b,c) -> Par(Node(a,b,c))
      | a -> a
    end
  (* Plus Plus *)
  | Node(BOperation(Plus),Node(BOperation(Plus),a,b),c) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f1+.f2+.f3))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Plus),n1,Val(Float(f2+.f3))))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Plus),n2,Val(Float(f1+.f3))))
      | Val(Float f1), Val(Float f3), n2 -> 
        simplify (Node(BOperation(Plus),n2,Val(Float(f1+.f3))))
      | n1,n2,n3 -> Node(BOperation(Plus),Node(BOperation(Plus),n1,n2),n3)
    end
  | Node(BOperation(Plus),c,Node(BOperation(Plus),a,b)) -> 
    simplify (Node(BOperation(Plus),Node(BOperation(Plus),a,b),c))
  (* Plus Minus *)
  | Node(BOperation(Plus),Node(BOperation(Minus),a,b),c) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f1-.f2+.f3))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Plus),n1,Val(Float(f3+.f2))))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Minus),Val(Float(f1+.f3)),n2))
      | Val(Float f1), Val(Float f2), n3 -> 
        simplify (Node(BOperation(Plus),Val(Float(f1-.f2)), n3))
      | n1,n2,n3 -> Node(BOperation(Plus),Node(BOperation(Minus),n1,n2),n3)
    end
  | Node(BOperation(Plus), c, Node(BOperation(Minus),a,b)) -> 
    simplify (Node(BOperation(Plus), Node(BOperation(Minus),a,b), c))
  (* Minus Plus *)
  | Node(BOperation(Minus),Node(BOperation(Plus),a,b),c) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f1+.f2-.f3))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Plus),n1,Val(Float(f2-.f3))))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Plus),Val(Float(f1-.f3)),n2))
      | Val(Float f1), Val(Float f2), n3 -> 
        simplify (Node(BOperation(Minus),Val(Float(f1+.f2)), n3))
      | n1,n2,n3 -> Node(BOperation(Minus),Node(BOperation(Plus),n1,n2),n3)
    end
  | Node(BOperation(Minus), c, Node(BOperation(Plus),a,b)) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f3-.f1+.f2))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Minus),Val(Float(f3-.f2)),n1))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Minus),Val(Float(f3-.f1)),n2))
      | Val(Float f1), Val(Float f2), n3 -> 
        simplify (Node(BOperation(Minus), n3, Val(Float(f1+.f2))))
      | n1,n2,n3 -> Node(BOperation(Minus),n3, Node(BOperation(Plus),n1,n2))
    end
  (* Minus Minus *)
  | Node(BOperation(Minus),Node(BOperation(Minus),a,b),c) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f1-.f2-.f3))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Minus),n1,Val(Float(f2+.f3))))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Minus),Val(Float(f1-.f3)),n2))
      | Val(Float f1), Val(Float f2), n3 -> 
        simplify (Node(BOperation(Minus),Val(Float(f1-.f2)), n3))
      | n1,n2,n3 -> Node(BOperation(Minus),Node(BOperation(Minus),n1,n2),n3)
    end
  | Node(BOperation(Minus),c,Node(BOperation(Minus),a,b)) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f3-.f1+.f2))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Minus),Val(Float(f2+.f3)), n1))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Plus),Val(Float(f3-.f1)),n2))
      | Val(Float f1), Val(Float f2), n3 -> 
        simplify (Node(BOperation(Minus),n3,Val(Float(f1+.f2))))
      | n1,n2,n3 -> Node(BOperation(Minus),n3,Node(BOperation(Minus),n1,n2))
    end
  (* Times Times *)
  | Node(BOperation(Times),Node(BOperation(Times),a,b),c) -> begin
      match simplify a, simplify b, simplify c with 
      | Val(Float f1), Val(Float f2), Val(Float f3) -> Val(Float(f1*.f2*.f3))
      | n1, Val(Float f2), Val(Float f3) ->
        simplify (Node(BOperation(Times),n1,Val(Float(f2*.f3))))
      | Val(Float f1), n2, Val(Float f3) -> 
        simplify (Node(BOperation(Times),n2,Val(Float(f1*.f3))))
      | Val(Float f1), Val(Float f3), n2 -> 
        simplify (Node(BOperation(Times),n2,Val(Float(f1*.f3))))
      | n1,n2,n3 -> Node(BOperation(Times),Node(BOperation(Times),n1,n2),n3)
    end
  | Node(BOperation(Times),c,Node(BOperation(Times),a,b)) -> 
    simplify (Node(BOperation(Times),Node(BOperation(Times),a,b),c))
  | Node(op,a,b) -> 
    Node(op, a |> simplify_1 |> simplify, b |> simplify_1 |> simplify)
  | t -> simplify_1 t

let rec reduce (t:expr t) = 
  match t with 
  | Node(op, l, r) -> 
    begin
      match op with 
      | BOperation Plus -> 
        if l = Val(Float 0.) || reduce l = Val(Float 0.) then reduce r
        else if r = Val(Float 0.)|| reduce r = Val(Float 0.) then reduce l
        else Node(op, reduce l, reduce r)
      | BOperation Minus -> 
        if r = Val(Float 0.) || reduce r = Val(Float 0.)then reduce l
        else Node(op, reduce l, reduce r)
      | BOperation Times ->
        if l = Val(Float 0.) || reduce l = Val(Float 0.)then Val (Float 0.)
        else if r = Val(Float 0.) || reduce r = Val(Float 0.) then Val (Float 0.)
        else if l = Val (Float 1.) || reduce l = Val (Float 1.)then reduce r
        else if r = Val(Float 1.) || reduce r = Val(Float 1.)then reduce l
        else Node(op, reduce l, reduce r)
      | BOperation Div ->
        if l = Val (Float 0.) || reduce l = Val (Float 0.) then Val (Float 0.)
        else if r = Val(Float 1.) || reduce r = Val(Float 1.) then reduce l
        else Node(op, reduce l, reduce r)
      | BOperation Power ->
        if l = Val (Float 0.) || l = Val (Float 1.) then l
        else if reduce l = Val (Float 0.) || reduce l = Val (Float 1.) then reduce l
        else if r = Val (Float 0.)|| reduce r = Val (Float 0.) then Val (Float 1.)
        else if r = Val(Float 1.) || reduce r = Val (Float 1.)then reduce l
        else Node(op, reduce l, reduce r)
      | UOperation Log ->
        if l = Val(Float 1.) || reduce l = Val(Float 1.) then Val (Float 0.)
        else Node(op, reduce l, reduce r)
      | _ -> Node(op, reduce l, reduce r)
    end
  | t -> t

let rec combine = function
  | Par(t) -> begin
      match combine t with 
      | Node(a,b,c) -> Par(Node(a,b,c))
      | a -> a
    end
  | Node(BOperation Plus,a,b) -> begin 
      match combine a, combine b with
      | Val(Vari x), Val(Vari y) -> 
        if String.equal x y then Node(BOperation Times, Val(Float 2.0), Val(Vari x))
        else Node(BOperation Plus,Val(Vari x), Val(Vari y))
      (* Plus Var *)
      | Node(BOperation Plus, Val (Vari x), c1), Val(Vari y)
      | Node(BOperation Plus, c1, Val (Vari x)), Val(Vari y)
      | Val(Vari y), Node(BOperation Plus, Val (Vari x), c1)
      | Val(Vari y), Node(BOperation Plus, c1, Val (Vari x)) 
        ->
        if String.equal x y then 
          Node(BOperation Plus, Node(BOperation Times, Val(Float 2.), Val(Vari x)),c1)
        else Node(BOperation Plus,combine a, combine b)
      (* Plus Plus *)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Plus,Val(Vari y),c2)
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Plus,Val(Vari y),c2)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Plus,c2,Val(Vari y))
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Plus,c2,Val(Vari y))
        -> 
        if String.equal x y then 
          Node(BOperation Plus, Node(BOperation Times, Val(Float 2.), Val(Vari x)),
               Node(BOperation Plus, c1, c2))
        else Node(BOperation Plus,combine a, combine b)
      (* Times Var *)
      | Node(BOperation Times,Val(Vari x),c1),Val(Vari y)
      | Node(BOperation Times,c1,Val(Vari x)),Val(Vari y)
      | Val(Vari y),Node(BOperation Times,Val(Vari x),c1)
      | Val(Vari y),Node(BOperation Times,c1,Val(Vari x))
        ->
        if String.equal x y then 
          Node(BOperation Times,Node(BOperation Plus,c1,Val(Float 1.)),Val(Vari x))
        else Node(BOperation Plus,combine a, combine b)
      (* Times Times *)
      | Node(BOperation Times,Val(Vari x),c1),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Times,c1,Val(Vari x)),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Times,Val(Vari x),c1),Node(BOperation Times,c2,Val(Vari y))
      | Node(BOperation Times,c1,Val(Vari x)),Node(BOperation Times,c2,Val(Vari y))
        ->
        if String.equal x y then 
          Node(BOperation Times,Node(BOperation Plus,c1,c2),Val(Vari x))
        else Node(BOperation Plus,combine a, combine b)
      (* Plus Times *)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Times,c2,Val(Vari y))
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Times,c2,Val(Vari y))
      | Node(BOperation Times,Val(Vari x),c2),Node(BOperation Plus,c1,Val(Vari y))
      | Node(BOperation Times,c2,Val(Vari x)),Node(BOperation Plus,c1,Val(Vari y))
      | Node(BOperation Times,Val(Vari x),c2),Node(BOperation Plus,Val(Vari y),c1)
      | Node(BOperation Times,c2,Val(Vari x)),Node(BOperation Plus,Val(Vari y),c1)
        ->
        if String.equal x y then 
          Node(BOperation Plus,Node(BOperation Times,
                                    Node(BOperation Plus, c2, Val(Float 1.)),Val(Vari x)),c1)
        else Node(BOperation Plus,combine a, combine b)
      (* Plus Minus *)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Minus,Val(Vari y),c2)
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Minus,Val(Vari y),c2)
      | Node(BOperation Minus,Val(Vari x),c2),Node(BOperation Plus,Val(Vari y),c1)
      | Node(BOperation Minus,Val(Vari x),c2),Node(BOperation Plus,c1,Val(Vari y))
        ->
        if String.equal x y then 
          Node(BOperation Plus,Node(BOperation Times, Val(Float 2.),Val(Vari x)),
               Node(BOperation Minus,c1,c2))
        else Node(BOperation Plus,combine a, combine b)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Minus,c2,Val(Vari y))
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Minus,c2,Val(Vari y))
      | Node(BOperation Minus,c2,Val(Vari x)),Node(BOperation Plus,c1,Val(Vari y))
      | Node(BOperation Minus,c2,Val(Vari x)),Node(BOperation Plus,Val(Vari y),c1)
        ->
        if String.equal x y then 
          Node(BOperation Plus,c1,c2)
        else Node(BOperation Plus,combine a, combine b)
      | x,y -> Node(BOperation Plus,x,y)
    end
  | Node(BOperation Times,Val(Vari x),Val(Vari y)) as n -> 
    if String.equal x y then Node(BOperation Power, Val(Vari x), Val(Float 2.0))
    else n
  | Node(BOperation Minus,a,b)-> begin
      match combine a, combine b with
      | Val(Vari x), Val(Vari y) -> 
        if String.equal x y then Val(Float 0.)
        else Node(BOperation Minus,combine a, combine b)
      | Node(BOperation Plus, Val (Vari x), c1), Val(Vari y)
      | Node(BOperation Plus, c1, Val (Vari x)), Val(Vari y)
        ->
        if String.equal x y then c1
        else Node(BOperation Minus,combine a, combine b)
      | Val(Vari y), Node(BOperation Plus, Val (Vari x), c1)
      | Val(Vari y), Node(BOperation Plus, c1, Val (Vari x)) 
        ->
        if String.equal x y then 
          Node(BOperation Minus, Val(Float 0.), c1)
        else Node(BOperation Minus,combine a, combine b)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Plus,Val(Vari y),c2)
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Plus,Val(Vari y),c2)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Plus,c2,Val(Vari y))
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Plus,c2,Val(Vari y))
        -> 
        if String.equal x y then 
          Node(BOperation Minus, c1, c2)
        else Node(BOperation Minus,combine a, combine b)
      | Node(BOperation Times,Val(Vari x),c1),Val(Vari y)
      | Node(BOperation Times,c1,Val(Vari x)),Val(Vari y)
        ->
        if String.equal x y then 
          Node(BOperation Times,Node(BOperation Minus,c1,Val(Float 1.)),Val(Vari x))
        else Node(BOperation Minus,combine a, combine b)
      | Val(Vari y),Node(BOperation Times,Val(Vari x),c1)
      | Val(Vari y),Node(BOperation Times,c1,Val(Vari x))
        ->
        if String.equal x y then 
          Node(BOperation Times,Node(BOperation Minus,Val(Float 1.),c1),Val(Vari x))
        else Node(BOperation Plus,combine a, combine b)
      | Node(BOperation Times,Val(Vari x),c1),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Times,c1,Val(Vari x)),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Times,Val(Vari x),c1),Node(BOperation Times,c2,Val(Vari y))
      | Node(BOperation Times,c1,Val(Vari x)),Node(BOperation Times,c2,Val(Vari y))
        ->
        if String.equal x y then 
          Node(BOperation Times,Node(BOperation Minus,c1,c2),Val(Vari x))
        else Node(BOperation Minus,combine a, combine b)
      (* Plus Times *)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Times,Val(Vari y),c2)
      | Node(BOperation Plus,Val(Vari x),c1),Node(BOperation Times,c2,Val(Vari y))
      | Node(BOperation Plus,c1,Val(Vari x)),Node(BOperation Times,c2,Val(Vari y))
        ->
        if String.equal x y then 
          Node(BOperation Plus,Node(BOperation Times,
                                    Node(BOperation Minus, Val(Float 1.), c2),Val(Vari x)),c1)
        else Node(BOperation Plus,combine a, combine b)
      | Node(BOperation Times,Val(Vari x),c2),Node(BOperation Plus,c1,Val(Vari y))
      | Node(BOperation Times,c2,Val(Vari x)),Node(BOperation Plus,c1,Val(Vari y))
      | Node(BOperation Times,Val(Vari x),c2),Node(BOperation Plus,Val(Vari y),c1)
      | Node(BOperation Times,c2,Val(Vari x)),Node(BOperation Plus,Val(Vari y),c1)
        ->
        if String.equal x y then 
          Node(BOperation Minus,Node(BOperation Times,
                                     Node(BOperation Minus, c2, Val(Float 1.)),Val(Vari x)),c1)
        else Node(BOperation Plus,combine a, combine b)
      | x,y -> Node(BOperation Minus,x,y)
    end
  | Node(BOperation Div,Val(Vari x),Val(Vari y)) as n -> 
    if String.equal x y then Val(Float 1.)
    else n
  | Node(op, a, b) -> Node(op, combine a, combine b)
  | t -> t