
type const = Pi | E

type value = 
  | Float of float
  | Matrix of float list list 
  | MatrixSVD of float list list * float list list * float list list 
  | MatrixDiag of float list list * float list list * float list list 
  | Const of const
  | Emp
  | Vari of string

type 'a t = Empty|Val of value| Node of 'a *'a t * 'a t |Par of 'a t

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

(**[parse s] returns the expression tree parsed from [s]
  *Requires: s is a valid mathematical expression 
  * Example: parse "2 + 3" = 
              Node(BOperation Plus, Value(Float 2.0), Value(Float 3.0)) 
             parse " sin 3" = 
               Node(UOperation Sin, Value(Emp), Value(Float 3.0)) 
             parse "1" = Value(Float 3.0)*)
val parse: string -> expr t 

(**[format t] returns the string formatted from the expression tree [t]
  *Examples: format Empty = ""
             format Node(BOperation Plus, Value(Float 3.0), Value(Float 2.0))
                = "3.0 + 2.0"*)
val format: expr t-> string

(**[evaluate t] returns the value after evaluating expression [t]
 *  Examples: let t = Node(BOperation Plus, Value(Float 3.0), Value(Float 2.0))
              evaluate t = 5.0
              let v = Value(Float 3.0)
              evaluate v = 3.0*)
val evaluate: expr t -> value

(** [result_formatter x accuracy print_tag] is the formatted [x] 
  * Examples: result_formatter 3.0 = "3"
              result_formatter 2.6 = "2.6"*)
val result_formatter : value -> int -> bool -> string

(** [trim_result_precision num p] is the trimed result of num, that add 1 to 
    [p]_th decimal place if the [p+1]_th decimal place is greater or equal to 
    5. 
    Examples: 
    [trim_result_precision 1.89 1] = "1.9"
    [trim_result_precision 1.89 2] = "1.89"
    [trim_result_precision 1.54 1] = "1.5"
*)
val trim_result_precision : float -> int -> string

(** [substitute eq num] replaces all "x" occurs in [eq] with [num]. 
    
    Examples: 
    [substitute "x+2" "3"] = "3+2"
    [substitute "2" "3"] = "2"
*)
val substitute : string -> string -> string

(** [simplify_1 t] removes unnessary paratheses in [t]. It doesn't change 
    the evaluated result of [t]. 

    Example: 
    [simplify_1 (Par (Val (Float 1.)))] = Val (Float 1.)
 *)
val simplify_1 : expr t -> expr t

(** [simplify t] combines mulitple terms to one while remaining the evaluted
    result. 

    Examples: 
    [simplify (Node (BOperation Times, 
                     Val (Float 5.),
                     Node (BOperation Times, 
                           Val (Float 6.), 
                           Val (Vari "x"))))] = Node (BOperation Times, 
                                                      Val (Vari "x"), 
                                                      Val (Float 30.))
    
    [simply (Node (BOperation Plus, 
                   Node (BOperation Plus, 
                         Val (Float 7.), 
                         Val (Vari "x")),
                   Val (Float 5.)))] = Node (BOperation Plus, 
                                             Val (Vari "x"),
                                             Val (Float 12.))
*)
val simplify : expr t -> expr t

(** [reduce t] is the simplified t that evaluates to the same result as [t]
    by removing operations that doesn't change [t]'s evaluated result. 

    Examples: 
    [reduce (Node (BOperation Div, 
                   Val (Vari "x"), 
                   Val (Float 1.)))] = [Val (Vari "x")]
    
    [reduce (Node (BOperation Times, 
                   Val (Float 1.),
                   Node (BOperation Div, 
                         Val (Vari "x"), 
                         Val (Float 1.))))] = [Val (Vari "x")]
    *)
val reduce : expr t -> expr t

(** [combine t] is the combined polynomial expression of [t] by putting 
    together the term that has the same exponent. 

    For example: 
    [combine (Node (BOperation Plus, 
                    Val (Vari "x"), 
                    Val (Vari "x")))] = Node (BOperation Times, 
                                              Val (Float 2.),
                                               Val (Vari "x"))
*)
val combine : expr t -> expr t