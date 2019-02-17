
(** [helper_calculus ()] prints the helper message for calculus mode. *)
let help_calculus () = 
  ANSITerminal.(print_string [blue] "
  Basic Operations: \n
  \t a + b : to add a to b; \n
  \t a - b : to minus b from a; \n
  \t a * b : to multiply 2 integers; \n
  \t a / b : to divide b from a; \n
  \t a ^ b : to raise a to its b_th power; \n
  \t - a : to change a to negative a;\n
  \t n C k: C(n,k) - combinations;\n
  \t n P k: P(n,k) - permutation; \n
  \n
  Calculus mode: \n
  Any mathematical expression uses operations above that only contains variable
  [x] can be computed \n
  \t Derivatives
  \t Integral \n
  Type the expression in the square bracelet, and use [J] to change the place 
  to type. \n
  The sequence of evalution in our calculator is : \n
    parenthesis > C P > power > times & divide > negate > add & minus \n");
    ()

(** [helper_algebra ()] prints the algebra message for algebra mode. *)
let help_algebra () = 
  ANSITerminal.(print_string [blue] "
  You can use the following command to do mathematical operations. \n
  a + b : to add a to b; \n
  a - b : to minus b from a; \n
  a * b : to multiply 2 integers; \n
  a / b : to divide b from a; \n
  a ^ b : to raise a to its b_th power; \n
  - a : to change a to negative a;\n
  n C k: C(n,k) - combinations;\n
  n P k: P(n,k) - permutation; \n
  \n
  Matrix Operations :\n
  \t Det : determinant
  \t Inv : inverse
  \t Trans : transpose
  \t Eigenval : eigenvalue
  \t Eigenvec : eigenvector
  \t Diag : diagnolization
  \t SVD : singular value decomposition
  \t TR : trace
  \t Adjoint : adjoint \n
  The sequence of evalution in our calculator is : \n
    parenthesis > C P > power > times & divide > negate > add & minus \n");
    ()


(** [quit ()] makes the terminal stops running the file. *)
let quit () =
  ANSITerminal.(print_string [green] "\n\n\n
\t  ███████╗███████╗███████╗    ██╗   ██╗ ██████╗ ██╗   ██╗
\t  ██╔════╝██╔════╝██╔════╝    ╚██╗ ██╔╝██╔═══██╗██║   ██║
\t  ███████╗█████╗  █████╗       ╚████╔╝ ██║   ██║██║   ██║
\t  ╚════██║██╔══╝  ██╔══╝        ╚██╔╝  ██║   ██║██║   ██║
\t  ███████║███████╗███████╗       ██║   ╚██████╔╝╚██████╔╝
\t  ╚══════╝╚══════╝╚══════╝       ╚═╝    ╚═════╝  ╚═════╝ 
                                                       
  \t     █████╗  ██████╗  █████╗ ██╗███╗   ██╗    ██╗      
  \t    ██╔══██╗██╔════╝ ██╔══██╗██║████╗  ██║    ██║      
  \t    ███████║██║  ███╗███████║██║██╔██╗ ██║    ██║      
  \t    ██╔══██║██║   ██║██╔══██║██║██║╚██╗██║    ╚═╝      
  \t    ██║  ██║╚██████╔╝██║  ██║██║██║ ╚████║    ██╗      
  \t    ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝    ╚═╝      
      \n\n\n\n\n\n\n\n\n");
  exit 0

(** [unkown_command ()] prints the warning for user. *)
let unknown_command () = 
  ANSITerminal.(print_string [red] 
  "Sorry, We can't understand your command.\n");
  ()

let debug = true