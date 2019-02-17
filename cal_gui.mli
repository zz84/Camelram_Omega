
type int_state = LowB | HighB | Eq_int

type deriv_state = Bound | Eq_deriv

type int_t = 
{
  cinput:string;
  coutput:string;
  coutput2:string;
  ceval:bool;
  ceval2:bool;
  upbound:string;
  lowbound:string;
  int_st:int_state
}

type deriv_t = 
{
  dinput:string;
  doutput:string;
  doutput2:string;
  deval:bool;
  deval2:bool;
  bound:string;
  deriv_st:deriv_state
}

(** [init_int] is the initial integral board defined as: 
    {
      cinput = "";
      coutput = "";
      coutput2 = "";
      ceval = false;
      ceval2 = false;
      upbound = "";
      lowbound = "";
      int_st = Eq_int
    } *)
val init_int : int_t

(** [init_deriv] is the inital derivative board defined as:
    {
      dinput = "";
      doutput = "";
      doutput2 = "";
      deval = false;
      deval2 = false;
      bound = "";
      deriv_st = Eq_deriv
    }  *)
val init_deriv : deriv_t

(** [print_integrate board] prints the integral board for [board].

    Requires:
      [board] is a valid integral board.

    Examples:
      [print_integrate init_int] prints the following string to the terminal:
"      
	██╗███╗   ██╗████████╗███████╗ ██████╗ ██████╗  █████╗ ████████╗███████╗
	██║████╗  ██║╚══██╔══╝██╔════╝██╔════╝ ██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
	██║██╔██╗ ██║   ██║   █████╗  ██║  ███╗██████╔╝███████║   ██║   █████╗
	██║██║╚██╗██║   ██║   ██╔══╝  ██║   ██║██╔══██╗██╔══██║   ██║   ██╔══╝
	██║██║ ╚████║   ██║   ███████╗╚██████╔╝██║  ██║██║  ██║   ██║   ███████╗
	╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚══════╝



	 Press 'J' to change where to [input], Press 'ENTER' to evaluate.
	      Type in 'quit' to exit, and 'help' to get help.

  	 ╔╗(  )
  	 ║  [  ] dx
  	╚╝ (  )    "  *)
val print_integrate : int_t -> unit

(** [print_deriv board] prints the derivative board for [board].

    Requires:
      [board] is a valid derivative board.

    Examples:
      [print_deriv init_deriv] prints the following string to the terminal:
"

	██████╗ ███████╗██████╗ ██╗██╗   ██╗ █████╗ ████████╗██╗██╗   ██╗███████╗
	██╔══██╗██╔════╝██╔══██╗██║██║   ██║██╔══██╗╚══██╔══╝██║██║   ██║██╔════╝
	██║  ██║█████╗  ██████╔╝██║██║   ██║███████║   ██║   ██║██║   ██║█████╗
	██║  ██║██╔══╝  ██╔══██╗██║╚██╗ ██╔╝██╔══██║   ██║   ██║╚██╗ ██╔╝██╔══╝
	██████╔╝███████╗██║  ██║██║ ╚████╔╝ ██║  ██║   ██║   ██║ ╚████╔╝ ███████╗
	╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝  ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═══╝  ╚══════╝



	 Press 'J' to change where to [input], Press 'ENTER' to evaluate.
	      Type in 'quit' to exit, and 'help' to get help.

  	 d
  	 —— [  ] at (  )
  	 dx

"  *)
val print_deriv : deriv_t -> unit

(** [run_integrate acc board] runs the integral calculator with accuracy
    [acc] and current board [board]. It waits a keyboard input and changes 
    the board according to the input. Then it prints the changed [board] to 
    the terminal, and recursively calls [run_integrate acc board].
    
    Requires: 
      [acc] is a positive number
      [board] is a valid integral board. 
    
    Examples:
      [run_integrate 5 init_int] runs the integral calculator with accuracy 
      [acc] and current board [init_int]. If the user hit "3", then it changes
      the [init_int] to 
        {
          cinput = "3";
          coutput = "";
          coutput2 = "";
          ceval = false;
          ceval2 = false;
          upbound = "";
          lowbound = "";
          int_st = Eq_int
        } ,
      prints the new board, and calls [run_integrate acc new_board].
 *)
val run_integrate : int -> int_t -> 'a

(** [run_deriv acc board] runs the derivative calculator with accuracy
    [acc] and current board [board]. It waits a keyboard input and changes 
    the board according to the input. Then it prints the changed [board] to 
    the terminal, and recursively calls [run_derivative acc board].
    
    Requires: 
      [acc] is a positive number
      [board] is a valid derivative board. 
    
    Examples:
      [run_derivative 5 init_deriv] runs the derivative calculator with accuracy 
      [acc] and current board [init_deriv]. If the user hit "4", then it changes
      the [init_deriv] to 
        {
          dinput = "4";
          doutput = "";
          doutput2 = "";
          deval = false;
          deval2 = false;
          bound = "";
          deriv_st = Eq_deriv
        } ,
      prints the new board, and calls [run_derivative acc new_board].
 *)
val run_deriv : int -> deriv_t -> 'a