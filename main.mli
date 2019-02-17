type state = Calculus | Algebra | Alge_gui | Integral | Derivative

(** 
  [main ()] promopts the calculator to work, then starts it by calling [run_op]
 *)
 val main : unit -> unit 

(** [run_op command accuracy mode] converts commands into abstract 
    syntax trees and evaulates it to a result in [mode] type 
    that has [accuracy] number of digits and. 
 *)
 val run_op : int -> state -> unit  