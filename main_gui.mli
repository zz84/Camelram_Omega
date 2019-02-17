open Gui 

type choices =
{
  question : string;
  choice : string list; 
  current : int;
  decide : bool
}

(** [mode_choices] is the choices for modes:
    {
      question = "Please choose a mode you want to use:\n"^
                  "(press J to move down, K to move up, ENTER to choose)\n";
      choice = ["Algebra\n"; "Matrix\n";"Calculus\n"];
      current = 0;
      decide = false
    }  *)
val mode_choices : choices 

val calculus_choices : choices

(** [print_ch_gui choice] prints the choices [choice]. 
    
    Example: [print_ch_gui mode_choices] is printing the following string in
              the terminal:
    "Please choose a mode you want to use:
    (press J to move down, K to move up, ENTER to choose)
        --> Algebra
            Matrix
            Calculus"  *)
val print_ch_gui : choices -> unit

(** [run_ch_gui ch] runs the gui of choices [ch] and returns a string once
    the user decide on a choice. 
    
    Example: [run_ch_gui mode_choices] runs the gui of [mode_choices] and 
              returns, say, "Algebra", if the user chooses Algebra mode *)
val run_ch_gui : choices -> string