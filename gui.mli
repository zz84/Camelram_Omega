open AST
type button = 
{
  name:string;
  highlight:bool;
}

type calc = 
{
  input:string;
  output:string;
  buttons:button list 
}

(** [init_calc] is the initial calculator without any input in the screen. 
    
    Example: 
    ——————————————————————————————————————————————
    |  ________________________________________  |
    |  |                                      |  |
    |  |                                      |  |
    |  |______________________________________|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | C |  |sin|  | ( |  | ) |  | % |  |<——|  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | P |  |cos|  | 7 |  | 8 |  | 9 |  | / |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  |exp|  |tan|  | 4 |  | 5 |  | 6 |  | * |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | q |  | ^ |  | 1 |  | 2 |  | 3 |  | - |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | h |  |log|  | 0 |  | . |  | = |  | + |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    ——————————————————————————————————————————————
 *)
val init_calc : calc

(** [print_board calc] prints the [calc] in terminal. 
    Example: 
    calc = 
    {
      input = "1 / 3";
      output = "0.33";
      buttons = init_button
    }
    
    [print_board calc] is 
    ——————————————————————————————————————————————
    |  ________________________________________  |
    |  |                                  1/3 |  |
    |  |                                 0.33 |  |
    |  |______________________________________|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | C |  |sin|  | ( |  | ) |  | % |  |<——|  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | P |  |cos|  | 7 |  | 8 |  | 9 |  | / |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  |exp|  |tan|  | 4 |  | 5 |  | 6 |  | * |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | q |  | ^ |  | 1 |  | 2 |  | 3 |  | - |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    |  _____  _____  _____  _____  _____  _____  |
    |  | h |  |log|  | 0 |  | . |  | = |  | + |  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
    ——————————————————————————————————————————————
*)
val print_board : calc -> unit 

(** [print_init_board ()] prints the initial board on terminal. *)
val print_init_board : unit -> unit

(** [del_one calc] is the board with the last input character deleted. 
    
    Example: 
    calc = 
    {
      input = "46";
      output = "";
      buttons = init_button
    }

    new_calc = 
    {
      input = "6";
      output = "";
      buttons = init_button with "<--" button shaded
    }

*)
val del_one : calc -> calc

(** [insert_char chr cal] adds [char] to the last position in the 
    screen of [cal]. 

    Example:
    cal = 
    {
      input = "6";
      output = "";
      buttons = init_button 
    }
    char = '4'

    then the output should be 
    new_cal = 
    {
      input = "64";
      output = "";
      buttons = init_button 
    }

*)
val insert_char : char -> calc -> calc


(** [get1char ()] reads the new input character from the io. 

    Example: 
    [get1char ()] = '1' if we type '1' on the keyboard.  
*)
val get1char : unit -> char

(** [shade str cal] makes the typed character's key in calculator becomes 
    shaded. 

    Examples:
    [shade "7" init_calc] is the [init_calc] with the shaded key ["7"]. 
    i.e. 
    if 
    init_calc = 
    {input = ""; output = "";
     buttons =
      [{name = "C"; highlight = false}; {name = "sin"; highlight = false};
      {name = "("; highlight = false}; {name = ")"; highlight = false};
      {name = "%"; highlight = false}; {name = "<——"; highlight = false};
      {name = "P"; highlight = false}; {name = "cos"; highlight = false};
      {name = "7"; highlight = false}; {name = "8"; highlight = false};
      {name = "9"; highlight = false}; {name = "/"; highlight = false};
      {name = "exp"; highlight = false}; {name = "tan"; highlight = false};
      {name = "4"; highlight = false}; {name = "5"; highlight = false};
      {name = "6"; highlight = false}; {name = "*"; highlight = false};
      {name = "q"; highlight = false}; {name = "^"; highlight = false};
      {name = "1"; highlight = false}; {name = "2"; highlight = false};
      {name = "3"; highlight = false}; {name = "-"; highlight = false};
      {name = "h"; highlight = false}; {name = "log"; highlight = false};
      {name = "0"; highlight = false}; {name = "."; highlight = false};
      {name = "="; highlight = false}; {name = "+"; highlight = false}]
      }

    outputs 

    {input = ""; output = "";
     buttons =
      [{name = "C"; highlight = false}; {name = "sin"; highlight = false};
      {name = "("; highlight = false}; {name = ")"; highlight = false};
      {name = "%"; highlight = false}; {name = "<——"; highlight = false};
      {name = "P"; highlight = false}; {name = "cos"; highlight = false};
      {name = "7"; highlight = true}; {name = "8"; highlight = false};
      {name = "9"; highlight = false}; {name = "/"; highlight = false};
      {name = "exp"; highlight = false}; {name = "tan"; highlight = false};
      {name = "4"; highlight = false}; {name = "5"; highlight = false};
      {name = "6"; highlight = false}; {name = "*"; highlight = false};
      {name = "q"; highlight = false}; {name = "^"; highlight = false};
      {name = "1"; highlight = false}; {name = "2"; highlight = false};
      {name = "3"; highlight = false}; {name = "-"; highlight = false};
      {name = "h"; highlight = false}; {name = "log"; highlight = false};
      {name = "0"; highlight = false}; {name = "."; highlight = false};
      {name = "="; highlight = false}; {name = "+"; highlight = false}]
      }
     *)
val shade : string -> calc -> calc

val eval_gui : int -> calc -> calc 

val run_gui : int -> calc -> 'a

val print_matrix : unit -> unit