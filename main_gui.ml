open Gui 

type choices =
  {
    question : string;
    choice : string list; 
    current : int;
    decide : bool
  }

let mode_choices = 
  {
    question = "Please choose a mode you want to use:\n"^
               "(press J to move down, K to move up, ENTER to choose)\n";
    choice = ["Algebra\n"; "Matrix\n";"Calculus\n"];
    current = 0;
    decide = false
  }


let calculus_choices = 
  {
    question = "Please choose a calculus operation you want to use : \n"^
               "(press J to move down, K to move up, ENTER to choose)\n";
    choice = ["Derivative\n"; "Integral\n"];
    current = 0;
    decide = false
  }

(** [format_choices ch] is the formated string list of the choice in choices 
    [ch]. 

    Example: [format_choice mode_choices] is 
    ["    --> Algebra"; "        Matrix"; "        Calculus"] *)
let format_choices ch = 
  let f acc x = 
    if fst acc = ch.current then (fst acc + 1, ("    --> " ^ x)::snd acc)
    else (fst acc + 1, ("        " ^ x)::snd acc)
  in 
  List.fold_left f (0, []) ch.choice |> snd |> List.rev

let print_ch_gui (ch:choices) = 
  ANSITerminal.(print_string [blue] "\n\n\n\n\n");
  ANSITerminal.(print_string [blue] ch.question);
  let f x = 
    if Char.equal x.[6] '>' then ANSITerminal.(print_string [blue; ANSITerminal.Blink] x)
    else ANSITerminal.(print_string [blue] x) 
  in 
  let _ = List.map f (format_choices ch) in 
  ANSITerminal.(print_string [blue] "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
  flush_all ();
  ()

(** [change_state ch chr] is changing the choices [ch] according to the input
    character [ch]. 'j' moves the arrow down, 'k' moves the arrow up, 'enter'
    chooses the current choice.

    Example: 
    [change_state mode_choice 'j'] is 
    {
      question = "Please choose a mode you want to use:\n"^
                  "(press J to move down, K to move up, ENTER to choose)\n";
      choice = ["Algebra\n"; "Matrix\n";"Calculus\n"];
      current = 1;
      decide = false
    } 

    [change_state mode_choice '\n' is 
    {
      question = "Please choose a mode you want to use:\n"^
                  "(press J to move down, K to move up, ENTER to choose)\n";
      choice = ["Algebra\n"; "Matrix\n";"Calculus\n"];
      current = 0;
      decide = true
    } *)
let change_state ch chr = 
  let len = List.length ch.choice in
  match chr with 
  | 'j' |'J'-> 
    let cur = if ch.current < len - 1 then ch.current + 1 else 0 in 
    {ch with current = cur}
  | 'k' |'K' -> 
    let cur = if ch.current = 0 then len - 1 else ch.current - 1 in 
    {ch with current = cur}
  | '\n' -> {ch with decide = true}
  | _ -> ch

(** [get_nth lst n] is the [n]th element in the list [lst]

    Requires: [lst] is not an empty list

    Example: [get_nth [1;2;3;4] 3] is 3 *)
let rec get_nth lst n = 
  match lst with
  | [] -> failwith "violate get_nth precondition" 
  | h::t -> if n = 0 then h else get_nth t (n-1)

let rec run_ch_gui ch = 
  let input = get1char () in 
  let new_ch = change_state ch input in 
  if new_ch.decide then get_nth new_ch.choice new_ch.current
  else
    let _ = print_ch_gui new_ch in 
    run_ch_gui new_ch