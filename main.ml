open AST
open BasicOperation
open Gui
open Main_gui
open Cal_gui
open Command 
open Cache

type state = Calculus | Algebra | Alge_gui | Integral | Derivative

(** [single_argument_parse x f] parses [x] and deletes all spaces.

    Example: 
    [single_argument_parse "    x" (fun x -> x)] = "x"
    [single_argument_parse "    quit   " (fun x -> x)] = "quit"
*)
let single_argument_parse x f = 
  let lst = String.split_on_char ' ' x in
  let delete_emp = List.filter (fun x -> x <> "") lst in
  try 
    match delete_emp with
    | [h] -> f h 
    | _ -> failwith "invalid single argument!"
  with 
    _ -> failwith "invalid single argument!"

(** [help mode] prints the correct helper message in terms of the current 
    mode. 

    Example: 
    [help Calculus] = 
    "
    You can use the following command to do mathematical operations. \n
    a + b : to add a to b; \n
    a - b : to minus b from a; \n
    a * b : to multiply 2 integers; \n
    a / b : to divide b from a; \n
    a ^ b : to raise a to its b_th power; \n
    - a : to change a to negative a;\n
    n C k: C(n,k) - combinations;\n
    n P k: P(n,k) - permutation; \n
    The sequence of evalution in our calculator is : \n
      parenthesis > C P > power > times & divide > negate > add & minus \n")
*)
let help = function
  | Calculus -> help_calculus ()
  | Algebra -> help_algebra ()
  | Alge_gui -> help_algebra ()
  | Integral -> help_calculus ()
  | Derivative -> help_calculus ()

(** [accuracy ()] asks the user to enter a valid dicimal place as rounded base. 
*)
let rec accuracy () = 
  try 
    let acc = single_argument_parse (read_line ()) int_of_string in
    acc
  with 
    _ -> 
    ANSITerminal.(print_string [red]
                    "This is an invalid accuracy. Please enter an integer. \n> ");
    accuracy ()

let rec run_op acc mode = 
  try 
    begin
    ANSITerminal.(print_string [yellow] 
                    "\nPlease enter the operation command to get the result! \n");
    ANSITerminal.(print_string [yellow] "> ");
    let e = read_line () in
    if (String.equal (String.lowercase_ascii e) "quit" ||
      String.equal (String.lowercase_ascii e) "qt" ||
      String.equal (String.lowercase_ascii e) "q") then quit ()
    else if (String.equal (String.lowercase_ascii e) "help" ||
      String.equal (String.lowercase_ascii e) "hp" ||
      String.equal (String.lowercase_ascii e) "h") then 
      (help mode; run_op acc mode)
    else
      let input = e |> parse in
      try 
        Hashtbl.find cache input |> print_endline;
        run_op acc mode
      with
        Not_found -> 
        let result = result_formatter (evaluate input) acc true in
        Hashtbl.add cache input result;
        print_endline result;
        run_op acc mode
    end
  with 
    e -> 
    if debug then 
      let () = print_endline (Printexc.to_string e) in 
      let () = unknown_command () in
      run_op acc mode
    else 
      unknown_command (); run_op acc mode

let main () = 
  ANSITerminal.(print_string [cyan; ANSITerminal.Bold; ANSITerminal.Blink]
                  "\n\n\n\n\n
\t   #######################################################################\n");  
  ANSITerminal.(print_string [cyan; ANSITerminal.Bold] "
\t    ██████╗ █████╗ ███╗   ███╗███████╗██╗     ██████╗  █████╗ ███╗   ███╗
\t   ██╔════╝██╔══██╗████╗ ████║██╔════╝██║     ██╔══██╗██╔══██╗████╗ ████║
\t   ██║     ███████║██╔████╔██║█████╗  ██║     ██████╔╝███████║██╔████╔██║
\t   ██║     ██╔══██║██║╚██╔╝██║██╔══╝  ██║     ██╔══██╗██╔══██║██║╚██╔╝██║
\t   ╚██████╗██║  ██║██║ ╚═╝ ██║███████╗███████╗██║  ██║██║  ██║██║ ╚═╝ ██║
\t    ╚═════╝╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝     ╚═╝

\t                ██████╗ ███╗   ███╗███████╗ ██████╗  █████╗              
\t               ██╔═══██╗████╗ ████║██╔════╝██╔════╝ ██╔══██╗             
\t               ██║   ██║██╔████╔██║█████╗  ██║  ███╗███████║             
\t               ██║   ██║██║╚██╔╝██║██╔══╝  ██║   ██║██╔══██║             
\t               ╚██████╔╝██║ ╚═╝ ██║███████╗╚██████╔╝██║  ██║             
 \t               ╚═════╝ ╚═╝     ╚═╝╚══════╝ ╚═════╝ ╚═╝  ╚═╝             

");

  ANSITerminal.(print_string [cyan; ANSITerminal.Bold; ANSITerminal.Blink] "
\t   #######################################################################\n\n\n");
  ANSITerminal.(print_string [red] 
                  "Welcome to the Camelram calculator! \n\nPlease tell me how accurate the result should be. \n> ");
  let acc = accuracy () in
  (* let mode = mode () in *)
  let _ = print_ch_gui mode_choices in 
  let mode = 
    match run_ch_gui mode_choices with 
    | "Algebra\n" -> Alge_gui
    | "Matrix\n" -> Algebra
    | "Calculus\n" -> Calculus
    | _ -> failwith "no other choices"
  in
  match mode with
  | Alge_gui -> 
    let () = print_init_board () in 
    run_gui acc init_calc
  | Calculus -> 
    let _ = print_ch_gui calculus_choices in 
    (match run_ch_gui calculus_choices with 
     | "Derivative\n" -> 
       let _ = print_deriv init_deriv in 
       run_deriv acc init_deriv
     | "Integral\n" ->
       let _ = print_integrate init_int in  
       run_integrate acc init_int
     | _ -> failwith "no other choices")
  | _ -> 
    let _ = print_matrix () in
    run_op acc mode

let () = main ()