open Gui 
open Command
open Derivative
open Integral
open AST
open Integral

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

let init_int = 
  {
    cinput = "";
    coutput = "";
    coutput2 = "";
    ceval = false;
    ceval2 = false;
    upbound = "";
    lowbound = "";
    int_st = Eq_int
  }

let init_deriv = 
  {
    dinput = "";
    doutput = "";
    doutput2 = "";
    deval = false;
    deval2 = false;
    bound = "";
    deriv_st = Eq_deriv
  }

(** [make_par str par] is the string with squre brackets around [str] if [par] is true;
    if [par] is false, then it puts parenthesis around [str]

    Examples:
      [make_par "3+x" true] is "[ 3+x ]"
      [make_par "3+x+5-9" false] is "( 3+x+5-9 )"
*)
let make_par (str:string) par = 
  if par then "[ " ^ str ^ " ]" else "( " ^ str ^ " )"

let print_integrate board = 
  let inp, up, low = 
    match board.int_st with 
    | LowB   -> make_par board.cinput false, make_par board.upbound false, 
                make_par board.lowbound true
    | HighB  -> make_par board.cinput false, make_par board.upbound true, 
                make_par board.lowbound false
    | Eq_int -> make_par board.cinput true, make_par board.upbound false, 
                make_par board.lowbound false
  in 
  ANSITerminal.(print_string [blue; ANSITerminal.Bold] "\n\n\n\n\n
\t██╗███╗   ██╗████████╗███████╗ ██████╗ ██████╗  █████╗ ████████╗███████╗
\t██║████╗  ██║╚══██╔══╝██╔════╝██╔════╝ ██╔══██╗██╔══██╗╚══██╔══╝██╔════╝
\t██║██╔██╗ ██║   ██║   █████╗  ██║  ███╗██████╔╝███████║   ██║   █████╗  
\t██║██║╚██╗██║   ██║   ██╔══╝  ██║   ██║██╔══██╗██╔══██║   ██║   ██╔══╝  
\t██║██║ ╚████║   ██║   ███████╗╚██████╔╝██║  ██║██║  ██║   ██║   ███████╗
\t╚═╝╚═╝  ╚═══╝   ╚═╝   ╚══════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝   ╚══════╝
  \n\n
\t Press 'J' to change where to [input], Press 'ENTER' to evaluate.
\t      Type in 'quit' to exit, and 'help' to get help.\n");
  ANSITerminal.(print_string [ANSITerminal.Bold] 
                  ("
  \t ╔╗" ^ up ^ "
  \t ║  " ^ inp ^ " dx 
  \t╚╝ " ^ low));
  let _ = if board.ceval then 
      ANSITerminal.(print_string [ANSITerminal.Bold] ("
    \t= " ^ board.coutput)); 
    ()
  in
  let _ = if board.ceval2 then 
      ANSITerminal.(print_string [ANSITerminal.Bold] ("
    \t= " ^ board.coutput2)); 
    ()
  in
  ANSITerminal.(print_string [ANSITerminal.Bold] "\n\n\n\n\n\n\n\n\n\n\n");
  flush_all ();
  ()

let print_deriv board = 
  let inp, b = 
    match board.deriv_st with 
    | Bound    -> make_par board.dinput false, make_par board.bound true
    | Eq_deriv -> make_par board.dinput true, make_par board.bound false
  in 
  ANSITerminal.(print_string [red; ANSITerminal.Bold] "\n\n\n\n\n
\t██████╗ ███████╗██████╗ ██╗██╗   ██╗ █████╗ ████████╗██╗██╗   ██╗███████╗
\t██╔══██╗██╔════╝██╔══██╗██║██║   ██║██╔══██╗╚══██╔══╝██║██║   ██║██╔════╝
\t██║  ██║█████╗  ██████╔╝██║██║   ██║███████║   ██║   ██║██║   ██║█████╗  
\t██║  ██║██╔══╝  ██╔══██╗██║╚██╗ ██╔╝██╔══██║   ██║   ██║╚██╗ ██╔╝██╔══╝  
\t██████╔╝███████╗██║  ██║██║ ╚████╔╝ ██║  ██║   ██║   ██║ ╚████╔╝ ███████╗
\t╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝  ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═══╝  ╚══════╝
  \n\n
\t Press 'J' to change where to [input], Press 'ENTER' to evaluate.
\t      Type in 'quit' to exit, and 'help' to get help.\n");
  ANSITerminal.(print_string [ANSITerminal.Bold] 
                  ("
  \t d
  \t —— " ^ inp ^ " at " ^ b ^ " 
  \t dx"));
  let _ = if board.deval then 
      ANSITerminal.(print_string [ANSITerminal.Bold] ("
  \t= " ^ board.doutput));
    ()
  in 
  let _ = if board.deval2 then 
      ANSITerminal.(print_string [ANSITerminal.Bold] ("
  \t= " ^ board.doutput2));
    ()
  in 
  ANSITerminal.(print_string [ANSITerminal.Bold] "\n\n\n\n\n\n\n\n\n\n\n");
  flush_all ();
  ()

(** [eval_integrate acc board] is the result of [board] with accuracy [acc].
    If input in [board] is "quit", then it exits. If input in [board] is 
    "help" then it calls help and returns [init_int]. Else, it tries to 
    evaluate the integral, changes the output and return changed [board]. If it
    fails to evaluate, then it calls [unknown_command ()] and returns current
    [board].

    Examples:
      [eval_integrate 5 {board with cinput = "quit"}] exits the calculator. 

      If [board_1] is:
          {
            cinput = "x+1";
            coutput = "";
            coutput2 = "";
            ceval = false;
            ceval2 = false;
            upbound = "";
            lowbound = "";
            int_st = Eq_int
          }
      [eval_integrate 5 board_1] is:
          {
            cinput = "x+1";
            coutput = "0.5*x^2+x+C";
            coutput2 = "";
            ceval = true;
            ceval2 = false;
            upbound = "";
            lowbound = "";
            int_st = Eq_int
          } 

      If [board_2] is:
          {
            cinput = "x^2+1";
            coutput = "";
            coutput2 = "";
            ceval = false;
            ceval2 = false;
            upbound = "4";
            lowbound = "2";
            int_st = HighB
          }
      [eval_integrate 5 board_1] is:
          {
            cinput = "x^2+1";
            coutput = "x^3/3+x+C";
            coutput2 = "20.66666";
            ceval = true;
            ceval2 = true;
            upbound = "4";
            lowbound = "2";
            int_st = HighB
          } *)
let eval_integrate acc board = 
  if String.equal (String.lowercase_ascii board.cinput) "quit" || 
     String.equal (String.lowercase_ascii board.cinput) "q" ||
     String.equal (String.lowercase_ascii board.cinput) "qt" then 
    let _ = quit () in 
    board
  else if String.equal (String.lowercase_ascii board.cinput) "help" ||
    String.equal (String.lowercase_ascii board.cinput) "h" ||
    String.equal (String.lowercase_ascii board.cinput) "hp" then 
    let _ = help_calculus () in 
    init_int
  else
    try
      let tree = AST.parse board.cinput in 
      let res = Integral.integrate tree
                |> AST.combine
                |> AST.simplify
                |> AST.reduce
                |> AST.simplify_1
                |> AST.format
                |> AST.parse 
                |> AST.combine
                |> AST.simplify
                |> AST.reduce
                |> AST.simplify_1
                |> AST.format in
      if String.equal "" board.upbound || String.equal "" board.lowbound then 
        {board with ceval = true; coutput = res ^ "+C"}
      else
        let res2 = AST.result_formatter
            (AST.substitute res board.upbound
             |> AST.parse 
             |> AST.evaluate) acc false
        in 
        let res3 = AST.result_formatter
            (AST.substitute res board.lowbound
             |> AST.parse 
             |> AST.evaluate) acc false in 
        let res4 = (res2^"-("^res3^")" )|>AST.parse |> AST.evaluate  in 
        let res5 = AST.result_formatter res4 acc false in 
        {board with ceval = true; ceval2 = true; coutput = res ^ "+C"; coutput2 = res5}
    with 
      e ->
      let _ = if Command.debug then 
          let () = print_endline (Printexc.to_string e) in 
          ()
        else () in 
      let _ = unknown_command () in 
      board



(** [eval_deriv acc board] is the result of [board] with accuracy [acc].
    If input in [board] is "quit", then it exits. If input in [board] is 
    "help" then it calls help and returns [init_deriv]. Else, it tries to 
    evaluate the derivative, changes the output and return changed [board]. If it
    fails to evaluate, then it calls [unknown_command ()] and returns current
    [board].

    Examples:
      [eval_deriv 5 {board with dinput = "quit"}] exits the calculator. 

      If [board_1] is:
            {
              dinput = "x+4";
              doutput = "";
              doutput2 = "";
              deval = false;
              deval2 = false;
              bound = "";
              deriv_st = Eq_deriv
            }
      [eval_integrate 5 board_1] is:
            {
              dinput = "x+4";
              doutput = "1";
              doutput2 = "";
              deval = true;
              deval2 = false;
              bound = "";
              deriv_st = Eq_deriv
            }

      If [board_2] is:
            {
              dinput = "x^10+4*x";
              doutput = "";
              doutput2 = "";
              deval = false;
              deval2 = false;
              bound = "5";
              deriv_st = Bound
            }
      [eval_integrate 5 board_1] is:
            {
              dinput = "x^10+4*x";
              doutput = "4+x^9*10";
              doutput2 = "19531254";
              deval = true;
              deval2 = true;
              bound = "5";
              deriv_st = Bound
            } 
*)
let eval_deriv acc board = 
  if String.equal (String.lowercase_ascii board.dinput) "quit" ||
    String.equal (String.lowercase_ascii board.dinput) "q" ||
    String.equal (String.lowercase_ascii board.dinput) "qt"  then 
    let _ = quit () in 
    board
  else if String.equal (String.lowercase_ascii board.dinput) "help" ||
    String.equal (String.lowercase_ascii board.dinput) "hp" ||
    String.equal (String.lowercase_ascii board.dinput) "h" then 
    let _ = help_calculus () in 
    init_deriv
  else
    try
      let tree = AST.parse board.dinput in 
      let res = Derivative.derivative tree (AST.Vari("x")) 
                |> AST.combine
                |> AST.simplify
                |> AST.reduce
                |> AST.simplify_1
                |> AST.format
                |> AST.parse 
                |> AST.combine
                |> AST.simplify
                |> AST.reduce
                |> AST.simplify_1
                |> AST.format in
      if String.equal "" board.bound then 
        {board with deval = true; doutput = res}
      else
        let res2 = AST.result_formatter
            (AST.substitute res board.bound
             |> AST.parse 
             |> AST.evaluate) acc false
        in 
        {board with deval = true; deval2 = true; doutput = res; doutput2 = res2}
    with 
      e ->
      let _ = if Command.debug then 
          let () = print_endline (Printexc.to_string e) in 
          ()
        else () in 
      let _ = unknown_command () in 
      board

(** [int_add_1 chr board] adds [chr] to the place marked by [int_st] in
    [board].

    Requires:
      [board] is a value integrate board

    Examples:
      [int_add_1 'x' init_int] is {init_int with cinput = "x"} *)
let int_add_1 chr board =
  match board.int_st with 
  | Eq_int -> {board with cinput = board.cinput ^ Char.escaped chr}
  | LowB   -> {board with lowbound = board.lowbound ^ Char.escaped chr}
  | HighB  -> {board with upbound = board.upbound ^ Char.escaped chr}

(** [int_del_1 board] is delete one char from the [board] marked by [int_st]
    in [board].

    Requires:
      [board] is a valid integrate board.

    Examples:
      if [board_1] is:
          {
            cinput = "x^2+1";
            coutput = "";
            coutput2 = "";
            ceval = false;
            ceval2 = false;
            upbound = "4";
            lowbound = "2";
            int_st = Eq_int
          } 
      then [int_del_1 board_1] is
          {
            cinput = "x^2+";
            coutput = "";
            coutput2 = "";
            ceval = false;
            ceval2 = false;
            upbound = "4";
            lowbound = "2";
            int_st = Eq_int
          }  *)
let int_del_1 board = 
  match board.int_st with 
  | Eq_int ->
    let len = String.length board.cinput in 
    let new_input = 
      if len > 1 then String.sub board.cinput 0 (len-1) 
      else if len = 1 then ""
      else board.cinput
    in 
    {board with cinput = new_input}
  | HighB ->
    let len = String.length board.upbound in 
    let new_bound = 
      if len > 1 then String.sub board.upbound 0 (len-1) 
      else if len = 1 then ""
      else board.upbound
    in 
    {board with upbound = new_bound}
  | LowB ->
    let len = String.length board.lowbound in 
    let new_bound = 
      if len > 1 then String.sub board.lowbound 0 (len-1) 
      else if len = 1 then ""
      else board.lowbound
    in 
    {board with lowbound = new_bound}

(** [deriv_add_1 chr board] adds [chr] to the place marked by [deriv_st] in
    [board].

    Requires:
      [board] is a value derivative board

    Examples:
      [deriv_add_1 'x' init_deriv] is {init_deriv with dinput = "x"} *)
let deriv_add_1 chr board =
  match board.deriv_st with 
  | Eq_deriv -> {board with dinput = board.dinput ^ Char.escaped chr}
  | Bound    -> {board with bound = board.bound ^ Char.escaped chr}

(** [deriv_del_1 board] is delete one char from the [board] marked by [deriv_st]
    in [board].

    Requires:
      [board] is a valid derivative board.

    Examples:
      if [board_1] is:
            {
              dinput = "x^10+4*x";
              doutput = "";
              doutput2 = "";
              deval = false;
              deval2 = false;
              bound = "5";
              deriv_st = Bound
            }
      then [int_del_1 board_1] is
            {
              dinput = "x^10+4*x";
              doutput = "";
              doutput2 = "";
              deval = false;
              deval2 = false;
              bound = "";
              deriv_st = Bound
            } *)
let deriv_del_1 board = 
  match board.deriv_st with 
  | Eq_deriv ->
    let len = String.length board.dinput in 
    let new_input = 
      if len > 1 then String.sub board.dinput 0 (len-1) 
      else if len = 1 then ""
      else board.dinput
    in 
    {board with dinput = new_input}
  | Bound ->
    let len = String.length board.bound in 
    let new_bound = 
      if len > 1 then String.sub board.bound 0 (len-1) 
      else if len = 1 then ""
      else board.bound
    in 
    {board with bound = new_bound}

(** [int_switch board] is switch the [int_st] in board [board]

    Requires:
      [board] is a valid integrate board

    Examples:
      [int_switch {board with int_st = Eq_int}] is 
      {board with int_st = HighB} 
      [int_switch {board with int_st = HighB}] is 
      {board with int_st = LowB}
*)
let int_switch board = 
  let new_st = 
    match board.int_st with 
    | LowB   -> Eq_int
    | Eq_int -> HighB
    | HighB  -> LowB
  in 
  {board with int_st = new_st}

(** [deriv_switch board] is switch the [deriv_st] in board [board]

    Requires:
      [board] is a valid derivative board

    Examples:
      [deriv_switch {board with deriv_st = Eq_deriv}] is 
      {board with deriv_st = Bound} 
      [deriv_switch {board with deriv_st = Bound}] is 
      {board with deriv_st = Eq_deriv} 
*)
let deriv_switch board = 
  let new_st = 
    match board.deriv_st with 
    | Bound -> Eq_deriv
    | Eq_deriv -> Bound 
  in 
  {board with deriv_st = new_st}

let rec run_integrate acc (board:int_t) = 
  match Gui.get1char () with 
  | '\n' -> 
    let new_board = eval_integrate acc board in 
    let _ = print_integrate new_board in 
    run_integrate acc init_int
  | '\127' -> 
    let new_board = int_del_1 board in 
    let _ = print_integrate new_board in 
    run_integrate acc new_board
  | 'j'|'J' -> 
    let new_board = int_switch board in 
    let _ = print_integrate new_board in 
    run_integrate acc new_board
  | t -> 
    let new_board = int_add_1 t board in 
    let _ = print_integrate new_board in 
    run_integrate acc new_board

let rec run_deriv acc (board:deriv_t) = 
  match Gui.get1char () with 
  | '\n' -> 
    let new_board = eval_deriv acc board in 
    let _ = print_deriv new_board in 
    run_deriv acc init_deriv
  | '\127' -> 
    let new_board = deriv_del_1 board in 
    let _ = print_deriv new_board in 
    run_deriv acc new_board
  | 'j' |'J'-> 
    let new_board = deriv_switch board in 
    let _ = print_deriv new_board in 
    run_deriv acc new_board
  | t -> 
    let new_board = deriv_add_1 t board in 
    let _ = print_deriv new_board in 
    run_deriv acc new_board