open AST
open Command
open Cache 

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


let init_button = 
[
  {name = "C"; highlight = false};
  {name = "sin"; highlight = false};
  {name = "("; highlight = false};
  {name = ")"; highlight = false};
  {name = "%"; highlight = false};
  {name = "<——"; highlight = false};
  {name = "P"; highlight = false};
  {name = "cos"; highlight = false};
  {name = "7"; highlight = false};
  {name = "8"; highlight = false};
  {name = "9"; highlight = false};
  {name = "/"; highlight = false};
  {name = "exp"; highlight = false};
  {name = "tan"; highlight = false};
  {name = "4"; highlight = false};
  {name = "5"; highlight = false};
  {name = "6"; highlight = false};
  {name = "*"; highlight = false};
  {name = "q"; highlight = false};
  {name = "^"; highlight = false};
  {name = "1"; highlight = false};
  {name = "2"; highlight = false};
  {name = "3"; highlight = false};
  {name = "-"; highlight = false};
  {name = "h"; highlight = false};
  {name = "log"; highlight = false};
  {name = "0"; highlight = false};
  {name = "."; highlight = false};
  {name = "="; highlight = false};
  {name = "+"; highlight = false}
]

let init_calc = 
{
  input = "";
  output = "";
  buttons = init_button
}

let top_out_boarder = "    ╔════════════════════════════════════════════╗\n"
let bot_out_boarder = "    ╚════════════════════════════════════════════╝\n"
let io_top_boarder =  "    ║  ╔══════════════════════════════════════╗  ║\n"
let io_bot_boarder =  "    ║  ╚══════════════════════════════════════╝  ║\n" 
let empty_line = "    |" ^ String.make 44 ' ' ^ "|\n"
let button_boarder = "  ╔═══╗"
let button_top_line = "    ║" ^ button_boarder ^ button_boarder ^ button_boarder
                  ^ button_boarder ^ button_boarder ^ button_boarder 
                  ^  "  ║\n"
let button_bot = "  ╚═══╝"
let button_bot_line = "    ║" ^ button_bot ^ button_bot ^ button_bot
                  ^ button_bot ^ button_bot ^ button_bot 
                  ^  "  ║\n"    

(** [format_io_line io] is the formatted input [io] that will be placed
    in the calculator screen. 

    Example: 
    [format_io_line "something"] = 
    "    |  |                            something |  |\n"
    *) 
let format_io_line io =
  let len = String.length io in 
  if len < 37 then
    "    ║  ║ " ^ (String.make (36-len) ' ') ^ io ^ " ║  ║\n"
  else
    "    ║  ║ " ^ String.sub io (len-36) 36 ^ " ║  ║\n"

(** [format_io_board inp oup] is the formatted screen of the calculator based
    on [inp] and [oup]. [inp] will be placed on the top of [oup]. 

    Example: 
    [format_io_board "9/1" "9"] = 
    "    |  ________________________________________  |
         |  |                                  9/1 |  |
         |  |                                    9 |  |
         |  |______________________________________|  |"
*)
let format_io_board inp oup = 
  let input_line = format_io_line inp in 
  let output_line = format_io_line oup in 
  io_top_boarder ^ input_line ^  output_line ^ io_bot_boarder

(** [get_6 lst acc] puts the element in [acc] to the beginning of the [lst] and
    copys the element in [lst] whose index exceeds 5 to [acc], in which
    we keeps the order of [acc]. 

    Requires: 
    [lst]'s length is 6 or more. 

    Example: 
    [get_6 [1;2;4;5;6;7] []] = ([1;2;4;5;6;7], [])
    [get_6 [1;2;4;5;6;7;8] []] = ([1;2;4;5;6;7], [8])
    [get_6 [1;2;4;5;6;7;9;10] []] = ([1;2;4;5;6;7], [9;10])
    [get_6 [1;2;4;5;6;7;9;10] [2;21]] = ([21; 2; 1; 2; 4; 5], [6; 7; 9; 10])
*)
let rec get_6 lst acc =
  if List.length acc = 6 then List.rev acc, lst
  else
    match lst with
    | []   -> failwith "violate get_6 precondition"
    | h::t -> get_6 t (h::acc)

(** [format_button_line but_line] is the formatted [but_line] as a line of 
    calculator. 

    Example: 
    but1 =  {name = "C"; highlight = false};
    but2 = {name = "sin"; highlight = false};
    but3 = {name = "("; highlight = false};
    but4 = {name = ")"; highlight = false};
    but5 = {name = "%"; highlight = false};
    but6 = {name = "<——"; highlight = false};

    [format_button_line [but1; but2;but3; but4; but5; but6] ] =  
    |  _____  _____  _____  _____  _____  _____  |
    |  | C |  |sin|  | ( |  | ) |  | % |  |<——|  |
    |  |___|  |___|  |___|  |___|  |___|  |___|  |
 *)
let format_button_line but_line = 
  let helper acc (but:button) =
    let len = String.length but.name in 
    let n = 
      if but.highlight then "~" ^ but.name ^ "~"
      else but.name
    in
    let but1 = 
      if len = 1 then "  ║ " ^ n ^ " ║"
      else if len = 2 then "  ║ " ^ n ^ "║"
      else "  ║" ^ n ^ "║"
    in 
    acc^but1
  in
  if List.length but_line <> 6 then 
    failwith ("invalid button line, expected 6 but was " ^ string_of_int (List.length but_line))
  else 
    "    ║" ^ List.fold_left helper "" but_line ^ "  ║\n"

(** [format_button_board but_lst] is the formatted [but_lst] as a calculator's
    keyboard. 
  
  Example: 
  [format_button_board init_button] = 
  "
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
"
*)
let format_button_board but_lst =
  if List.length but_lst <> 30 then failwith "invalid button list"
  else
    let but_lst_1, l1 = get_6 but_lst [] in  
    let but_lst_2, l2 = get_6 l1 [] in 
    let but_lst_3, l3 = get_6 l2 [] in 
    let but_lst_4, but_lst_5 = get_6 l3 [] in 
    let lst = [but_lst_1; but_lst_2; but_lst_3; but_lst_4; but_lst_5] in 
    let f acc l =
      acc ^ button_top_line ^ format_button_line l ^ button_bot_line
    in
    List.fold_left f "" lst 

(** [format_board cal] is the formatted [cal] as a calculator. 
    
    Example: 
    [format_board cal] = 
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
let format_board cal = 
  let io = format_io_board cal.input cal.output in 
  let but = format_button_board cal.buttons in 
  top_out_boarder ^ io ^ but ^ bot_out_boarder

(** [extend str] is the simplified version of [str]. 

    Example: 
    [extend "sin"] = "sin"
    [extend "ab"] = "b"
    [extend "hfsdf"] = "f"
*)
let extend str = 
  let op_3 = ["sin"; "cos"; "tan"; "log"; "exp"] in 
  let op_4 = ["help"; "quit"] in 
  let len = String.length str in 
  let last = Char.escaped(str.[len-1]) in 
  if len <= 2 then last
  else
    if List.mem (String.sub str (len-3) 3) op_3 then 
      (String.sub str (len-3) 3)
    else if len >= 4 && List.mem (String.sub str (len-4) 4) op_4 then
      Char.escaped str.[len-4]
    else last

let shade str cal = 
  let f acc but = 
    let cur = if String.equal but.name str then {but with highlight = true}
              else {but with highlight = false} in
    cur::acc 
  in 
  let new_buttons = List.fold_left f [] cal.buttons |> List.rev in 
  {cal with buttons = new_buttons}

(** [make_shade cal] shades the [cal]'s input key. 
    
    Example: 
    {input = "7"; output = "";
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
    {input = "7"; output = "";
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
let make_shade cal = 
  let len = String.length cal.input in 
  if len = 0 then cal 
  else
    let cur = extend cal.input in  
    shade cur cal 

let print_board cal = 
  ANSITerminal.(print_string [ANSITerminal.Bold; magenta] "\n\n\n\n\n\n\n
   █████╗ ██╗      ██████╗ ███████╗██████╗ ██████╗  █████╗ 
  ██╔══██╗██║     ██╔════╝ ██╔════╝██╔══██╗██╔══██╗██╔══██╗
  ███████║██║     ██║  ███╗█████╗  ██████╔╝██████╔╝███████║
  ██╔══██║██║     ██║   ██║██╔══╝  ██╔══██╗██╔══██╗██╔══██║
  ██║  ██║███████╗╚██████╔╝███████╗██████╔╝██║  ██║██║  ██║
  ╚═╝  ╚═╝╚══════╝ ╚═════╝ ╚══════╝╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝\n\n\n");
  let strlst = format_board cal |> String.split_on_char '~' in 
  if List.length strlst = 1 then
    let f x =
      ANSITerminal.(print_string [ANSITerminal.Bold] x)
    in 
    let _ = List.map f strlst in 
    flush_all ();
    ()
  else
    let f acc x =
      let _ =
        if acc = 0 || acc = 2 then 
          ANSITerminal.(print_string [ANSITerminal.Bold] x)
        else 
          ANSITerminal.(print_string [ANSITerminal.Bold; red] x)
      in 
      acc+1
    in 
    let _ = List.fold_left f 0 strlst in
    flush_all ();
    ()

let print_init_board () =
  print_board init_calc

let del_one board = 
  let len = String.length board.input in 
  let new_input = 
    if len > 1 then String.sub board.input 0 (len-1) 
    else if len = 1 then ""
    else board.input
  in 
  shade "<——" {board with input = new_input}

let insert_char (ch:char) board =
  let new_input = board.input ^ Char.escaped(ch) in 
  make_shade {board with input = new_input}

let get1char () =
    let termio = Unix.tcgetattr Unix.stdin in
    let () =
        Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
            { termio with Unix.c_icanon = false; Unix.c_echo = false} in
    let res = input_char stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
    res

let eval_gui acc board = 
  try
    let t = board.input in
    (* match board.input with 
    | "quit" | "q" -> quit ()
    | "help" | "h" -> 
      help_algebra (); 
      init_calc
    | t ->  *)
    if (String.equal (String.lowercase_ascii t) "quit" ||
      String.equal (String.lowercase_ascii t) "qt" ||
      String.equal (String.lowercase_ascii t) "q") then quit ()
    else if (String.equal (String.lowercase_ascii t) "help" ||
      String.equal (String.lowercase_ascii t) "hp" ||
      String.equal (String.lowercase_ascii t) "h") then 
      (help_algebra (); init_calc)
    else
      let input = t |> parse in
      try 
        shade "=" {board with output = Hashtbl.find cache input} 
      with
        Not_found -> 
          let result = result_formatter (evaluate input) acc false in
          match String.index_opt result '\n' with 
          | Some _ -> failwith "malformated"
          | None -> 
            Hashtbl.add cache input result;
            shade "=" {board with output = result}
  with 
    e -> 
      if debug then 
        let () = print_endline (Printexc.to_string e) in
        shade "=" {board with output = "ERROR"}
      else 
        shade "=" {board with output = "ERROR"}

let rec run_gui acc board = 
  match get1char () with
  | '\n' -> 
    let new_board = eval_gui acc board in 
    let _ = print_board new_board in 
    run_gui acc init_calc
  | '\127' -> 
    let new_board = del_one board in 
    let _ = print_board new_board in 
    run_gui acc new_board
  | t -> 
    let new_board = insert_char t board in 
    let _ = print_board new_board in 
    run_gui acc new_board

let print_matrix () = 
  ANSITerminal.(print_string [ANSITerminal.Bold; yellow] "\n\n\n\n\n\n\n
  ███╗   ███╗ █████╗ ████████╗██████╗ ██╗██╗  ██╗
  ████╗ ████║██╔══██╗╚══██╔══╝██╔══██╗██║╚██╗██╔╝
  ██╔████╔██║███████║   ██║   ██████╔╝██║ ╚███╔╝ 
  ██║╚██╔╝██║██╔══██║   ██║   ██╔══██╗██║ ██╔██╗ 
  ██║ ╚═╝ ██║██║  ██║   ██║   ██║  ██║██║██╔╝ ██╗
  ╚═╝     ╚═╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝╚═╝╚═╝  ╚═╝\n\n\n");
  ()