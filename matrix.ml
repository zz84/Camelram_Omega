(** [row_length m num] checks if every row of matrix m equals num
     Rquires: num is a non-negative number *)
let row_length m num= 
  let f acc ele = acc && (List.length ele = num) in 
  List.fold_left f true m

(** [is_valid m] check if matrix m contains equal number of elements *)
let is_valid = function 
  | [] -> true
  | h::t -> row_length t (List.length h)

(** [dimension m1 m2] check if matrix m1 and m2 are both 
    valid and have the same dimension*)
let dimension m1 m2 = 
  let rows = (List.length m1  = List.length m2) in 
  let valid = is_valid m1 in 
  let f acc a b = acc && (List.length a = List.length b) in 
  List.fold_left2 f (rows&&valid) m1 m2

(** [is_square m] check if matrix m contains equal number of 
    elements in each row as the number of its rows*)
let is_square m = 
  (is_valid m) && row_length m (List.length m)

(** [list_sum x y acc] is the sum of two row vector [x] and [y]
    Raises:
      ["dimension does not agree"] failure if the length of [x] and [y] 
      is different
    
    Example:
      [list_sum [1.;3.] [2.;4.] []] is [[3.;7.]]*)
let rec list_sum x y acc= 
  match x, y with
  | [], [] -> List.rev acc
  | h1::t1, [] -> failwith "dimension does not agree"
  | [], h1::t1 -> failwith "dimension does not agree"
  | h1::t1, h2::t2 -> 
      list_sum t1 t2 (h1+.h2::acc)

let mplus x y = 
  let rec helper_mplus x y acc = 
  match x, y with
  | [],[] -> List.rev acc
  | [], h1::t1 -> failwith "dimension does not agree"
  | h1::t1, [] -> failwith "dimension does not agree"
  | h1::t1, h2::t2 ->
      helper_mplus t1 t2 ((list_sum h1 h2 [])::acc)
  in 
  helper_mplus x y []

(** [list_minus x y acc] is the  of two row vector [x] and [y]
    Raises:
      ["dimension does not agree"] failure if the length of [x] and [y] 
      is different
    
    Example:
      [list_minus [1.;3.] [2.;4.] []] is [[-1.;-1.]]*)
let rec list_minus x y acc= 
  match x, y with
  | [], [] -> List.rev acc
  | h1::t1, [] -> failwith "dimension does not agree"
  | [], h1::t1 -> failwith "dimension does not agree"
  | h1::t1, h2::t2 -> 
      list_minus t1 t2 (h1-.h2::acc)

let mminus x y = 
  let rec helper_mminus x y acc = 
  match x, y with
  | [],[] -> List.rev acc
  | [], h1::t1 -> failwith "dimension does not agree"
  | h1::t1, [] -> failwith "dimension does not agree"
  | h1::t1, h2::t2 ->
      helper_mminus t1 t2 ((list_minus h1 h2 [])::acc)
  in 
  helper_mminus x y []

let mtimes_mv x a = 
  let rec helper_mtimes x a acc = 
    match x with 
    | [] -> List.rev acc
    | h::t -> 
        let l_times = List.map (fun x -> x*.a ) h in 
        helper_mtimes t a (l_times::acc)
  in 
  helper_mtimes x a []

let mtimes_vm a x = mtimes_mv x a

(** [get_head l] returns the head element of the list [l]
    Raises: 
      ["no head"] failure if the list [l] is an empty list
      
    Example:
      [get_head [1.;2.;3.]] is [1.]
      [get_head [[1.;2.];[3.;4.]]] is [[1.;2.]]*)
let get_head l= 
  match l with 
  | [] -> failwith "no head"
  | h::t -> h

(** [get_tail l] returns the tail list of the list [l]
    Raises: 
      ["no tail"] failure if the list [l] is an empty list
      
    Example:
      [get_tail [1.;2.;3.]] is [2.;3.]
      [get_head [[1.;2.];[3.;4.]]] is [[3.;4.]]*)
let get_tail l = 
  match l with
  | [] -> failwith "no tail"
  | h::t -> t

let rec transpose x  =
  let rec helper_transpose l acc = 
    match l with
    | [] -> List.rev acc
    | []::h -> List.rev acc
    | x -> helper_transpose (List.map get_tail x) (List.map get_head x :: acc) 
  in 
  helper_transpose x []

(** [row_times l1 l2 acc] is the dot product of two row vector [l1] [l2]
    Raises:
      ["dimensions not matched"] failure if the length of [l1] and [l2]
      is different
      
    Example:
      [row_times [1.;2.] [3.;4.] []] is [12.]*)
let rec row_times l1 l2 acc = 
  match l1, l2 with 
  | [], [] -> acc
  | [], h::t -> failwith "dimensions not match"
  | h::t, [] -> failwith "dimensions not match"
  | h1::t1, h2::t2 -> row_times t1 t2 ((h1*.h2)+.acc)

(** [helper_col x l acc] is the matrix after multiplying column vector [l] to 
    matrix [x]
    Raises:
      ["dimensions not matched"] failure if the number of columns in matrix [x]
      is different from the dimension of [l]
      
    Example:
      [helper_mcol [[1.;0.];[0.;1.] [1.;2.] []] is [1.;2.]*)
let rec helper_mcol x l acc = 
  match x with 
  | [] -> List.rev acc
  | h::t -> helper_mcol t l ((row_times h l 0.)::acc)

let mtimes_mm x y = 
  let new_y = transpose y in 
  let rec helper_times x y acc=
  match y with 
  | [] -> List.rev acc
  | h::t -> helper_times x t ((helper_mcol x h [])::acc)
  in 
  transpose (helper_times x new_y [])

let mdivide_mv x a = 
  let rec helper_mdivide x a acc = 
    match x with 
    | [] -> List.rev acc
    | h::t -> 
        let l_div = List.map (fun x -> x/.a ) h in 
        helper_mdivide t a (l_div::acc)
  in 
  helper_mdivide x a []

let mpower x a = 
  if is_square x then begin
    let rec compute count x res =
      if count = 0. then res
      else compute (count-.1.) x (mtimes_mm res x)
    in
    compute (a-.1.) x x
  end
  else failwith "can only power on square matrix"

(** [get_nth_number lst n counter] is the [n]th element of the list [lst]
    Raises:
      ["out of bound"] failure if the required index [n] is greater than the
      maximum index of the list
      
    Example:
      [get_nth_number [1;2;3] 1 0] is [2]*)
let rec get_nth_number lst n counter= 
  match lst with 
  | [] -> failwith "out of bound"
  | h::t -> 
      if counter = n then h 
      else get_nth_number t n (counter + 1) 

let tr x = 
  if is_square x then begin
  let rec compute x counter res= 
    match x with 
    | [] -> res
    | h::t -> compute t (counter+1) (res+.get_nth_number h counter 0)
  in
  compute x 0 0.0
  end
  else failwith "can only compute trace on square matrix"

(** [get_list_rest lst n] is the list without the [n]th number of the 
    list [lst]
    Raises:
      ["out of bound"] failure if the required index [n] is greater than the
      maximum index of list [lst]
      
    Examples:
      [get_list_rest [1;2;3] 1] is [1;3]*)
let get_list_rest lst n = 
  let rec helper lst idx n acc = 
    match lst with 
    | [] -> List.rev acc
    | h::t -> 
        if idx = n then helper t (idx+1) n acc
        else helper t (idx+1) n (h::acc)
  in 
  helper lst 0 n []

let det x = 
  if is_square x then begin
  let rec helper_det x n acc = 
    if List.length x = 1 then get_head (get_head x)
    else if n = List.length x then acc
    else
      let obj = (get_nth_number x n 0) in (*get the line of the multiplier*)
      let lst_rest = get_list_rest x n in (*the rest of the matrix, not square*)
      let rest_square = List.map get_tail lst_rest in (*now square*)
      let mul = get_head obj in (*multiplier*) 
      let res = 
        if (n mod 2) = 1 then (-1.)*.mul*.(helper_det rest_square 0 0.)
        else mul*.(helper_det rest_square 0 0.)
      in 
      helper_det x (n+1) (acc+.res)
  in 
  helper_det x 0 0.
  end
  else failwith "can only perform determinant on square matrix"

let adjoint x = 
  if is_square x then begin
  let rec helper x row acc = 
    if row = List.length x then List.rev acc
    else begin
      let rec helper_itercol x r idx acc = 
        if idx = List.length x then List.rev acc 
        else begin
          let without_r = get_list_rest x r in (* without row r, not square*)
          let rest_square = 
            List.map (fun x -> get_list_rest x idx) without_r in (*square*)
          let cofactor = 
            if (idx + r) mod 2 = 0 then det rest_square
            else (-1.)*.det rest_square in (*cofactor*)
          helper_itercol x r (idx+1) (cofactor::acc)
        end
      in
      let res_r = helper_itercol x row 0 [] in (*float list, res of a row*)
      helper x (row+1) (res_r::acc)
    end
  in 
  transpose (helper x 0 [])
  end 
  else failwith "adjoint can only compute on square matrix"

let inv x = 
  if det x = 0. then failwith "not invertible"
  else mdivide_mv (adjoint x) (det x)

(** [eye a] is the identity matrix with size [a]*[a].

    Example:
      [eye 2] is [[1.;0.];[1.;0.]]*)
let eye a = 
  let rec helper n idx limit acc = 
    if idx = limit then List.rev acc
    else if idx = n then helper n (idx+1) limit (1.::acc)
    else helper n (idx+1) limit (0.::acc)
  in 
  let rec create_eye counter a acc = 
    if counter = a then List.rev acc 
    else create_eye (counter + 1) a ((helper counter 0 a [])::acc)
  in 
  create_eye 0 a []

(** [sum_lst_sq l] is the Euclidean norm of the list [l]

    Example:
      [sum_lst_sq [1.;2.]] is [2.23606]*)
let sum_lst_sq l = 
  let rec helper l res = 
    match l with 
    | [] -> sqrt res
    | h::t -> helper t (h**2.+.res)
  in
  helper l 0.

let eigval x = 
  if is_square x then begin
  let t = tr x in (*trace*)
  let d = det x in (*determinant*)
  let l1 = t/.2. +. sqrt (t**2./.4. -. d) in 
  let l2 = t/.2. -. sqrt (t**2./.4. -. d) in 
  if l1 = nan || l2 = nan then 
  failwith "no real solutions"
  else [l1]::[l2]::[]
  end 
  else failwith "eigenvalues happen on square matrix"

(** [sort_lst lst] is the 2 dimension list [lst] that is sorted in descending 
    order
    
    Example:
      [sort_lst [[1.];[2.];[0.]] is [[2.];[1.];[0.]]*)
let sort_lst lst = 
  lst |> List.flatten |> List.sort compare |> List.rev 

let eigvec x = 
  let l = sort_lst (eigval x) in 
  let l1 = get_head l in 
  let l2 = get_head (get_list_rest l 0) in 
  let a = get_head (get_nth_number x 0 0) in 
  let b = get_nth_number (get_head x) 1 0 in 
  let c = get_head (get_nth_number x 1 0) in 
  let d = get_nth_number (get_nth_number x 1 0) 1 0 in 
  if b = 0. && c = 0.
  then begin
    [[1.;0.];[0.;1.]]
  end
  else if b <> 0. then begin
    [[b;b];[l1-.a;l2-.a]]
  end
  else if c <> 0. then begin
    [[l1-.d;l2-.d];[c;c]]
  end
  else
    failwith "shouldn't be like that"

(** [create_diag x res] is the diagonal matrix whose diagonal entries are 
    respectively the values in list [x]
    
    Example:
      [create_diag [[1.];[2.]] []] is [[[1.;0.];[0.;2.]]]*)
let rec create_diag x res= 
  match x with 
  | [] -> List.rev res
  | h::[] -> create_diag [] ((0.::h)::res)
  | h::t -> create_diag t ((h@[0.])::res)

let diag x = 
  if List.length x > 2 then failwith "only support 2D"
  else if is_square x then begin
  let l = create_diag (eigval x) [] in 
  let p = eigvec x in 
  match l with 
  | [] -> failwith "not diagonalizable"
  | h::t when h = List.flatten t-> failwith "not diagonalizable" (*geometric mul*)
  | _ ->(p, l, inv p)
  end
  else failwith "diagnolization happens on square matrix"

(** [sort_lst lst] is the 2 dimension list [lst] that is sorted in descending 
    order with each element is the square root of the original list
    
    Example:
      [sort_lst [[1.];[2.];[0.]] is [[1.414];[1.];[0.]]*)
let sort_lst_sq lst = 
  lst |> List.flatten |> List.sort compare |> List.rev 
  |> List.map (fun x -> (sqrt x)::[])

(** [normalize l] is the normalized list [l]
    
    Example:
      [normalize [1.;3.]] is [0.5; 1.5]*)
let normalize l = 
  let sum = sum_lst_sq l in 
  List.map (fun x -> x/.sum) l

(** [ortho x] is the orthogonalized matrix [x]

    Example:
      [ortho [[1.;3.];[2.;2.]]] is [[[0.5;1.5];[1.;1.]]]*)
let ortho x = 
  let new_x = transpose x in 
  List.map normalize new_x

let svd x = 
  let at = transpose x in 
  let ata = mtimes_mm at x in (*step 1*)
  let eig = eigval ata in 
  let sq_sort_eig = sort_lst_sq eig in (*step 2*)
  let s = create_diag sq_sort_eig [] in 
  let sinv = inv s in (*step 3*)
  let v_init = eigvec ata in 
  let v = ortho v_init in 
  let vt = transpose v in (*step 4*)
  let u = mtimes_mm (mtimes_mm x v) sinv in 
  (u, s, vt)

