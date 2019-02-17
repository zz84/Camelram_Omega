open OUnit2
open AST 
open BasicOperation
open Gui
open Main_gui
open Matrix
open Cal_gui
open Derivative
open Integral

(* set up variables for later test use *)
let expected1 = Node (BOperation Plus, Val (Float 1.), Val (Float 2.)) 
let expected2 = Node (BOperation Times, Val (Float 1.), Val (Float 2.)) 
let expected3 = Node (BOperation Div, Val (Float 1.), Val (Float 2.)) 
let expected4 = Node (BOperation Minus, Val (Float 0.), Val (Float 1.)) 
let expected5 = Node (BOperation Minus, Val (Float 1.), Val (Float 2.)) 
let expected6 = Node (BOperation Combin, Val (Float 5.), Val (Float 3.)) 
let expected7 = Node (BOperation Permut, Val (Float 5.), Val (Float 3.)) 
let expected8 = Node (BOperation Power, Val (Float 2.), Val (Float 3.)) 
let expected9 = Node (BOperation Plus, Val (Float 1.), 
                      Node (BOperation Power, Val (Float 2.), Val (Float 3.))) 
let expected10 = Node (BOperation Plus,  
                       Node (BOperation Power, Val (Float 2.), Val (Float 3.)),
                       Val (Float 1.)) 

let expected11 = (Node (BOperation Plus, Val (Float 3.), Val (Float 8.9)))

let ast_tests =
  [
    (** parse tests *)
    (* one operations tests *)
    (* plus tests *)
    "parse_add1" >:: (fun _ -> assert_equal expected1 (parse "1 + 2"));
    "parse_add2" >:: (fun _ -> assert_equal expected1 (parse "1 +      2"));
    "parse_add3" >:: (fun _ -> assert_equal expected1 (parse "    1 +      2"));
    "parse_add4" >:: (fun _ -> assert_equal expected1 (parse "1+2"));

    (* minus tests *)
    "parse_add1" >:: (fun _ -> assert_equal expected5 (parse "1 - 2"));
    "parse_add2" >:: (fun _ -> assert_equal expected5 (parse "1 -      2"));
    "parse_add3" >:: (fun _ -> assert_equal expected5 (parse "    1 -      2"));
    "parse_add4" >:: (fun _ -> assert_equal expected5 (parse "1-2"));

    (* times tests *)
    "parse_times1" >:: (fun _ -> assert_equal expected2 (parse "1*2"));
    "parse_times2" >:: (fun _ -> assert_equal expected2 (parse "1   *2"));
    "parse_times3" >:: (fun _ -> assert_equal expected2 (parse "    1  * 2"));
    "parse_times4" >:: (fun _ -> assert_equal expected2 (parse "1*     2"));

    (* divide tests *)
    "parse_divide1" >:: (fun _ -> assert_equal expected3 (parse "1/2"));
    "parse_divide2" >:: (fun _ -> assert_equal expected3 (parse "1   /2"));
    "parse_divide3" >:: (fun _ -> assert_equal expected3 (parse "    1  / 2"));
    "parse_divide4" >:: (fun _ -> assert_equal expected3 (parse "1/     2"));

    (* negate tests *)
    "parse_negate1" >:: (fun _ -> assert_equal expected4 (parse " - 1"));
    "parse_negate2" >:: (fun _ -> assert_equal expected4 (parse "-       1"));
    "parse_negate3" >:: (fun _ -> assert_equal expected4 (parse "-1"));

    (* combination tests *)
    "parse_combination1" >:: (fun _ -> assert_equal expected6 
                                 (parse "5 C 3"));
    "parse_combination2" >:: (fun _ -> assert_equal expected6 
                                 (parse "5 C    3"));
    "parse_combination3" >:: (fun _ -> assert_equal expected6 
                                 (parse "5C3"));
    "parse_combination4" >:: (fun _ -> assert_equal expected6 
                                 (parse "     5 C 3"));
    "parse_combination5" >:: (fun _ -> assert_equal expected6 
                                 (parse "5 C 3    "));

    (* permutation tests *)
    "parse_permutation1" >:: (fun _ -> assert_equal expected7 (parse "5 P 3"));
    "parse_permutation2" >:: (fun _ -> assert_equal expected7 (parse "5 P    3"));
    "parse_permutation3" >:: (fun _ -> assert_equal expected7 (parse "5P3"));
    "parse_permutation4" >:: (fun _ -> assert_equal expected7 (parse "     5 P 3"));
    "parse_permutation5" >:: (fun _ -> assert_equal expected7 (parse "5 P 3    "));

    (* power tests *)
    "parse_power1" >:: (fun _ -> assert_equal expected8 (parse "2^3"));
    "parse_power2" >:: (fun _ -> assert_equal expected8 (parse "2   ^3"));
    "parse_power3" >:: (fun _ -> assert_equal expected8 (parse "    2  ^ 3"));
    "parse_power4" >:: (fun _ -> assert_equal expected8 (parse "2^     3"));

    (* two operations tests *)
    "parse_2operations1" >:: (fun _ -> assert_equal expected9 (parse "1 + 2^3"));
    "parse_2operations2" >:: (fun _ -> assert_equal expected10 (parse "2^3 + 1"));

    (* tests for format *)
    (* "format_test_float" >::(fun _ -> assert_equal "3."("3."|> parse |> format));
       "format_test_bop" >::(fun _ -> assert_equal  "(3.+8.9)"
                             (format expected11)); *)

    (* tests for evaluate *)
    "evaluate_test_float" >::(fun _ -> assert_equal (Float 11.9)
                                 (evaluate  expected11));
    (*"evaluate_test_single_float" >::(fun _ -> assert_equal (Float 0.4)
                                        (evaluate Val (Float 0.4)));*)

    (* tests for result_formatter *)
    "result_formatter_test_float2" >::(fun _ -> assert_equal "3" 
                                          (result_formatter 
                                             (Float 3.) 2 true));
    "result_formatter_test_float3" >::(fun _ -> assert_equal "3" 
                                          (result_formatter 
                                             (Float 3.) 3 true));
    "result_formatter_test_float4" >::(fun _ -> assert_equal "3" 
                                          (result_formatter 
                                             (Float 3.) 4 true));
    "result_formatter_test_matrix3" >::(fun _ -> assert_equal "[ 1   ]\n"
                                           (result_formatter 
                                              (Matrix [[1.]]) 2 true));
    "result_formatter_test_matrix3" >::(fun _ -> assert_equal "[ 1   ]\n"
                                           (result_formatter (
                                               Matrix [[1.]]) 3 true));
    "result_formatter_test_matrix4" >::(fun _ -> assert_equal "[ 1   ]\n"
                                           (result_formatter 
                                              (Matrix [[1.]]) 4 true));

  ]
let converge_eq x1 x2 = 
  abs_float (x1 -. x2) < 1e-13

let basic_op_tests =
  [

    (* tests for plus *)
    "plus_test1" >:: (fun _ -> assert_equal 0.0 (plus 0.0 0.0));
    "plus_test2" >:: (fun _ -> assert_equal 0.0 (plus (-1.0) 1.0));
    "plus_test3" >:: (fun _ -> assert_equal 1.3 (plus 0.0 1.3));
    "plus_test4" >:: (fun _ -> assert_equal true (converge_eq 1.3 (plus 0.6 0.7)));
    (* tests for minus *)
    "minus_test1" >:: (fun _ -> assert_equal 0.0 (minus 0.0 0.0));
    "minus_test2" >:: (fun _ -> assert_equal 1.0 (minus 1.0 0.0));
    "minus_test3" >:: (fun _ -> assert_equal (-1.0) (minus 0.0 1.0));
    "minus_test4" >:: (fun _ -> assert_equal 1.2 (minus 2.4 1.2));
    (* tests for times *)
    "times_test1" >:: (fun _ -> assert_equal 0.0 (times 1.0 0.0));
    "times_test2" >:: (fun _ -> assert_equal 0.12 (times 0.2 0.6));
    "times_test3" >:: (fun _ -> assert_equal (-0.12) (times (-0.2) 0.6));
    "times_test4" >:: (fun _ -> assert_equal 0.12 (times (-0.2) (-0.6)));
    (* tests for divide *)
    "divide_test1" >:: (fun _ -> assert_equal 0.5 (divide 1.0 2.0));
    "divide_test2" >:: (fun _ -> assert_equal 2.0 (divide 2.0 1.0));
    "divide_test3" >:: (fun _ -> assert_equal (-0.5) (divide (-1.0) 2.0));
    "divide_test4" >:: (fun _ -> assert_equal 0.0 (divide 0.0 1.0));
    "divide_test5" >:: (fun _ -> assert_equal 0.0 (divide 0.0 (-1.0))); 
    (* tests for power *)
    "power_test1" >:: (fun _ -> assert_equal 4.0 (power 2.0 2.0));
    "power_test2" >:: (fun _ -> assert_equal 1.0 (power 2.0 0.0));
    "power_test3" >:: (fun _ -> assert_equal 2.0 (power 2.0 1.0));
    (* tests for combination *)
    "combo_test1" >:: (fun _ -> assert_equal 1.0 (combination 5.0 0.0));
    "combo_test2" >:: (fun _ -> assert_equal 5.0 (combination 5.0 1.0));
    "combo_test3" >:: (fun _ -> assert_equal 10.0 (combination 5.0 2.0));
    "combo_test4" >:: (fun _ -> assert_equal 10.0 (combination 5.0 3.0));
    "combo_test5" >:: (fun _ -> assert_equal 5.0 (combination 5.0 4.0));
    "combo_test6" >:: (fun _ -> assert_equal 1.0 (combination 5.0 5.0));
    (* tests for permutation *)
    "permu_test1" >:: (fun _ -> assert_equal 1.0 (permutation 5.0 0.0));
    "permu_test2" >:: (fun _ -> assert_equal 5.0 (permutation 5.0 1.0));
    "permu_test3" >:: (fun _ -> assert_equal 20.0 (permutation 5.0 2.0));
    "permu_test4" >:: (fun _ -> assert_equal 60.0 (permutation 5.0 3.0));
    "permu_test5" >:: (fun _ -> assert_equal 120.0 (permutation 5.0 4.0));
    "permu_test6" >:: (fun _ -> assert_equal 120.0 (permutation 5.0 5.0));
    (* tests for log *)
    "log_test1" >:: (fun _ -> assert_equal 0.0 (log 1.0));
    (* tests for exp *)
    "exp_test1" >:: (fun _ -> assert_equal 1.0 (exp 0.0));
    (* combination test for log and exponential*)
    "lnExp_test1" >:: (fun _ -> assert_equal 5.0 (5.0 |> exp |> log));
    "lnExp_test2" >:: (fun _ -> assert_equal 0.0 (0.0 |> exp |> log));
    "lnExp_test3" >:: (fun _ -> assert_equal 2.4 (2.4 |> exp |> log));
    "lnExp_test4" >:: (fun _ -> assert_equal (-2.0) ((-2.0) |> exp |> log));
    "lnExp_test5" >:: (fun _ -> assert_equal (-2.2) ((-2.2) |> exp |> log));
    "lnExp_test6" >:: (fun _ -> assert_equal true 
                          (converge_eq 5.0 (5.0 |> log |> exp)));
    "lnExp_test7" >:: (fun _ -> assert_equal 0.0 (0.0 |> log |> exp));
    "lnExp_test8" >:: (fun _ -> assert_equal 2.4 (2.4 |> log |> exp));
    (* tests for sin *)
    "sin_test1" >:: (fun _ -> assert_equal 0.0 (sin 0.0));
    (* tests for cos *)
    "cos_test1" >:: (fun _ -> assert_equal 1.0 (cos 0.0));
    (* tests for tan *)
    "tan_test1" >:: (fun _ -> assert_equal 0.0 (tan 0.0));
    (* combination test for trigometry*)
    "trig_test1" >:: (fun _ -> assert_equal true 
                         (converge_eq (tan 1.0) (divide (sin 1.0) (cos 1.0))));
    "trig_test2" >:: (fun _ -> assert_equal true 
                         (converge_eq (tan 1.2) (divide (sin 1.2) (cos 1.2))));
    "trig_test3" >:: (fun _ -> assert_equal true 
                         (converge_eq (tan 2.3) (divide (sin 2.3) (cos 2.3))));
    (* combination tests for asin *)
    "arcsin_test1" >:: (fun _ -> assert_equal true 
                           (converge_eq 1.2 (1.2 |> sin |> asin)));
    "arcsin_test2" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.8 (0.8 |> asin |> sin)));
    "arcsin_test3" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.0 (0.0 |> sin |> asin)));
    "arcsin_test4" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.0 (0.0 |> asin |> sin)));
    (* "arcsin_test5" >:: (fun _ -> assert_equal true 
       (converge_eq 3.14 (3.14 |> sin |> asin))); *)
    "arcsin_test6" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.14 (0.14 |> asin |> sin)));
    (* "arcsin_test7" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> sin |> asin)));
       "arcsin_test8" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> asin |> sin))); *)
    (* tests for acos *)
    "arccos_test1" >:: (fun _ -> assert_equal true 
                           (converge_eq 1.2 (1.2 |> cos |> acos)));
    "arccos_test2" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.8 (0.8 |> acos |> cos)));
    "arccos_test3" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.0 (0.0 |> cos |> acos)));
    "arccos_test4" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.0 (0.0 |> acos |> cos)));
    (* "arccos_test5" >:: (fun _ -> assert_equal true 
       (converge_eq 3.14 (3.14 |> cos |> acos))); *)
    "arccos_test6" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.14 (0.14 |> acos |> cos)));
    (* "arccos_test7" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> cos |> acos)));
       "arccos_test8" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> acos |> cos))); *)
    (* tests for atan *)
    "arctan_test1" >:: (fun _ -> assert_equal true 
                           (converge_eq 1.2 (1.2 |> tan |> atan)));
    "arctan_test2" >:: (fun _ -> assert_equal true 
                           (converge_eq 1.2 (1.2 |> atan |> tan)));
    "arctan_test3" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.1 (0.1 |> tan |> atan)));
    "arctan_test4" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.1 (0.1 |> atan |> tan)));
    "arctan_test5" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.14 (0.14 |> tan |> atan)));
    "arctan_test6" >:: (fun _ -> assert_equal true 
                           (converge_eq 0.14 (0.14 |> atan |> tan)));
    (* "arctan_test7" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> tan |> atan)));
       "arctan_test8" >:: (fun _ -> assert_equal true 
       (converge_eq (-1.9) ((-1.9) |> atan |> tan))); *)

  ]



let matrix_tests = [
  (* tests for mplus *)
  "mplus_test1" >:: (fun _ -> assert_equal [[3.8;4.2;7.0]]
                        (mplus [[1.9;2.1;3.5]] [[1.9;2.1;3.5]]));

  "mplus_test2" >:: (fun _ -> assert_equal [[3.8;4.2;7.0]]
                        (mplus [[3.8;4.2;7.0]] [[0.0;0.0;0.0]]));

  "mplus_test3" >:: (fun _ -> assert_equal [[0.0;0.0;0.0]]
                        (mplus [[0.0;0.0;0.0]] [[0.0;0.0;0.0]]));

  (* tests for mminus *)
  "mminus_test1" >:: (fun _ -> assert_equal [[0.0;0.0;0.0]]
                         (mminus [[1.9;2.1;3.5]] [[1.9;2.1;3.5]]));

  "mminus_test2" >:: (fun _ -> assert_equal [[0.0;0.0;0.0]]
                         (mminus [[0.0;0.0;0.0]] [[0.0;0.0;0.0]]));

  "mminus_test3" >:: (fun _ -> assert_equal [[1.9;2.1;3.5]]
                         (mminus [[3.8;4.2;7.0]] [[1.9;2.1;3.5]]));

  (* tests for mtimes_mm *)
  "mtimes_mm_test1" >:: (fun _ -> assert_equal [[2.;5.];[1.;9.]]
                            (mtimes_mm [[1.;0.];[0.;1.]] [[2.;5.];[1.;9.]]));

  "mtimes_mm_test2" >:: (fun _ -> assert_equal [[3.; 14.]; [3.; 14.]]
                            (mtimes_mm [[1.;1.];[1.;1.]] [[2.;5.];[1.;9.]]));

  "mtimes_mm_test3" >:: (fun _ -> assert_equal [[2091.; 589.]; [1200.; 9400.]]
                            (mtimes_mm [[99.;1.];[0.;100.]] [[21.;5.];[12.;94.]]));

  (* tests for mtimes_mv *)
  "mtimes_mv_test1" >:: (fun _ -> assert_equal [[2.;14.];[4.;6.]]
                            (mtimes_mv [[1.;7.];[2.;3.]] 2.));

  "mtimes_mv_test2" >:: (fun _ -> assert_equal [[0.; 0.]; [0.; 0.]]
                            (mtimes_mv [[1.;1.];[1.;1.]] 0.));

  "mtimes_mv_test3" >:: (fun _ -> assert_equal [[9801.; 99.]; [0.; 9900.]]
                            (mtimes_mv [[99.;1.];[0.;100.]] 99.));

  (* tests for mtimes_vm *)
  "mtimes_vm_test1" >:: (fun _ -> assert_equal [[2.;14.];[4.;6.]]
                            (mtimes_vm 2. [[1.;7.];[2.;3.]]));

  "mtimes_vm_test2" >:: (fun _ -> assert_equal [[0.; 0.]; [0.; 0.]]
                            (mtimes_vm 0. [[1.;1.];[1.;1.]]));

  "mtimes_vm_test3" >:: (fun _ -> assert_equal [[9801.; 99.]; [0.; 9900.]]
                            (mtimes_vm 99. [[99.;1.];[0.;100.]]));

  (* tests for mdivide_mv *)
  "mdivide_mv_test1" >:: (fun _ -> assert_equal [[2.;14.];[4.;6.]]
                             (mdivide_mv [[1.;7.];[2.;3.]] 0.5));

  "mdivide_mv_test2" >:: (fun _ -> assert_equal [[10.; 10.]; [10.; 10.]]
                             (mdivide_mv [[1.;1.];[1.;1.]] 0.1));

  "mdivide_mv_test3" >:: (fun _ -> assert_equal [[49.5; 0.5]; [0.; 50.]]
                             (mdivide_mv [[99.;1.];[0.;100.]] 2.));

  (* tests for mpower *)
  "mpower_test1" >:: (fun _ -> assert_equal [[15.; 28.]; [8.; 23.]]
                         (mpower [[1.;7.];[2.;3.]] 2.));

  "mpower_test2" >:: (fun _ -> assert_equal [[1.;1.];[1.;1.]]
                         (mpower [[1.;1.];[1.;1.]] 1.));

  "mpower_test3" >:: (fun _ -> assert_equal 
                         [[9.04382075008804454e+19; 9.56179249911955e+18]; 
                          [0.; 1e+20]]
                         (mpower [[99.;1.];[0.;100.]] 10.));

  (* tests for transpose *)
  "transpose_test1" >:: (fun _ -> assert_equal [[1.; 3.; 5.]; [2.; 4.; 6.]]
                            (transpose [[1.;2.];[3.;4.];[5.;6.]]));

  "transpose_test2" >:: (fun _ -> assert_equal [[1.;1.];[1.;1.]]
                            (transpose [[1.;1.];[1.;1.]]));

  "transpose_test3" >:: (fun _ -> assert_equal 
                            [[99.; 0.]; [1.; 100.]; [76.; 88.]]
                            (transpose [[99.;1.;76.];[0.;100.;88.]]));

  (* tests for adjoint *)
  "adjoint_test1" >:: (fun _ -> assert_equal [[4.; -2.]; [-3.; 1.]]
                          (adjoint [[1.;2.];[3.;4.]]));

  "adjoint_test2" >:: (fun _ -> assert_equal 
                          [[-4026.3; 1112.3; 23.5]; 
                           [1068.8; -293.3; -23.5]; [91.; -91.; 0.]]
                          (adjoint [[1.;1.;9.];[1.;1.;32.5]; [33.;124.;3.7]]));

  "adjoint_test3" >:: (fun _ -> assert_equal 
                          [[0.; -0.; 0.; -0.]; [-0.; 0.; -0.; 0.]; 
                           [0.; -0.; 0.; -0.];[-0.; 0.; -0.; 0.]]
                          (adjoint [[99.;1.;76.;33.3];[0.;100.;88.;2.];
                                    [99.;1.;76.;33.3];[0.;100.;88.;2.]]));

  (* tests for inv *)
  "inv_test1" >:: (fun _ -> assert_equal 
                      [[-0.272727272727272707; 0.636363636363636354];
                       [0.181818181818181823; -0.0909090909090909116]]
                      (inv [[1.;7.];[2.;3.]]));

  "inv_test2" >:: (fun _ -> assert_equal 
                      [[0.111111111111111105; -0.111111111111111105];
                       [-0.111111111111111105; 1.11111111111111116]]
                      (inv [[10.;1.];[1.;1.]]));

  "inv_test3" >:: (fun _ -> assert_equal 
                      [[0.0101010101010101019; -0.00010101010101010101]; 
                       [-0.; 0.01]]
                      (inv [[99.;1.];[0.;100.]]));

  (* tests for diag *)
  "diag_test1" >:: (fun _ -> assert_equal 
                       ([[7.; 7.]; [4.87298334620741702; -2.87298334620741702]],
                        [[5.87298334620741702; 0.]; [0.; -1.87298334620741702]],
                        [[0.052985793589488496; 0.129099444873580577];
                         [0.089871349267654374; -0.129099444873580577]])
                       (diag [[1.;7.];[2.;3.]]));

  "diag_test2" >:: (fun _ -> assert_equal 
                       ([[1.; 1.]; [0.109772228646443537; -9.10977222864644354]],
                        [[10.1097722286464435; 0.]; [0.; 0.890227771353556463]],
                        [[0.988093530091976424; 0.108465228909328082];
                         [0.0119064699080236; -0.108465228909328082]])
                       (diag [[10.;1.];[1.;1.]]));

  "diag_test3" >:: (fun _ -> assert_equal 
                       ([[1.; 1.]; [1.; 0.]], 
                        [[100.; 0.]; [0.; 99.]], [[-0.; 1.]; [1.; -1.]])
                       (diag [[99.;1.];[0.;100.]]));

  "diag_test4" >:: (fun _ -> assert_equal 
                       ([[220.; 220.]; 
                         [3.83386228072356516; -573.833862280723565]],
                        [[692.833862280723565; 0.]; [0.; 115.166137719276435]],
                        [[0.00451528729533893364; 0.00173109896482303649];
                         [3.01672501156120435e-05; -0.00173109896482303649]])
                       (diag [[689.;220.];[10.;119.]]));

  (* tests for det *)
  "det_test1" >:: (fun _ -> assert_equal (-2.) (det [[1.;2.];[3.;4.]]));

  "det_test2" >:: (fun _ -> assert_equal (-2138.5)
                      (det [[1.;1.;9.];[1.;1.;32.5]; [33.;124.;3.7]]));

  "det_test3" >:: (fun _ -> assert_equal 0.
                      (det [[99.;1.;76.;33.3];[0.;100.;88.;2.];
                            [99.;1.;76.;33.3];[0.;100.;88.;2.]]));

  (* tests for eigval *)
  "eigval_test1" >:: (fun _ -> assert_equal 
                         [[5.87298334620741702]; [-1.87298334620741702]] 
                         (eigval [[1.;7.];[2.;3.]]));

  "eigval_test2" >:: (fun _ -> assert_equal 
                         [[10.1097722286464435]; [0.890227771353556463]] 
                         (eigval [[10.;1.];[1.;1.]]));

  "eigval_test3" >:: (fun _ -> assert_equal [[100.]; [99.]]
                         (eigval [[99.;1.];[0.;100.]]));

  (* tests for eigvec *)
  "eigvec_test1" >:: (fun _ -> assert_equal 
                         [[7.; 7.]; [4.87298334620741702; -2.87298334620741702]]
                         (eigvec [[1.;7.];[2.;3.]]));

  "eigvec_test2" >:: (fun _ -> assert_equal 
                         [[1.; 1.]; [0.109772228646443537; -9.10977222864644354]]
                         (eigvec [[10.;1.];[1.;1.]]));

  "eigvec_test3" >:: (fun _ -> assert_equal [[1.; 1.]; [1.; 0.]]
                         (eigvec [[99.;1.];[0.;100.]]));

  "TR_test1" >:: (fun _ -> assert_equal (Float 10.) 
                     (parse "TR [[1 2][5 9]]" |> evaluate));
  "TR_test2 " >:: (fun _ -> assert_equal (Float 10.) 
                      (parse "TR [[1 2][5 9        ]]" |> evaluate));
  "TR_test3" >:: (fun _ -> assert_equal (Float 10.) 
                     (parse "TR [[1 2][5 9]     ]" |> evaluate));
  "TR_test4" >:: (fun _ -> assert_equal (Float 10.) 
                     (parse "TR        [    [1 2][5 9]]" |> evaluate));
  "TR_test5" >:: (fun _ -> assert_equal(Float 15.)  
                     (parse "TR [[1 22 7][5 9 61][8 23 5]]" |> evaluate));
  "TR_test6" >:: (fun _ -> assert_equal(Float 15.) 
                     (parse "TR [[1 22 7][    5 9 61][8 23 5]]" |> evaluate));
  "TR_test7" >:: (fun _ -> assert_equal (Float 15.) 
                     (parse "TR [[1 22 7][5 9 61][     8 23 5]]" |> evaluate));
  "TR_test8" >:: (fun _ -> assert_equal (Float 15.) 
                     (parse "TR [     [1 22 7][5 9 61][8 23 5]]" |> evaluate));
  "TR_test9" >:: (fun _ -> assert_equal (Float 15.) 
                     (parse "TR [[  1  22 7]   [5 9 61][8 23 5]]" |> evaluate));

  (* tests for svd *)
  "SVD_test1" >:: (fun _ -> assert_equal 
                      (MatrixSVD ([[3.; 3.]; [6.; -2.]], [[7.; 0.]; [0.; -1.]],
                                  [[0.0833333333333333287; 0.125]; [0.25; -0.125]])) 
                      ("SVD [[1 3][4 5 ]]" |> parse |> evaluate));

  "SVD_test2" >:: (fun _ -> assert_equal 
                      (MatrixSVD ([[3.; 3.]; [6.; -2.]], [[7.; 0.]; [0.; -1.]],
                                  [[0.0833333333333333287; 0.125]; [0.25; -0.125]])) 
                      ("SVD [[1 3][         4 5 ]]" |> parse |> evaluate));

  "SVD_test3" >:: (fun _ -> assert_equal 
                      (MatrixSVD ([[3.; 3.]; [6.; -2.]], [[7.; 0.]; [0.; -1.]],
                                  [[0.0833333333333333287; 0.125]; [0.25; -0.125]])) 
                      ("SVD [[      1 3     ][4 5 ]]" |> parse |> evaluate));

  "SVD_test4" >::(fun _ -> assert_equal
                     (MatrixSVD ([[9.; 9.]; 
                                  [20.9187404563938628; -15.9187404563938628]],
                                 [[21.9187404563938628; 0.]; 
                                  [0.; -14.9187404563938628]],
                                 [[0.0480149265305846798; 0.0271462644898951555];
                                  [0.0630961845805264321; -0.0271462644898951555]]))
                     ("SVD [[1 9][ 37 6]]"|>parse |> evaluate ));

  "SVD_test5" >::(fun _ -> assert_equal
                     (MatrixSVD ([[9.; 9.]; 
                                  [20.9187404563938628; -15.9187404563938628]],
                                 [[21.9187404563938628; 0.]; 
                                  [0.; -14.9187404563938628]],
                                 [[0.0480149265305846798; 0.0271462644898951555];
                                  [0.0630961845805264321; -0.0271462644898951555]]))
                     ("SVD [[          1 9][ 37 6       ]]"|>parse |> evaluate ));

  "SVD_test5" >::(fun _ -> assert_equal
                     (MatrixSVD ([[9.; 9.]; 
                                  [20.9187404563938628; -15.9187404563938628]],
                                 [[21.9187404563938628; 0.]; 
                                  [0.; -14.9187404563938628]],
                                 [[0.0480149265305846798; 0.0271462644898951555];
                                  [0.0630961845805264321; -0.0271462644898951555]]))
                     ("SVD [[ 1 9            ][        37 6]]"|>parse |> evaluate ));
  "SVD_test6" >::(fun _ -> assert_equal
                     (MatrixSVD ([[22.; 22.]; 
                                  [15.2249721603218262; -7.22497216032182443]],
                                 [[16.2249721603218262; 0.]; 
                                  [0.; -6.22497216032182443]],
                                 [[0.0146284472147750182; 0.0445435403187374];
                                  [0.0308260982397704393; -0.0445435403187374]]))
                     ("SVD [[1 22][5 9]]" |> parse|>evaluate));
  "SVD_test6" >::(fun _ -> assert_equal
                     (MatrixSVD ([[22.; 22.]; 
                                  [15.2249721603218262; -7.22497216032182443]],
                                 [[16.2249721603218262; 0.]; 
                                  [0.; -6.22497216032182443]],
                                 [[0.0146284472147750182; 0.0445435403187374];
                                  [0.0308260982397704393; -0.0445435403187374]]))
                     ("SVD [[1 22][     5 9]]" |> parse|>evaluate));
  "SVD_test6" >::(fun _ -> assert_equal
                     (MatrixSVD ([[22.; 22.]; 
                                  [15.2249721603218262; -7.22497216032182443]],
                                 [[16.2249721603218262; 0.]; 
                                  [0.; -6.22497216032182443]],
                                 [[0.0146284472147750182; 0.0445435403187374];
                                  [0.0308260982397704393; -0.0445435403187374]]))
                     ("SVD [[1 22       ][5 9]]" |> parse|>evaluate));
  "SVD_test6" >::(fun _ -> assert_equal
                     (MatrixSVD ([[22.; 22.]; 
                                  [15.2249721603218262; -7.22497216032182443]],
                                 [[16.2249721603218262; 0.]; [0.; -6.22497216032182443]],
                                 [[0.0146284472147750182; 0.0445435403187374];
                                  [0.0308260982397704393; -0.0445435403187374]]))
                     ("SVD [[1 22][5 9]      ]" |> parse|>evaluate));                   
  "SVD_test6" >::(fun _ -> assert_equal
                     (MatrixSVD ([[22.; 22.]; 
                                  [15.2249721603218262; -7.22497216032182443]],
                                 [[16.2249721603218262; 0.]; 
                                  [0.; -6.22497216032182443]],
                                 [[0.0146284472147750182; 0.0445435403187374];
                                  [0.0308260982397704393; -0.0445435403187374]]))
                     ("SVD [          [1 22][5 9]]" |> parse|>evaluate));

]

let plus_simp_1 = Node (BOperation Plus, Val (Float 1.), Val (Vari "x")) 
let plus_simp_2 = Node (BOperation Plus, Val (Float 100.), Val (Vari "x")) 
let plus_simp_3 = Node (BOperation Plus, Val (Float 10.3), Val (Vari "x")) 
let plus_simp_4 = Node (BOperation Plus, Val (Const Pi), Val (Vari "x")) 
let plus_simp_5 = Node (BOperation Plus, Val (Const E), Val (Vari "x")) 
let plus_simp_6 = Node (BOperation Plus, Val (Vari "x"), Val (Vari "x")) 
let plus_simp_7 = Node (BOperation Plus, Val (Vari "a"), Val (Vari "x")) 

let minus_simp_1 = Node (BOperation Minus, Val (Float 1.), Val (Vari "x")) 
let minus_simp_2 = Node (BOperation Minus, Val (Float 100.), Val (Vari "x")) 
let minus_simp_3 = Node (BOperation Minus, Val (Float 10.3), Val (Vari "x")) 
let minus_simp_4 = Node (BOperation Minus, Val (Const Pi), Val (Vari "x")) 
let minus_simp_5 = Node (BOperation Minus, Val (Const E), Val (Vari "x")) 
let minus_simp_6 = Node (BOperation Minus, Val (Vari "x"), Val (Vari "x")) 
let minus_simp_7 = Node (BOperation Minus, Val (Vari "a"), Val (Vari "x")) 
let rst = Node (BOperation Plus, Val (Float 0.), Val (Float 1.))
let rst2 = Node (BOperation Plus, Val (Float 1.), Val (Float 1.))
let rst3 = Node (BOperation Minus, Val (Float 0.), Val (Float 1.))
let rst4 = Node (BOperation Minus, Val (Float 1.), Val (Float 1.))

let  times_simp_1 = Node (BOperation Times, Val (Vari "a"), Val (Vari "x"))
let times_simp_2 = Node (BOperation Times, Val (Float 0.), Val (Vari "x"))
let times_simp_3 = Node (BOperation Times, Val (Float 100.), Val (Vari "x"))
let times_simp_4 = Node (BOperation Times, Val (Float (-1.)), Val (Vari "x"))
let times_simp_5 = Node (BOperation Times, Val (Const E), Val (Vari "x"))
let times_simp_6 = Node (BOperation Times, Val (Const Pi), Val (Vari "x"))
let times_simp_7 = Node (BOperation Times, Val (Vari"xx"), Val (Vari "x"))
let times_simp_8 = Node (BOperation Times, Val (Vari "x"), Val (Vari "x"))

let times_simp_1_rst = Node (
    BOperation Plus, 
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Vari "a"), Val (Float 1.)))
let times_simp_2_rst =   Node (
    BOperation Plus,
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Float 0.), Val (Float 1.)))
let times_simp_3_rst =   Node (
    BOperation Plus,
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Float 100.), Val (Float 1.)))
let times_simp_4_rst =   Node (
    BOperation Plus,
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Float (-1.)), Val (Float 1.)))

let times_simp_5_rst = Node (
    BOperation Plus, 
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Const E), Val (Float 1.)))

let times_simp_6_rst  =   Node (
    BOperation Plus, 
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Const Pi), Val (Float 1.)))

let times_simp_7_rst  =   Node (
    BOperation Plus, 
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Vari "xx"), Val (Float 1.)))

let times_simp_8_rst  =   Node (
    BOperation Plus, 
    Node (BOperation Times, Val (Float 1.), Val (Vari "x")),
    Node (BOperation Times, Val (Vari "x"), Val (Float 1.)))

let div_sim_1 = Node (BOperation Div, Val (Vari "a"), Val (Vari "x"))
let div_sim_2 = Node (BOperation Div, Val (Float 0.), Val (Vari "x"))
let div_sim_3 = Node (BOperation Div, Val (Float 100.), Val (Vari "x"))
let div_sim_4 = Node (BOperation Div, Val (Float (-1.)), Val (Vari "x"))
let div_sim_5 = Node (BOperation Div, Val (Const E), Val (Vari "x"))
let div_sim_6 = Node (BOperation Div, Val (Const Pi), Val (Vari "x"))
let div_sim_7 = Node (BOperation Div, Val (Vari"xx"), Val (Vari "x"))
let div_sim_8 = Node (BOperation Div, Val (Vari "x"), Val (Vari "x"))

let div_sim_1_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Vari "a"), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_sim_2_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Float 0.), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_sim_3_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Float 100.), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_sim_4_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Float (-1.)), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_sim_5_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Const E), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_sim_6_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Const Pi), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))

let div_sim_7_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.)),
          Node (BOperation Times, Val (Vari "xx"), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))

let div_sim_8_rst = Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))
let div_rev_sim_1 = Node (BOperation Div, Val (Vari "x"),Val (Vari "a"))
let div_rev_sim_2 = Node (BOperation Div, Val (Vari "x"),Val (Float 0.))
let div_rev_sim_3 = Node (BOperation Div, Val (Vari "x"),Val (Float 100.))
let div_rev_sim_4 = Node (BOperation Div, Val (Vari "x"),Val (Float (-1.)))
let div_rev_sim_5 = Node (BOperation Div, Val (Vari "x"),Val (Const E))
let div_rev_sim_6 = Node (BOperation Div, Val (Vari "x"),Val (Const Pi))
let div_rev_sim_7 = Node (BOperation Div, Val (Vari "x"),Val (Vari"xx") )
let div_rev_sim_8 = Node (BOperation Div, Val (Vari "x"),Val (Vari "x") )

let div_rev_sim_1_rst = Node 
    (BOperation Div,
     Node (BOperation Minus,
           Node (BOperation Times, Val (Vari "a"), Val (Float 1.)),
           Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
     Node (BOperation Power, Val (Vari "a"), Val (Float 2.)))
let div_rev_sim_2_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Float 0.), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
    Node (BOperation Power, Val (Float 0.), Val (Float 2.)))
let div_rev_sim_3_rst =
  Node (BOperation Div,
        Node (BOperation Minus,
              Node (BOperation Times, Val (Float 100.), Val (Float 1.)),
              Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
        Node (BOperation Power, Val (Float 100.), Val (Float 2.)))
let div_rev_sim_4_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Float (-1.)), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
    Node (BOperation Power, Val (Float (-1.)), Val (Float 2.)))
let div_rev_sim_5_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Const E), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
    Node (BOperation Power, Val (Const E), Val (Float 2.)))
let div_rev_sim_6_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Const Pi), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
    Node (BOperation Power, Val (Const Pi), Val (Float 2.)))
let div_rev_sim_7_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "xx"), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 0.))),
    Node (BOperation Power, Val (Vari "xx"), Val (Float 2.)))
let div_rev_sim_8_rst =Node (
    BOperation Div,
    Node (BOperation Minus,
          Node (BOperation Times, Val (Vari "x"), Val (Float 1.)),
          Node (BOperation Times, Val (Vari "x"), Val (Float 1.))),
    Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))

let power_sim_1 = Node (BOperation Power, Val (Vari "a"), Val (Vari "x"))
let power_sim_2 = Node (BOperation  Power, Val (Float 0.), Val (Vari "x"))
let  power_sim_3 = Node (BOperation  Power, Val (Float 100.), Val (Vari "x"))
let  power_sim_4 = Node (BOperation  Power, Val (Float (-1.)), Val (Vari "x"))
let  power_sim_5 = Node (BOperation  Power, Val (Const E), Val (Vari "x"))
let  power_sim_6 = Node (BOperation  Power, Val (Const Pi), Val (Vari "x"))
let  power_sim_7 = Node (BOperation  Power, Val (Vari"xx"), Val (Vari "x"))

let power_sim_1_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Vari "a"), Val (Vari "x")),
          Node (UOperation Log, Val Emp, Val (Vari "a"))),
    Val (Float 1.))

let power_sim_2_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Float 0.), Val (Vari "x")),
          Node (UOperation Log,  Val Emp,Val (Float 0.))),
    Val (Float 1.))

let power_sim_3_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Float 100.), Val (Vari "x")),
          Node (UOperation Log,  Val Emp,Val (Float 100.))),
    Val (Float 1.))
let power_sim_4_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Float (-1.)), Val (Vari "x")),
          Node (UOperation Log, Val Emp,Val (Float (-1.)))),
    Val (Float 1.))

let power_sim_5_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Const E), Val (Vari "x")),
          Node (UOperation Log,Val Emp, Val (Const E))),
    Val (Float 1.))

let power_sim_6_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Const Pi), Val (Vari "x")),
          Node (UOperation Log, Val Emp,Val (Const Pi))),
    Val (Float 1.))
let power_sim_7_rst = Node (
    BOperation Times,
    Node (BOperation Times,
          Node (BOperation Power, Val (Vari "xx"), Val (Vari "x")),
          Node (UOperation Log, Val Emp,Val (Vari "xx"))),
    Val (Float 1.))

let power_rev_sim_1 = Node (BOperation  Power, Val (Vari "x"),Val (Vari "a"))
let  power_rev_sim_2 = Node (BOperation  Power, Val (Vari "x"),Val (Float 0.))
let  power_rev_sim_3 = Node (BOperation  Power, Val (Vari "x"),Val (Float 100.))
let  power_rev_sim_4 = Node (BOperation  Power, Val (Vari "x"),Val (Float (-1.)))
let  power_rev_sim_5 = Node (BOperation  Power, Val (Vari "x"),Val (Const E))
let  power_rev_sim_6 = Node (BOperation  Power, Val (Vari "x"),Val (Const Pi))
let  power_rev_sim_7 = Node (BOperation  Power, Val (Vari "x"),Val (Vari"xx") )

let  power_rev_sim_1_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Vari "a"),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Vari "a"), Val (Float 1.)))))
let  power_rev_sim_2_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Float 0.),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Float 0.), Val (Float 1.)))))
let  power_rev_sim_3_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Float 100.),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Float 100.), Val (Float 1.)))))
let  power_rev_sim_4_rst =  Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Float (-1.)),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Float (-1.)), Val (Float 1.)))))
let  power_rev_sim_5_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Const E),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Const E), Val (Float 1.)))))
let  power_rev_sim_6_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Const Pi),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Const Pi), Val (Float 1.)))))
let  power_rev_sim_7_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Vari "xx"),
          Node (BOperation Power, Val (Vari "x"),
                Node (BOperation Minus, Val (Vari "xx"), Val (Float 1.)))))

let log_sim = Node (UOperation Log, Val Emp, Val (Vari "x"))
let sin_sim = Node (UOperation Sin, Val Emp, Val (Vari "x"))
let cos_sim = Node (UOperation Cos, Val Emp, Val (Vari "x"))
let tan_sim = Node (UOperation Tan, Val Emp, Val (Vari "x"))

let log_sim_rst = Node (
    BOperation Times, 
    Node (BOperation Div, Val (Float 1.), Val (Vari "x")),Val (Float 1.))
let sin_sim_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (UOperation Cos, Val Emp, Val (Vari "x")))
let cos_sim_rst = Node (
    BOperation Times, Val (Float 1.),
    Node (BOperation Times, Val (Float (-1.)),
          Node (UOperation Sin, Val Emp, Val (Vari "x"))))
let tan_sim_rst = Node (
    BOperation Div,
    Node (
      BOperation Minus,
      Node (
        BOperation Times, Node (UOperation Cos, Val Emp, Val (Vari "x")),
        Node (BOperation Times, Val (Float 1.),
              Node (UOperation Cos, Val Emp, Val (Vari "x")))),
      Node (
        BOperation Times, Node (UOperation Sin, Val Emp, Val (Vari "x")),
        Node (BOperation Times, Val (Float 1.),
              Node (BOperation Times, Val (Float (-1.)),
                    Node (UOperation Sin, Val Emp, Val (Vari "x")))))),
    Node (BOperation Power, Node (UOperation Cos, Val Emp, Val (Vari "x")),
          Val (Float 2.)))
let derivative_tests = [

  (** Node test: basic Binary operation matches*)
  (* tests for pure plus*)
  "basic_bop_test1" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_1 (Vari "x")));
  "basic_bop_test2" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_2 (Vari "x")));
  "basic_bop_test3" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_3 (Vari "x")));
  "basic_bop_test4" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_4 (Vari "x")));
  "basic_bop_test5" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_5 (Vari "x")));
  "basic_bop_test6" >:: (fun _ -> assert_equal rst2
                            (derivative plus_simp_6 (Vari "x")));
  "basic_bop_test7" >:: (fun _ -> assert_equal rst
                            (derivative plus_simp_7 (Vari "x")));
  (* tests for pure minus*)
  "basic_bop_test8" >:: (fun _ -> assert_equal rst3
                            (derivative minus_simp_1 (Vari "x")));
  "basic_bop_test9" >:: (fun _ -> assert_equal rst3
                            (derivative minus_simp_2 (Vari "x")));
  "basic_bop_test10" >:: (fun _ -> assert_equal rst3
                             (derivative minus_simp_3 (Vari "x")));
  "basic_bop_test11" >:: (fun _ -> assert_equal rst3
                             (derivative minus_simp_4 (Vari "x")));
  "basic_bop_test12" >:: (fun _ -> assert_equal rst3
                             (derivative minus_simp_5 (Vari "x")));
  "basic_bop_test13" >:: (fun _ -> assert_equal rst4
                             (derivative minus_simp_6 (Vari "x")));
  "basic_bop_test14" >:: (fun _ -> assert_equal rst3
                             (derivative minus_simp_7 (Vari "x")));
  (* tests for pure times*)
  "basic_bop_test15" >:: (fun _ -> assert_equal times_simp_1_rst
                             (derivative times_simp_1 (Vari "x")));
  "basic_bop_test16" >:: (fun _ -> assert_equal times_simp_2_rst
                             (derivative times_simp_2 (Vari "x")));
  "basic_bop_test17" >:: (fun _ -> assert_equal times_simp_3_rst
                             (derivative times_simp_3 (Vari "x")));
  "basic_bop_test18" >:: (fun _ -> assert_equal times_simp_4_rst
                             (derivative times_simp_4 (Vari "x")));
  "basic_bop_test19" >:: (fun _ -> assert_equal times_simp_5_rst
                             (derivative times_simp_5 (Vari "x")));
  "basic_bop_test20" >:: (fun _ -> assert_equal times_simp_6_rst
                             (derivative times_simp_6 (Vari "x")));
  "basic_bop_test21" >:: (fun _ -> assert_equal times_simp_7_rst
                             (derivative times_simp_7 (Vari "x")));
  "basic_bop_test22" >:: (fun _ -> assert_equal times_simp_8_rst
                             (derivative times_simp_8 (Vari "x")));
  (* tests for pure div x as denominator *)
  "basic_bop_test23" >:: (fun _ -> assert_equal div_sim_1_rst
                             (derivative div_sim_1 (Vari "x")));
  "basic_bop_test24" >:: (fun _ -> assert_equal div_sim_2_rst
                             (derivative   div_sim_2 (Vari "x")));
  "basic_bop_test25" >:: (fun _ -> assert_equal   div_sim_3_rst
                             (derivative   div_sim_3 (Vari "x")));
  "basic_bop_test26" >:: (fun _ -> assert_equal   div_sim_4_rst
                             (derivative   div_sim_4 (Vari "x")));
  "basic_bop_test27" >:: (fun _ -> assert_equal   div_sim_5_rst
                             (derivative   div_sim_5 (Vari "x")));
  "basic_bop_test28" >:: (fun _ -> assert_equal   div_sim_6_rst
                             (derivative   div_sim_6 (Vari "x")));
  "basic_bop_test29" >:: (fun _ -> assert_equal   div_sim_7_rst
                             (derivative   div_sim_7 (Vari "x")));
  "basic_bop_test30" >:: (fun _ -> assert_equal   div_sim_8_rst
                             (derivative   div_sim_8 (Vari "x")));
  (* tests for pure div x as numerator *)
  "basic_bop_test31" >:: (fun _ -> assert_equal  div_rev_sim_1_rst
                             (derivative  div_rev_sim_1 (Vari "x")));
  "basic_bop_test32" >:: (fun _ -> assert_equal  div_rev_sim_2_rst
                             (derivative    div_rev_sim_2 (Vari "x")));
  "basic_bop_test33" >:: (fun _ -> assert_equal    div_rev_sim_3_rst
                             (derivative    div_rev_sim_3 (Vari "x")));
  "basic_bop_test34" >:: (fun _ -> assert_equal    div_rev_sim_4_rst
                             (derivative    div_rev_sim_4 (Vari "x")));
  "basic_bop_test35" >:: (fun _ -> assert_equal    div_rev_sim_5_rst
                             (derivative    div_rev_sim_5 (Vari "x")));
  "basic_bop_test36" >:: (fun _ -> assert_equal    div_rev_sim_6_rst
                             (derivative    div_rev_sim_6 (Vari "x")));
  "basic_bop_test37" >:: (fun _ -> assert_equal    div_rev_sim_7_rst
                             (derivative    div_rev_sim_7 (Vari "x")));
  "basic_bop_test38" >:: (fun _ -> assert_equal    div_rev_sim_8_rst
                             (derivative    div_rev_sim_8 (Vari "x")));
  (*test for pure power *)
  "basic_bop_test39" >:: (fun _ -> assert_equal   power_sim_1_rst
                             (derivative   power_sim_1 (Vari "x")));
  "basic_bop_test40" >:: (fun _ -> assert_equal   power_sim_2_rst
                             (derivative     power_sim_2 (Vari "x")));
  "basic_bop_test41" >:: (fun _ -> assert_equal     power_sim_3_rst
                             (derivative     power_sim_3 (Vari "x")));
  "basic_bop_test42" >:: (fun _ -> assert_equal     power_sim_4_rst
                             (derivative     power_sim_4 (Vari "x")));
  "basic_bop_test43" >:: (fun _ -> assert_equal     power_sim_5_rst
                             (derivative     power_sim_5 (Vari "x")));
  "basic_bop_test44" >:: (fun _ -> assert_equal     power_sim_6_rst
                             (derivative     power_sim_6 (Vari "x")));
  "basic_bop_test45" >:: (fun _ -> assert_equal     power_sim_7_rst
                             (derivative     power_sim_7 (Vari "x")));
  (*test for pure power with x as numerator *)
  "basic_bop_test46" >:: (fun _ -> assert_equal    power_rev_sim_1_rst
                             (derivative    power_rev_sim_1 (Vari "x")));
  "basic_bop_test47" >:: (fun _ -> assert_equal    power_rev_sim_2_rst
                             (derivative      power_rev_sim_2 (Vari "x")));
  "basic_bop_test48" >:: (fun _ -> assert_equal      power_rev_sim_3_rst
                             (derivative      power_rev_sim_3 (Vari "x")));
  "basic_bop_test49" >:: (fun _ -> assert_equal      power_rev_sim_4_rst
                             (derivative      power_rev_sim_4 (Vari "x")));
  "basic_bop_test50" >:: (fun _ -> assert_equal      power_rev_sim_5_rst
                             (derivative      power_rev_sim_5 (Vari "x")));
  "basic_bop_test51" >:: (fun _ -> assert_equal      power_rev_sim_6_rst
                             (derivative      power_rev_sim_6 (Vari "x")));
  "basic_bop_test52" >:: (fun _ -> assert_equal      power_rev_sim_7_rst
                             (derivative      power_rev_sim_7 (Vari "x")));
  (*Val tests: Consts, Float, and Vari*)
  "val_test1" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Float 1.)) (Vari "x")));
  "val_test2" >:: (fun _ -> assert_equal (Val(Float 0.))
                      (derivative (Val(Const E)) (Vari "x")));
  "val_test3" >:: (fun _ -> assert_equal (Val (Float 0.))
                      (derivative (Val(Vari "y")) (Vari "x")));
  "val_test4" >:: (fun _ -> assert_equal (Val(Float 0.))
                      (derivative (Val(Const Pi)) (Vari "x")));
  "val_test5" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Float 0.)) (Vari "x")));
  "val_test6" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Float 2.5)) (Vari "x"))); 
  "val_test7" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Float 100.3)) (Vari "x")));
  "val_test8" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Vari "a")) (Vari "x")));
  "val_test9" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                      (derivative (Val(Vari "mcq")) (Vari "x")));
  "val_test10" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                       (derivative (Val(Vari "xx")) (Vari "x")));
  "val_test11" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                       (derivative (Val(Vari "xxx")) (Vari "x")));
  "val_test12" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                       (derivative (Val(Vari "acqz")) (Vari "x")));
  "val_test13" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                       (derivative (Val(Float 2.9)) (Vari "x")));
  "val_test14" >:: (fun _ -> assert_equal (Val(Float 0.)) 
                       (derivative (Val(Float 10.)) (Vari "x")));
  "val_test15" >:: (fun _ -> assert_equal (Val (Float 1.))
                       (derivative (Val(Vari "x")) (Vari "x")));
  (*tests for basic unary operations *)
  "basic_uop_test1" >:: (fun _ -> assert_equal     log_sim_rst
                            (derivative      log_sim (Vari "x")));
  "basic_uop_test2" >:: (fun _ -> assert_equal     sin_sim_rst
                            (derivative      sin_sim (Vari "x")));
  "basic_uop_test3" >:: (fun _ -> assert_equal     cos_sim_rst
                            (derivative      cos_sim (Vari "x")));
  "basic_uop_test4" >:: (fun _ -> assert_equal     tan_sim_rst
                            (derivative      tan_sim (Vari "x")));
]

(** set up trees to for integral tests *)
let times_cov t1 t2 = 
  Node (BOperation Times, t1, t2)

let div_cov t1 t2 = 
  Node (BOperation Div, t1, t2)

let plus_cov t1 t2 = 
  Node (BOperation Plus, t1, t2)

let minus_cov t1 t2 = 
  Node (BOperation Minus, t1, t2)

let pow_cov t1 t2 = 
  Node (BOperation Power, t1, t2)

let sin_cov t1 = 
  Node (UOperation Sin, Val(Emp), t1)

let cos_cov t1 = 
  Node (UOperation Cos, Val(Emp), t1)

let log_cov t1 = 
  Node (UOperation Log, Val(Emp), t1)

let var_x = Val (Vari "x") 
let sin_x = sin_cov var_x
let cos_x = cos_cov var_x
let log_x = log_cov var_x

let integral_tests = [

  (* sin_integral tests *)
  "sin_integral_test1" >:: (fun _ -> assert_equal 
  (times_cov (Val (Float (-1.))) cos_x) 
  (integrate sin_x));

  "sin_integral_test2" >:: (fun _ -> assert_equal 
  (Node (BOperation Div,
  Node (UOperation Cos, Val Emp,
  Node (BOperation Times, Val (Float 2.), Val (Vari "x"))),
  Node (BOperation Times, Val (Float (-1.)),
  Node (BOperation Plus,
      Node (BOperation Times, Val (Float 0.), Val (Vari "x")), Val (Float 2.)))))
  ((integrate (sin_cov (times_cov (Val (Float 2.)) var_x)) |> simplify)));

  "sin_integral_test3" >:: (fun _ -> assert_equal 
  (Node (BOperation Times, Val (Float (sin 1.)), var_x)) 
  (integrate (Node (UOperation Sin, Val (Emp), Val (Float 1.)))));

  (* cos integral tests *) 
  "cos_integral_test1" >:: (fun _ -> assert_equal (sin_x) (integrate cos_x));

  "cos_integral_test2" >:: (fun _ -> assert_equal 
  (Node (BOperation Times, 
        Val (Float 2.),
        Node (BOperation Times, 
              Val (Float 0.5),
              Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))))
  (integrate (times_cov (Val (Float 2.)) var_x)));

  "cos_integral_test3" >:: (fun _ -> assert_equal 
  (Node (BOperation Times, Val (Float (cos 1.)), var_x)) 
  (integrate (cos_cov (Val (Float 1.)))));

  "cos_integral_test4" >:: (fun _ -> assert_equal 
  (Node (BOperation Div,
  Node (BOperation Times,
    Node (UOperation Sin, Val Emp,
    Node (BOperation Times, Val (Float 0.5), Val (Vari "x"))),
    Val (Float (-1.))),
  Node (BOperation Plus,
    Node (BOperation Times, Val (Float 0.), Val (Vari "x")),
    Node (BOperation Times, Val (Float 0.5), Val (Float 1.)))))
  (integrate (cos_cov (times_cov (Val (Float 0.5)) var_x))));

   "poly_tests_1" >:: (fun _ -> assert_equal 
  (Node (BOperation Plus,
    Node (BOperation Div, Node (BOperation Power, Val (Vari "x"), Val (Float 3.)),
      Node (BOperation Plus, Val (Float 2.), Val (Float 1.))),
    Node (BOperation Times, Val (Float 0.5),
      Node (BOperation Power, Val (Vari "x"), Val (Float 2.)))))
      (integrate (Node (BOperation Plus,
      Node (BOperation Power, Val (Vari "x"), Val (Float 2.)), Val (Vari "x")))));
  
  "poly_tests_2" >:: (fun _ -> assert_equal 
    (Node (BOperation Plus,
      Node (BOperation Plus,
        Node (BOperation Times, Val (Float 0.5),
        Node (BOperation Power, Val (Vari "x"), Val (Float 2.))),
        Node (BOperation Div,
        Node (BOperation Power, Val (Vari "x"), Val (Float 3.)),
        Node (BOperation Plus, Val (Float 2.), Val (Float 1.)))),
      Node (BOperation Times, Val (Float (-1.)),
        Node (UOperation Cos, Val Emp, Val (Vari "x")))))
  (integrate (plus_cov (plus_cov var_x (pow_cov var_x (Val (Float 2.)))) 
  (sin_cov var_x))));
  
  "poly_tests_3" >:: (fun _ -> assert_equal 
  (Node (BOperation Times, 
         Val (Float 1.), Node (UOperation Log, Val Emp, Val (Vari "x"))))
  (integrate ( Node (BOperation Div, Val (Float 1.), Val (Vari "x")))));
  
   "poly_tests_4" >:: (fun _ -> assert_equal 
  (Node (BOperation Div,
    Node (BOperation Power, Val (Vari "x"), Val (Float (-2.))),
    Node (BOperation Plus, Val (Float (-3.)), Val (Float 1.))))
  (integrate ( Node (BOperation Power, Val (Vari "x"), Val (Float (-3.))))));
  
  "poly_tests_5" >:: (fun _ -> assert_equal 
    (Node (BOperation Times, Val (Float 3.),
  Node (BOperation Div, Node (BOperation Power, Val (Vari "x"), Val (Float 4.)),
    Node (BOperation Plus, Val (Float 3.), Val (Float 1.)))))
  (integrate (times_cov (pow_cov var_x (Val (Float 3.))) (Val (Float 3.)))));

  "poly_tests_6" >:: (fun _ -> assert_equal 
    (Node (BOperation Plus,
          Node (BOperation Times, Val (Float 3.),
            Node (BOperation Div,
            Node (BOperation Power, Val (Vari "x"), Val (Float 4.)),
            Node (BOperation Plus, Val (Float 3.), Val (Float 1.)))),
          Node (BOperation Times, Val (Float 2.),
            Node (UOperation Log, Val Emp, Val (Vari "x")))))
  (integrate (Node (BOperation Plus,
   Node (BOperation Times,
    Node (BOperation Power, Val (Vari "x"), Val (Float 3.)), Val (Float 3.)),
   Node (BOperation Div, Val (Float 2.), Val (Vari "x"))))));
  
   "poly_tests_7" >:: (fun _ -> assert_equal 
    (Node (BOperation Minus,
    Node (BOperation Plus,
    Node (BOperation Times, Val (Float 3.),
    Node (BOperation Div,
      Node (BOperation Power, Val (Vari "x"), Val (Float 4.)),
      Node (BOperation Plus, Val (Float 3.), Val (Float 1.)))),
    Node (BOperation Times, Val (Float 2.),
    Node (UOperation Log, Val Emp, Val (Vari "x")))),
    Node (BOperation Div,
    Node (UOperation Cos, Val Emp,
    Node (BOperation Minus, Val (Float 0.), Val (Vari "x"))),
    Node (BOperation Times, Val (Float (-1.)),
    Node (BOperation Minus, Val (Float 0.), Val (Float 1.))))))
   (integrate (Node (BOperation Minus,
    Node (BOperation Plus,
      Node (BOperation Times,
      Node (BOperation Power, Val (Vari "x"), Val (Float 3.)), Val (Float 3.)),
      Node (BOperation Div, Val (Float 2.), Val (Vari "x"))),
    Node (UOperation Sin, Val Emp,
      Node (BOperation Minus, Val (Float 0.), Val (Vari "x")))))));

]

(** set up calcultors for later tests *)
let input_b1 = 
  [
    {name = "1"; highlight = false};
    {name = "2"; highlight = false};
    {name = "3"; highlight = false};
    {name = "<——"; highlight = false};
  ]

let input_b2 = 
  [
    {name = "q"; highlight = false};
    {name = "h"; highlight = false};
    {name = "3"; highlight = false};
    {name = "<——"; highlight = false};
  ]

let exp_b1 = 
  [
    {name = "1"; highlight = true};
    {name = "2"; highlight = false};
    {name = "3"; highlight = false};
    {name = "<——"; highlight = false};
  ]

let exp_b2 = 
  [
    {name = "1"; highlight = false};
    {name = "2"; highlight = true};
    {name = "3"; highlight = false};
    {name = "<——"; highlight = false};
  ]

let exp_b3 = 
  [
    {name = "1"; highlight = false};
    {name = "2"; highlight = false};
    {name = "3"; highlight = true};
    {name = "<——"; highlight = false};
  ]

let exp_b4 = 
  [
    {name = "1"; highlight = false};
    {name = "2"; highlight = false};
    {name = "3"; highlight = false};
    {name = "<——"; highlight = true};
  ]

let exp_b5 = 
  [
    {name = "1"; highlight = false};
    {name = "2"; highlight = false};
    {name = "3"; highlight = true};
    {name = "<——"; highlight = false};
  ]


let input_calc1 = 
  {
    input = "";
    output = "";
    buttons = input_b1
  }

let exp_calc1 = 
  {
    input_calc1 with buttons = exp_b1
  }

let exp_calc2 = 
  {
    exp_calc1 with buttons = exp_b2
  }

let exp_calc3 = 
  {
    exp_calc1 with buttons = exp_b3
  }

let input_calc2 = {
  input = "312";
  output = "";
  buttons = exp_b1
}

let exp_calc4 = 
  {
    input = "31";
    output = "";
    buttons = exp_b4
  }

let exp_calc5 = 
  {
    input = "3";
    output = "";
    buttons = exp_b4
  }

let exp_calc6 = 
  {
    input = "";
    output = "";
    buttons = exp_b4
  }

let exp_calc7 = 
  {
    input = "3";
    output = "";
    buttons = exp_b5
  }

let exp_calc8 = 
  {
    input = "31";
    output = "";
    buttons = exp_b1
  }

let exp_calc9 = 
  {
    input = "312";
    output = "";
    buttons = exp_b2
  }

let exp_calc10 = 
  {
    input = "312a";
    output = "";
    buttons = input_b1
  }

let gui_tests = [
  (* del_one tests *)
  "del_test1" >:: (fun _ -> assert_equal exp_calc4 (del_one input_calc2));
  "del_test2" >:: (fun _ -> assert_equal exp_calc5 (del_one exp_calc4));
  "del_test3" >:: (fun _ -> assert_equal exp_calc6 (del_one exp_calc5));

  (* insert_char tests *)
  "insert_char_test1" >:: 
  (fun _ -> assert_equal exp_calc7 (insert_char '3' exp_calc6));
  "insert_char_test2" >:: 
  (fun _ -> assert_equal exp_calc8 (insert_char '1' exp_calc7));
  "insert_char_test3" >:: 
  (fun _ -> assert_equal exp_calc9 (insert_char '2' exp_calc8));
  "insert_char_test4" >:: 
  (fun _ -> assert_equal exp_calc10 (insert_char 'a' exp_calc9));

  (* shade tests *)
  "shade_test1" >:: (fun _ -> assert_equal exp_calc1 (shade "1" input_calc1));
  "shade_test2" >:: (fun _ -> assert_equal exp_calc2 (shade "2" exp_calc1));
  "shade_test3" >:: (fun _ -> assert_equal exp_calc3 (shade "3" exp_calc2))
]

let suite =
  "test suite for Midterm Project"  >::: List.flatten [
    ast_tests;
    basic_op_tests;
    matrix_tests;
    derivative_tests;
    gui_tests;
    integral_tests;
  ]

let _ = run_test_tt_main suite
