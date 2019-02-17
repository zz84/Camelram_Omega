open AST 
open BasicOperation


(** [integrated t] is the integrated result of [t] in terms of [Vari "x"]. 

    Requires: 
    [t] doesn't contain any variables other than [Vari "x"]

    Examples: 
    [integrate (Node (BOperation Power, 
                Val (Vari "x"), 
                Val (Float 2.)))] = Node (BOperation Div, 
                                          Node (BOperation Power, 
                                                Val (Vari "x"), 
                                                Val (Float 3.)),
                                          Node (BOperation Plus, 
                                                Val (Float 2.), 
                                                Val (Float 1.)))

    [integrate (Node (UOperation Sin, 
                      Val Emp, 
                      Val (Vari "x")))] = [Node (BOperation Times, 
                                                 Val (Float (-1.)),
                                                 Node (UOperation Cos, 
                                                       Val Emp, 
                                                       Val (Vari "x")))]

*)
val integrate : AST.expr AST.t -> AST.expr AST.t 