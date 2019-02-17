open AST

(** [derivative f x] is the derivative of expression [f] with respect to [x].
    Supports parameters within [f]
    Requires:
      [f] is a valid AST representation
      [x] is Vari of string, of type AST value 
    
    Examples:
      [derivative Node(UOperation Sin, 
        Node(BOperation Times, Val(Float 2.), Val(Vari "x")), Empty)
        (Vari "x")] is [Node(BOperation Times, Val(Float 2.), 
        Node(UOperation Cos, Node(BOperation Times, Val(Float 2.), Val(Vari "x"))
        Empty))] or it's equivalent *)
val derivative: AST.expr AST.t -> AST.value -> AST.expr AST.t

