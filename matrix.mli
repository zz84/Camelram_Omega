(**[mplus m1 m2] is a new matrix whose elements are the sum of 
   every elements of m1 with corresponding elements of m2 
   Requires: 
    m1 and m2 are the same dimension,
    otheriwse raises ["dimension does not agree"] failure

   Example: 
    let [matrix] be [[[1.9;2.1;3.5]]]
    [plus matrix matrix] is [[[3.8;4.2;7.0]]]*)
val mplus: float list list -> float list list -> float list list

(** [mmius x y] is the result of deducting matrix [y] from [x]
    Raises:
      ["dimension does not agree"] failure if the number of rows of [x] and [y]
      is different, or the number of columns of [x] and [y] is different
      
    Example:
      [mminus [[1.;2.];[3.;4.]] [[0.;1.];[0.;1.]]] is [[[1.;1.];[3.;3.]]]*)
val mminus: float list list -> float list list -> float list list

(** [mtimes_mm x y] is the multipication of two matrices [x] and [y]
    Raises:
      ["dimensions does not match"] failure if the number of columns in [x] is
      different from the number of rows in [y]
      
    Example:
      [mtimes_mm [[1.;0.];[0.;1.]] [[2.;5.];[1.;9.]]] is [[[2.;5.];[1.;9.]]]*)
val mtimes_mm: float list list -> float list list -> float list list

(** [mtimes_mv x a] is the matrix [x] times the multiplier [a]

    Example:
      [mtimes_mv [[1.;7.];[2.;3.]] 2.] is [[2.;14.];[4.;6.]]*)
val mtimes_mv: float list list -> float -> float list list

(** [mtimes_vm a x] is the matrix [x] times the multiplier [a]

    Example:
      [mtimes_vm 2. [[1.;7.];[2.;3.]]] is [[2.;14.];[4.;6.]]*)
val mtimes_vm: float -> float list list -> float list list

(** [mdivide_mv x a] is the matrix [x] divides the coefficient [a]

    Example:
      [mdivide_mv [[1.;7.];[2.;3.]] 2.] is [[0.5;3.5];[1.;1.5]]*)
val mdivide_mv: float list list -> float -> float list list

(** [mpower x a] is the [a]th power to the matrix [x]
    
    Example:
      [mpower [[1.;0.];[0.;1.]] 2] is [[1.;0.];[0.;1.]]*)
val mpower:  float list list -> float ->float list list

(** [transpose x] is the transpose of matrix [x]
    
    Example:
      [transpose [[1.;2.];[3.;4.];[5.;6.]]] is [[[1.;3.;5.];[2.;4.;6.]]]*)
val transpose: float list list -> float list list

(** [adjoint x] is the adjoint matrix of the given matrix [x].
    Given a matrix [A] at position [A[i][j]], which corresponds to [i]th row
    and [j]th column of the matrix [A], it's adjoint matrix [B] has the value
    of cofactor of [A[i][j]] at position [B[i][j]]
    
    Raises:
      ["adjoint can only compute on square matrix"] failure if adjoint matrix
      is computed on a non-square matrix [x]*)
val adjoint: float list list -> float list list

(** [inv x] is the inverse of matrix [x]
    Raises:
      ["not invertible"] if the given matrix [x] is not invertible
      ["adjoint can only compute on square matrix"] failure if the input matrix
      is a non-square matrix [x]*)
val inv: float list list -> float list list

(**[diag x] is a tuple of matrices [(P, L, P^(-1))] where [L] is the diagonal
    matrix that is similar to the input matrix [x]. Output also satisfies
    [x] is equal to [P*L*P^(-1)]
    Requires:
      [x] is a 2*2 matrix
    
   Raises:
    ["only suport 2D"] failure if [x] is not a 2*2 matrix
    ["not diagonalizable"] failure if [x] is not diagonalizable
    ["diagnolization happens on square matrix"] if the input matrix [x] is 
    not a square matrix*)
val diag: float list list -> float list list*float list list*float list list

(** [det x] is the determinant of the input matrix [x]
    Raises:
      ["can only perform determinant on square matrix"] failure if the given 
      matrix [x] is not a square matrix
      
    Example:
      [det [[1.;3.];[2.;4.]]] is [-2.]*)
val det: float list list -> float

(** [eigval x] is the list of eigenvalues of the given matrix [x], and returns
    eigenvalues wth same values if there are eigenvalues with algebraice 
    multiplicity greater than one.
    Requires:
      [x] is a 2*2 matrix
    Raises:
      ["no real solutions"] failure if the eigenvalues of the input matrix [x]
        are complex (conjugate) values
      ["eigenvalues happen on square matrix"] failure if the input matrix [x]
        is not a square matrix*)
val eigval: float list list -> float list list

(** [eigvec x] is the list of eigenvectors of the given matrix [x], and returns
    eigenvectors wth same values if there are eigenvalues with algebraice 
    multiplicity greater than one.
    Requires:
      [x] is a 2*2 matrix
    Raises:
      ["no real solutions"] failure if the eigenvalues of the input matrix [x]
        are complex (conjugate) values
      ["eigenvalues happen on square matrix"] failure if the input matrix [x]
        is not a square matrix*)
val eigvec: float list list -> float list list

(** [tr x] is the trace of input matrix [x]
    Raises:
      ["can only compute trace on square matrix"] failure if the input matrix
        [x] is a non-square matrix
        
    ExampleL:
      [tr [[1.2;3.4];[1.;2.]]] is [3.2]*)
val tr: float list list -> float

(** [svd x] is a tuple that returns the simple value decomposition of the 
    input matrix [x]. Given the tuple [(u,s,vt)], it satisfies the following
    properties:
      1. [u]*[s]*[vt] approximately equals input matrix [x]
      2. [vt] is the transpose of matrix [v], where [v] is consisted of the
        eigenvectors of matrix [x^tx] with [x] is the input matrix
      3. [u] and [v] are othogonal matrices
      4. [s] is diagonal, whose diagonal entries are the singular value of 
        [x^tx] with [x] is the input matrix*)
val svd: float list list -> (float list list)*(float list list)*(float list list)






(* IGNORE BELOW AT THIS POINT*)

(*(*such that P^(-1)AP = J*)
(*maybe in the end I cannot implement it mathematically*)
val jordan: float list list -> float list list

(*some possible algo from 4220*)
(*LU factorization HW2 Q2*)
val lu: float list list -> (float list list)*(float list list)
(*QR decomposition HW3, very likely I can't implement it*)
val qr: float list list -> (float list list)*(float list list) *)
