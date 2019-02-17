(* [plus v1 v2] is the float[v] after performing plus operation on [v1] and [v2]
    Raises:
      [Invalid calculation] if [plus] cannot be performed*)
val plus: float -> float -> float

(* [minus v1 v2] is the float [v] after deducting [v2] from [v1]*)
val minus: float -> float -> float

(* [times v1 v2] is the float [v] after multiplying [v2] to [v1]*)
val times: float -> float -> float

(* [divide v1 v2] is the float [v] after dividing [v2] from [v1]*)
val divide: float -> float -> float

(* [negative v1 e] is the float [v] after negating [v1]*)
val negative: float -> float

(* [power v1 v2] is the float [v] after doing [v2]th power on [v1]. 
   Requires: 
   [v2] should be an integer represent in float type. e.g. 2 as 2.0. 
*)
val power: float -> float -> float

(* [combination v1 v2] is the number of unordered ways to choose a 
   set of v2 numbers of elements from a set with v1 number of elements
 *  Requires: [v1] and [v2] should be positive integers represent 
   in float type. e.g. 2 as 2.0. [v1] is less than or equal to [v2]*)
val combination: float -> float -> float

(* [permutation v1 v2] is the number of ordered ways to choose a 
   set of v2 numbers of elements from a set with v1 number of elements
 *  Requires: [v1] and [v2] should be positive integers represent 
   in float type. e.g. 2 as 2.0. [v1] is less than or equal to [v2]*)
val permutation: float -> float -> float

(* [log v1] is the natural logarithm result of [v1]. *)
val log: float -> float

(* [sin v1] is the sine result of radian [v1].*)
val sin: float -> float

(* [cos v1] is the cosine result of radian [v1]. *)
val cos: float -> float

(* [tan v1] is the tangent result of radian [v1]. *)
val tan: float -> float

(* [sin v1] is the sine result of radian [v1]. 
   Requires: [v1] is between -1.0 and 1.0 *)
val asin: float -> float

(* [cos v1] is the cosine result of radian [v1]. 
   Requires: [v1] is between -1.0 and 1.0 *)
val acos: float -> float

(* [tan v1] is the tangent result of radian [v1]. *)
val atan: float -> float

(* [exp v1] is the exponentiation result of [v1]. *)
val exp: float -> float

(**[remainder a b] is the remainder of a by dividing b
   Requires: [a] and [b] are all integers *)
val remainder: float -> float -> float
