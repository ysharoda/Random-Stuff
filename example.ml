(* source: https://stackoverflow.com/questions/52161048/applicative-vs-generative-functors
 * retrieved on: Dec 28, 2018 *) 

module App (M : sig end) : sig
  type t
  val zero : t
end = struct
  type t = int
  let zero = 0
end

(* A () argument signifies a generative functor in OCaml. *)
module Gen (M : sig end) () : sig
  type t
  val zero : t
end = struct
  type t = int
  let zero = 0
end

module Empty = struct end

module A = App (Empty)
module B = App (Empty)
module C = App (struct end) (* The argument is syntactically different. *)

module D = Gen (Empty) ()
module E = Gen (Empty) ()

let _ = begin
  (* A.t and B.t are compatible. *)
  ignore (A.zero = B.zero);  (* OK *)

  (* A.t and C.t are not compatible because the functor arguments
   * are not syntactically equal. *)
  ignore (A.zero = C.zero);  (* type error *)

  (* D.t and C.t are not compatible because they are produced
   * from generative functors. *)
  ignore (D.zero = E.zero); (* type error *)
end
