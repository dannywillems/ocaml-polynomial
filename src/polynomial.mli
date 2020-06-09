type natural_with_infinity = Natural of int | Infinity

(** General module signature for a ring *)
module type RING_SIG = sig
  type t

  val order : Z.t

  val zero : unit -> t

  val one : unit -> t

  val is_zero : t -> bool

  val is_one : t -> bool

  val random : unit -> t

  val add : t -> t -> t

  val mul : t -> t -> t

  val eq : t -> t -> bool

  val negate : t -> t

  (* Unsafe version of inverse *)
  val inverse : t -> t

  (* Safe version of inverse *)
  val inverse_opt : t -> t option

  val square : t -> t

  val double : t -> t

  val pow : t -> Z.t -> t

  val to_string : t -> string
end

(***)
module type T = sig
  type scalar

  type polynomial

  (** Returns the degree of the polynomial *)
  val degree : polynomial -> natural_with_infinity

  val evaluation : polynomial -> scalar -> scalar

  val zero : unit -> polynomial

  val constants : scalar -> polynomial

  val add : polynomial -> polynomial -> polynomial

  val mult_by_scalar : scalar -> polynomial -> polynomial

  val is_null : polynomial -> bool

  val is_constant : polynomial -> bool

  val opposite : polynomial -> polynomial

  val equal : polynomial -> polynomial -> bool

  val of_coefficients : (scalar * int) list -> polynomial

  val lagrange_interpolation : (scalar * scalar) list -> polynomial

  val to_string : polynomial -> string
end

module Make : functor (R : RING_SIG) -> T with type scalar = R.t
