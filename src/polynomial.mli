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

  type polynome

  (** Returns the degree of the polynome *)
  val degree : polynome -> natural_with_infinity

  val evaluation : polynome -> scalar -> scalar

  val zero : unit -> polynome

  val constants : scalar -> polynome

  val add : polynome -> polynome -> polynome

  val mult_by_scalar : scalar -> polynome -> polynome

  val is_null : polynome -> bool

  val is_constant : polynome -> bool

  val opposite : polynome -> polynome

  val equal : polynome -> polynome -> bool

  val of_coefficients : (scalar * int) list -> polynome

  val lagrange_interpolation : (scalar * scalar) list -> polynome

  val to_string : polynome -> string
end

module Make : functor (R : RING_SIG) -> T with type scalar = R.t
