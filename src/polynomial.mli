type natural_with_infinity = Natural of int | Infinity

(** General module signature for a ring [(A, +, *, 0_A, 1_A)] *)
module type RING_SIG = sig
  type t

  val order : Z.t
  (** The order of the finite field *)

  val size_in_bytes : int
  (** minimal number of bytes required to encode a value of the field. *)

  val zero : t
  (** The neutral element for the addition *)

  val one : t
  (** The neutral element for the multiplication *)

  val is_zero : t -> bool
  (** [is_zero x] returns [true] if [x] is the neutral element for the addition *)

  val is_one : t -> bool
  (** [is_one x] returns [true] if [x] is the neutral element for the multiplication *)

  val random : unit -> t
  (** [random ()] returns a random element of the field *)

  val add : t -> t -> t
  (** [add a b] returns [a + b mod order] *)

  val ( + ) : t -> t -> t
  (** Infix operator for [add] *)

  val mul : t -> t -> t
  (** [mul a b] returns [a * b mod order] *)

  val ( * ) : t -> t -> t
  (** Infix operator for [mul] *)

  val eq : t -> t -> bool
  (** [eq a b] returns [true] if [a = b mod order], else [false] *)

  val ( = ) : t -> t -> bool
  (** Infix operator for [eq] *)

  val negate : t -> t
  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)

  val ( - ) : t -> t
  (** Infix operator for [negate] *)

  val inverse_exn : t -> t
  (** [inverse_exn x] returns [x^-1] if [x] is not [0], else raise
      [Division_by_zero]
  *)

  val inverse_opt : t -> t option
  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)

  val div_exn : t -> t -> t
  (** [div_exn a b] returns [a * b^-1]. Raise [Division_by_zero] if [b = zero] *)

  val div_opt : t -> t -> t option
  (** [div_opt a b] returns [a * b^-1] as an option. Return [None] if [b = zero] *)

  val ( / ) : t -> t -> t
  (** Infix operator for [div_exn] *)

  val square : t -> t
  (** [square x] returns [x^2] *)

  val double : t -> t
  (** [double x] returns [2x] *)

  val pow : t -> Z.t -> t
  (** [pow x n] returns [x^n] *)

  val ( ** ) : t -> Z.t -> t
  (** Infix operator for [pow] *)

  val to_string : t -> string
  (** String representation of a value t. It is not required that to_string
      of_string t = t. By default, decimal representation of the number is
      used *)

  val of_z : Z.t -> t
  (** [of_z x] builds an element t from the Zarith element x. [mod order] is
      applied if [x > order] *)

  val to_z : t -> Z.t
  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integer *)
end

module type T = sig
  (** The type of the polynomial coefficients. Can be a field or more generally a ring *)
  type scalar

  (** Represents a polynomial *)
  type polynomial

  val degree : polynomial -> natural_with_infinity
  (** Returns the degree of the polynomial *)

  val have_same_degree : polynomial -> polynomial -> bool
  (** [have_same_degree P Q] returns [true] if [P] and [Q] have the same
      degree
  *)

  val shift_by_n : polynomial -> int -> polynomial
  (** [shift_by_n P n] multiplies [P] by [X^n]. For instance,
      [P(X) = a_{0} + a_{1} X + ... + a_{m} X^m] will be transformed in
      [a_{0} X^{n} + a_{1} X^{n + 1} + ... a_{m} X^{n + m}].
  *)

  val get_dense_polynomial_coefficients : polynomial -> scalar list
  (** [get_dense_polynomial_coeffiecients P] returns the list of the
      coefficients of P, including the null coefficients, in decreasing order
      i.e. if P(X) = a_{0} + a_{1} X + ... + a_{n - 1} X^{n - 1}, the function
      will return [a_{n - 1}, ..., a_{0}]
  *)

  val evaluation : polynomial -> scalar -> scalar
  (** [evaluation P s] computes [P(s)]. Use Horner's method in O(n). *)

  val zero : polynomial
  (** Returns the polynomial [P(X) = 0] *)

  val constants : scalar -> polynomial
  (** [constants s] returns the constant polynomial [P(X) = s] *)

  val add : polynomial -> polynomial -> polynomial
  (** [add P Q] returns [P(X) + Q(X)] *)

  val mult_by_scalar : scalar -> polynomial -> polynomial
  (** [mult_by_scalar s P] returns [s*P(X)] *)

  val is_null : polynomial -> bool
  (** [is_null P] returns [true] iff [P(X) = 0] *)

  val is_constant : polynomial -> bool
  (** [is_constant P] returns [true] iff [P(X) = s] for s scalar *)

  val opposite : polynomial -> polynomial
  (** [opposite P] returns [-P(X)] *)

  val equal : polynomial -> polynomial -> bool
  (** [equal P Q] returns [true] iff [P(X) = Q(X)] on S *)

  val of_coefficients : (scalar * int) list -> polynomial
  (** [of_coefficients [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]] builds the
      polynomial Σ(a_i * X^i) as a type [polynomial] *)

  val lagrange_interpolation : (scalar * scalar) list -> polynomial
  (** [lagrange_interpolation [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]]
      builds the unique polynomial P of degre n such that P(x_i) = y_i for i = 0...n
      using the intermediate lagrange polynomials. [lagrange_interpolation_fft] can
      be used in case of a FFT friendly scalar structure. It is supposed all x_i are different.
  *)

  val even_polynomial : polynomial -> polynomial
  (** [even_polynomial P] returns the polynomial P_even containing only the even
      coefficients of P *)

  val odd_polynomial : polynomial -> polynomial
  (** [odd_polynomial P] returns the polynomial P_odd containing only the odd
      coefficients of P *)

  val to_string : polynomial -> string
  (** [to_string P] returns a string representation of P *)

  val evaluation_fft :
    generator:scalar -> power:Z.t -> polynomial -> scalar list
  (** [evaluate_fft ~generator:g ~power P] evaluates P on the points [{g^i}] for
      [i = 0...power]. [power] must be a power of 2 and [generator] must be a
      power-th root of unity *)

  val generate_random_polynomial : natural_with_infinity -> polynomial
  (** [generate_random_polynomial n] returns a random polynomial of degree n *)

  val get_highest_coefficient : polynomial -> scalar
  (** [get_highest_coefficient P] where [P(X) = a_n X^n + ... a_0] returns [a_n] *)

  val interpolation_fft :
    generator:scalar -> power:Z.t -> scalar list -> polynomial
  (** [interpolation_fft ~generator ~power [y_0 ; y_1 ;
      ... y_n]] computes the interpolation using FFT Cookey Tukey. The same
      conditions than for [evaluation_fft] must hold. [x_0] must be the
      evaluation of the generator *)

  val polynomial_multiplication : polynomial -> polynomial -> polynomial
  (** [polynomial_multiplication P Q] computes the
      product P(X).Q(X) *)

  val polynomial_multiplication_fft :
    generator:scalar -> power:Z.t -> polynomial -> polynomial -> polynomial
  (** [polynomial_multiplication_fft ~generator:g ~power:n P Q] computes the
      product P(X).Q(X) using FFT. [g] is a [power]-th roots of unity.*)

  val ( = ) : polynomial -> polynomial -> bool
  (** Infix operator for [equal] *)

  val ( + ) : polynomial -> polynomial -> polynomial
  (** Infix operator for [add] *)

  val ( * ) : polynomial -> polynomial -> polynomial
  (** Infix operator for [polynomial_multiplication] *)

  val ( - ) : polynomial -> polynomial
  (** Infix operator for [opposite] *)
end

(** [Make(R)] builds a module of type [T] where the coefficients are of type R.t *)
module Make : functor (R : RING_SIG) -> T with type scalar = R.t
