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
  (** The type of the polynomial coefficiences. Can be a field or more generally a ring *)
  type scalar

  type polynomial

  val degree : polynomial -> natural_with_infinity
  (** Returns the degree of the polynomial *)

  val evaluation : polynomial -> scalar -> scalar
  (** [evaluation P s] computes P(s) *)

  val zero : unit -> polynomial
  (** Returns the polynomial P(X) = 0 *)

  val constants : scalar -> polynomial
  (** [constants s] returns the constant polynomial P(X) = s *)

  val add : polynomial -> polynomial -> polynomial
  (** [add P Q] returns P(X) + Q(X) *)

  val mult_by_scalar : scalar -> polynomial -> polynomial
  (** [mult_by_scalar s P] returns s*P(X) *)

  val is_null : polynomial -> bool
  (** [is_null P] returns [true] iff P(X) = 0 *)

  val is_constant : polynomial -> bool
  (** [is_constant P] returns [true] iff P(X) = s for s scalar *)

  val opposite : polynomial -> polynomial
  (** [opposite P] returns -P(X) *)

  val equal : polynomial -> polynomial -> bool
  (** [equal P Q] returns [true] iff P(X) = Q(X) on S *)

  val of_coefficients : (scalar * int) list -> polynomial
  (** [of_coefficients [(a_i, i)] builds the polynomial Σ(a_i * X^i) as a type [polynomial] *)

  val lagrange_interpolation : (scalar * scalar) list -> polynomial
  (** [lagrange_interpolation [(x_0, y_0) ; (x_1, y_1); ... ; (x_n ; y_n)]]
      builds the unique polynomial P of degre n such that P(x_i) = y_i for i = 0...n
      using the intermediate lagrange polynomials. [lagrange_interpolation_fft] can
      be used in case of a FFT friendly scalar structure
  *)

  val even_polynomial : polynomial -> polynomial
  (** [eval_polynomial P] returns the polynomial P_even containing only the even
      coefficients of P *)

  val odd_polynomial : polynomial -> polynomial
  (** [eval_polynomial P] returns the polynomial P_odd containing only the odd
      coefficients of P *)

  val to_string : polynomial -> string
  (** [to_string P] returns a string representation of P *)

  val get_dense_polynomial_coefficients : polynomial -> scalar list
  (** [get_dense_polynomial_coeffiecients P] returns the list of the coefficients of P, including the null coefficients. *)

  val evaluation_fft :
    generator:scalar -> power:Z.t -> polynomial -> scalar list
  (** [evaluate_fft ~generator ~power P] evaluates P on the points [generator**i] for i = 0...power. [power] must be a power of 2 and [generator] must be a power-th root of unity *)

  val generate_random_polynomial : natural_with_infinity -> polynomial
  (** [generate_random_polynomial n] returns a random polynomial of degree n *)

  val lagrange_interpolation_fft :
    generator:scalar -> power:Z.t -> (scalar * scalar) list -> polynomial
  (** [lagrange_interpolation_fft ~generator ~power [(x_0, y_0) ; (x_1, y_1) ;
      ... (x_n, y_n)]] computes the lagrange interpolation using FFT. The same
      conditions than for [evaluation_fft] must hold *)
end

module Make : functor (R : RING_SIG) -> T with type scalar = R.t
