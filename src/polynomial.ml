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
  (** The type of the polynomial coefficients. Can be a field or more generally
      a ring
  *)
  type scalar

  (** Represents a polynomial *)
  type polynomial

  val degree : polynomial -> natural_with_infinity
  (** Returns the degree of the polynomial *)

  val degree_int : polynomial -> int

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

  val get_dense_polynomial_coefficients_with_degree :
    polynomial -> (scalar * int) list

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
      be used in case of a FFT friendly scalar structure. It is supposed all x_i
      are different.
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

  val euclidian_division_opt :
    polynomial -> polynomial -> (polynomial * polynomial) option

  val ( = ) : polynomial -> polynomial -> bool
  (** Infix operator for [equal] *)

  val ( + ) : polynomial -> polynomial -> polynomial
  (** Infix operator for [add] *)

  val ( * ) : polynomial -> polynomial -> polynomial
  (** Infix operator for [polynomial_multiplication] *)

  val ( - ) : polynomial -> polynomial
  (** Infix operator for [opposite] *)
end

module Make (R : RING_SIG) = struct
  type scalar = R.t

  (* We encode the two representations in a sum type.
     In the case of coefficients are given, we suppose the dominant factor is non null, and is the first element on the list.
     a_n * X^n + ... a_1 X + a0 with a_n non null is Coefficient [a_n ; ... ; a_1 ; a_0]
  *)
  type polynomial = Sparse of (scalar * int) list

  (* | Dense of (scalar * scalar) list *)

  let degree = function
    | Sparse l -> (
        match l with
        | [] -> Infinity
        | [(e, 0)] -> if R.is_zero e then Infinity else Natural 0
        | _ as l -> Natural (snd (List.hd l)) )

  let degree_int p = match degree p with Infinity -> -1 | Natural n -> n

  let have_same_degree p q =
    match (degree p, degree q) with
    | (Infinity, Infinity) -> true
    | (Infinity, _) | (_, Infinity) -> false
    | (Natural n, Natural m) -> n = m

  let shift_by_n p n =
    assert (n >= 1) ;
    match p with Sparse l -> Sparse (List.map (fun (c, e) -> (c, e + n)) l)

  let zero = Sparse []

  let constants c = Sparse [(c, 0)]

  let is_null p = match p with Sparse [] -> true | _ -> false

  let is_constant p =
    match p with
    | Sparse [] -> true
    | Sparse l ->
        if List.length l > 1 then false
        else
          let (_, p) = List.hd l in
          if p = 0 then true else false

  let add p1 p2 =
    match (p1, p2) with
    | (Sparse [], p) | (p, Sparse []) -> p
    | (Sparse l1, Sparse l2) ->
        let rec inner acc l1 l2 =
          match (l1, l2) with
          | ([], l) | (l, []) -> List.concat [List.rev acc; l]
          | (l1, l2) ->
              let (e1, p1) = List.hd l1 in
              let (e2, p2) = List.hd l2 in
              if p1 = p2 && R.is_zero (R.add e1 e2) then
                inner acc (List.tl l1) (List.tl l2)
              else if p1 = p2 then
                inner ((R.add e1 e2, p1) :: acc) (List.tl l1) (List.tl l2)
              else if p1 > p2 then inner ((e1, p1) :: acc) (List.tl l1) l2
              else inner ((e2, p2) :: acc) l1 (List.tl l2)
        in
        let l = inner [] l1 l2 in
        if List.length l = 0 then Sparse [] else Sparse l

  let mult_by_scalar a p =
    match p with
    | Sparse [] -> Sparse []
    | Sparse p ->
        let l =
          List.filter_map
            (fun (coef, power) ->
              let c = R.mul coef a in
              if R.is_zero c then None else Some (c, power))
            p
        in
        if List.length l = 0 then Sparse [] else Sparse l

  let opposite = function
    | Sparse [] -> Sparse []
    | Sparse l -> Sparse (List.map (fun (e, p) -> (R.negate e, p)) l)

  let equal p1 p2 =
    match (p1, p2) with
    | (Sparse [], Sparse []) -> true
    | (Sparse [], _) | (_, Sparse []) -> false
    | (Sparse l1, Sparse l2) ->
        let rec inner p1 p2 =
          match (p1, p2) with
          | ([], []) -> true
          | ([], _) | (_, []) -> false
          | ((e1, p1) :: l1, (e2, p2) :: l2) ->
              if p1 <> p2 then false
              else if e1 <> e2 then false
              else inner l1 l2
        in
        inner l1 l2

  let of_coefficients l =
    (* check if the powers are all positive *)
    assert (List.for_all (fun (_e, power) -> power >= 0) l) ;
    (* Remove null coefficients *)
    let l = List.filter (fun (e, _power) -> not (R.is_zero e)) l in
    (* sort by the power, higher power first *)
    let l =
      List.fast_sort
        (fun (_e1, power1) (_e2, power2) -> Int.sub power2 power1)
        l
    in
    Sparse l

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | Sparse [] -> [R.zero]
    | Sparse l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

  let get_dense_polynomial_coefficients_with_degree polynomial =
    let coefficients = get_dense_polynomial_coefficients polynomial in
    List.rev (List.mapi (fun i c -> (c, i)) (List.rev coefficients))

  (* Evaluate the given polynomial to a point *)
  let evaluation polynomial point =
    let rec inner point l acc =
      match l with
      | [] -> acc
      | coef :: tail ->
          let acc = R.add (R.mul acc point) coef in
          inner point tail acc
    in
    match polynomial with
    | Sparse [] -> R.zero
    | Sparse _ ->
        let coefficients = get_dense_polynomial_coefficients polynomial in
        inner point coefficients R.zero

  let assert_no_duplicate_point points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if R.eq e1 e2 then 0 else -1) points
    in
    assert (List.length points = List.length points_uniq)

  let to_string p =
    match p with
    | Sparse p ->
        let rec inner l =
          match l with
          | [] -> "0"
          | [(e, p)] ->
              if R.is_one e && p = 1 then Printf.sprintf "X"
              else if p = 1 then Printf.sprintf "%sX" (R.to_string e)
              else if p = 0 then Printf.sprintf "%s" (R.to_string e)
              else if R.is_one e then Printf.sprintf "X^%d" p
              else Printf.sprintf "%s X^%d" (R.to_string e) p
          | (e, p) :: tail ->
              if R.is_one e && p = 1 then Printf.sprintf "X + %s" (inner tail)
              else if p = 1 then
                Printf.sprintf "%sX + %s" (R.to_string e) (inner tail)
              else if p = 0 then Printf.sprintf "%s" (R.to_string e)
              else if R.is_one e then Printf.sprintf "X^%d + %s" p (inner tail)
              else Printf.sprintf "%s X^%d + %s" (R.to_string e) p (inner tail)
        in
        inner p

  let intermediate_lagrange_interpolation x_i i xs =
    List.fold_left
      (fun acc (j, x_j) ->
        if i = j then acc
        else
          match acc with
          | Sparse [] -> Sparse []
          | Sparse acc ->
              let acc_1 = Sparse (List.map (fun (e, p) -> (e, p + 1)) acc) in
              let acc_2 = mult_by_scalar x_j (Sparse acc) in
              let acc = add acc_1 (opposite acc_2) in
              let acc_final =
                mult_by_scalar (R.inverse_exn (R.add x_i (R.negate x_j))) acc
              in
              acc_final)
      (constants R.one)
      xs

  let lagrange_interpolation points =
    assert_no_duplicate_point points ;
    let indexed_points = List.mapi (fun i (x_i, y_i) -> (i, x_i, y_i)) points in
    let evaluated_at = List.mapi (fun i (x_i, _) -> (i, x_i)) points in
    List.fold_left
      (fun acc (i, x_i, y_i) ->
        let l_i = intermediate_lagrange_interpolation x_i i evaluated_at in
        add acc (mult_by_scalar y_i l_i))
      (Sparse [])
      indexed_points

  let even_polynomial polynomial =
    match polynomial with
    | Sparse [] -> Sparse []
    | Sparse l ->
        let l = List.filter (fun (_e, n) -> n mod 2 = 0) l in
        if List.length l = 0 then Sparse [] else Sparse l

  let odd_polynomial polynomial =
    match polynomial with
    | Sparse [] -> Sparse []
    | Sparse l ->
        let l = List.filter (fun (_e, n) -> n mod 2 = 1) l in
        if List.length l = 0 then Sparse [] else Sparse l

  let filter_mapi (f : int -> 'a -> 'b option) l =
    let l = List.mapi (fun i a -> (i, a)) l in
    List.filter_map (fun (i, a) -> f i a) l

  let evaluation_fft ~generator ~power polynomial =
    assert (Z.pow (Z.of_string "2") (Z.log2 power) = power) ;
    assert (R.is_one (R.pow generator power)) ;
    (* We only take exponents module the order. It is useful for inverse fft as we divide by the power *)
    assert (Z.leq power R.order) ;
    assert (power >= Z.one) ;
    let rec inner domain coefficients =
      match coefficients with
      | [] -> failwith "Must never happen"
      | l ->
          if List.length l = 1 then l
          else
            let new_domain =
              Array.of_list
                (filter_mapi
                   (fun i e -> if i mod 2 = 0 then Some e else None)
                   (Array.to_list domain))
            in
            let odd_coeffients =
              filter_mapi
                (fun i e -> if i mod 2 = 1 then Some e else None)
                coefficients
            in
            let even_coeffients =
              filter_mapi
                (fun i e -> if i mod 2 = 0 then Some e else None)
                coefficients
            in
            let odd_fft = inner new_domain odd_coeffients in
            let even_fft = inner new_domain even_coeffients in
            let combined_fft = List.combine even_fft odd_fft in
            (* only one allocation, used for the output initialization *)
            let zero = R.zero in
            let length_odd = List.length odd_coeffients in
            let output =
              Array.init (List.length coefficients) (fun _i -> zero)
            in
            List.iteri
              (fun i (x, y) ->
                let right = R.mul y domain.(i) in
                output.(i) <- R.add x right ;
                output.(i + length_odd) <- R.add x (R.negate right))
              combined_fft ;
            Array.to_list output
    in
    let domain =
      List.init (Z.to_int power) (fun i -> R.pow generator (Z.of_int i))
    in
    (* we reverse to have the scalar first and have the correspondance of the coefficients of degree n with the index of the list *)
    let coefficients =
      List.rev (get_dense_polynomial_coefficients polynomial)
    in
    assert (List.length domain = List.length coefficients) ;
    inner (Array.of_list domain) coefficients

  let generate_random_polynomial degree =
    let rec random_non_null () =
      let r = R.random () in
      if R.is_zero r then random_non_null () else r
    in
    match degree with
    | Infinity -> Sparse []
    | Natural n when n >= 0 ->
        let coefficients = List.init n (fun _i -> R.random ()) in
        Sparse
          ( (random_non_null (), n)
          :: List.mapi (fun i c -> (c, n - i - 1)) coefficients )
    | _ -> failwith "The degree must be positive"

  let get_highest_coefficient polynomial =
    match polynomial with Sparse [] -> R.zero | Sparse ((c, _e) :: _) -> c

  let interpolation_fft ~generator ~power points =
    let polynomial =
      of_coefficients (List.rev (List.mapi (fun i p -> (p, i)) points))
    in
    let inverse_generator = R.inverse_exn generator in
    let inverse_fft =
      evaluation_fft ~generator:inverse_generator ~power polynomial
    in
    let polynomial =
      of_coefficients (List.rev (List.mapi (fun i p -> (p, i)) inverse_fft))
    in
    mult_by_scalar (R.inverse_exn (R.of_z power)) polynomial

  let polynomial_multiplication p q =
    match (degree p, degree q) with
    | (Infinity, _) | (_, Infinity) -> Sparse []
    | (Natural n, Natural m) ->
        let p =
          Array.of_list (List.rev (get_dense_polynomial_coefficients p))
        in
        let q =
          Array.of_list (List.rev (get_dense_polynomial_coefficients q))
        in
        let zero = R.zero in
        let p_times_q = Array.make (n + m + 1) zero in
        for i = 0 to n do
          for j = 0 to m do
            p_times_q.(i + j) <- R.add p_times_q.(i + j) (R.mul p.(i) q.(j))
          done
        done ;
        let p_times_q =
          List.mapi (fun e c -> (c, e)) (Array.to_list p_times_q)
        in
        Sparse (List.rev p_times_q)

  let polynomial_multiplication_fft ~generator ~power p q =
    assert (R.eq (R.pow generator power) R.one) ;
    if is_null p || is_null q then zero
    else (
      assert (have_same_degree p q) ;
      assert (Z.pow (Z.of_string "2") (Z.log2 power) = power) ;
      let p_coefficients = get_dense_polynomial_coefficients p in
      let q_coefficients = get_dense_polynomial_coefficients q in
      let zero = R.zero in
      let p_coefficients =
        List.append
          p_coefficients
          (List.init
             (Z.to_int power - List.length p_coefficients)
             (fun _i -> zero))
      in
      let p_coefficients =
        List.mapi (fun i c -> (c, i)) (List.rev p_coefficients)
      in
      let q_coefficients =
        List.append
          q_coefficients
          (List.init
             (Z.to_int power - List.length q_coefficients)
             (fun _i -> zero))
      in
      let q_coefficients =
        List.mapi (fun i c -> (c, i)) (List.rev q_coefficients)
      in
      let p' =
        evaluation_fft ~generator ~power (of_coefficients p_coefficients)
      in
      let q' =
        evaluation_fft ~generator ~power (of_coefficients q_coefficients)
      in
      let coefficients = List.map2 (fun p_x q_x -> R.mul p_x q_x) p' q' in
      interpolation_fft ~generator ~power coefficients )

  let euclidian_division_opt a b =
    (* Euclidian algorithm *)
    let rec internal i deg_b dominant_coef_b b q r =
      let deg_r = List.length r - 1 in
      (* if the rest is null, it means it B is A divisor of A *)
      if deg_r = -1 || i = 3 then Some (of_coefficients q, of_coefficients r)
      else if deg_r < deg_b then Some (of_coefficients q, of_coefficients r)
      else
        let dominant_coef_r = fst (List.hd r) in
        let s = R.(dominant_coef_r / dominant_coef_b) in
        (* q = q + s * X^(deg_r - deg_b). We always increase the degree of q *)
        let q = List.rev ((s, deg_r - deg_b) :: q) in
        let temp_b =
          List.map (fun (c, e) -> (R.(negate (c * s)), e + deg_r - deg_b)) b
        in
        (* deg_r >= deg_temp_b*)
        let r = add (of_coefficients r) (of_coefficients temp_b) in
        internal
          (i + 1)
          deg_b
          dominant_coef_b
          b
          q
          (get_dense_polynomial_coefficients_with_degree r)
    in
    match (a, b) with
    (* Impossible to divide by 0 (B = 0) *)
    | (_, Sparse []) -> None
    (* If A = 0, A = 0 * B + 0 *)
    | (Sparse [], _) -> Some (Sparse [], Sparse [])
    | (Sparse coef_a, Sparse coef_b) ->
        let deg_a_natural = degree_int a in
        let deg_b_natural = degree_int b in
        (* If A has a lower degree than B -> A = 0 * B + B *)
        if deg_b_natural > deg_a_natural then Some (Sparse [], b)
        else internal 0 deg_b_natural (fst (List.hd coef_b)) coef_b [] coef_a

  let ( = ) = equal

  let ( + ) = add

  let ( * ) = polynomial_multiplication

  let ( - ) = opposite
end
