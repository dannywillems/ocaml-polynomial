type natural_with_infinity = Natural of int | Infinity

(** General module signature for a finite field *)
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

  val of_z : Z.t -> t

  val to_z : t -> Z.t
end

(** Represent A[X] *)
module type T = sig
  (* Element of A*)
  type scalar

  (* Represent A[X] *)
  type polynomial

  val degree : polynomial -> natural_with_infinity

  val have_same_degree : polynomial -> polynomial -> bool

  val shift_by_n : polynomial -> int -> polynomial

  val get_dense_polynomial_coefficients : polynomial -> scalar list

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

  (* Compute the Lagrange interpolation based on the given list of points *)
  val lagrange_interpolation : (scalar * scalar) list -> polynomial

  val even_polynomial : polynomial -> polynomial

  val odd_polynomial : polynomial -> polynomial

  val to_string : polynomial -> string

  val evaluation_fft :
    generator:scalar -> power:Z.t -> polynomial -> scalar list

  val generate_random_polynomial : natural_with_infinity -> polynomial

  val get_highest_coefficient : polynomial -> scalar

  val interpolation_fft :
    generator:scalar -> power:Z.t -> scalar list -> polynomial

  val polynomial_multiplication : polynomial -> polynomial -> polynomial

  val polynomial_multiplication_fft :
    generator:scalar -> power:Z.t -> polynomial -> polynomial -> polynomial
end

module Make (R : RING_SIG) = struct
  type scalar = R.t

  (* We encode the two representations in a sum type.
     In the case of coefficients are given, we suppose the dominant factor is non null, and is the first element on the list.
     a_n * X^n + ... a_1 X + a0 with a_n non null is Coefficient [a_n ; ... ; a_1 ; a_0]
  *)
  type polynomial =
    | Sparse of (scalar * int) list
    (* | Dense of (scalar * scalar) list *)
    | Zero

  let degree = function
    | Zero -> Infinity
    | Sparse l -> (
        match l with
        | [] -> failwith "must never happen"
        | [(e, 0)] -> if R.is_zero e then Infinity else Natural 0
        | _ as l -> Natural (snd (List.hd l)) )

  let have_same_degree p q =
    match (degree p, degree q) with
    | (Infinity, Infinity) -> true
    | (Infinity, _) | (_, Infinity) -> false
    | (Natural n, Natural m) -> n = m

  let shift_by_n p n =
    assert (n >= 1) ;
    match p with
    | Zero -> Zero
    | Sparse l -> Sparse (List.map (fun (c, e) -> (c, e + n)) l)

  let zero () = Zero

  let constants c = Sparse [(c, 0)]

  let is_null p = match p with Zero -> true | _ -> false

  let is_constant p =
    match p with
    | Zero -> true
    | Sparse l ->
        if List.length l > 1 then false
        else
          let (_, p) = List.hd l in
          if p = 0 then true else false

  let add p1 p2 =
    match (p1, p2) with
    | (Zero, p) | (p, Zero) -> p
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
        if List.length l = 0 then Zero else Sparse l

  let mult_by_scalar a p =
    match p with
    | Zero -> Zero
    | Sparse p ->
        let l =
          List.filter_map
            (fun (coef, power) ->
              let c = R.mul coef a in
              if R.is_zero c then None else Some (c, power))
            p
        in
        if List.length l = 0 then Zero else Sparse l

  let opposite = function
    | Zero -> Zero
    | Sparse l -> Sparse (List.map (fun (e, p) -> (R.negate e, p)) l)

  let equal p1 p2 =
    match (p1, p2) with
    | (Zero, Zero) -> true
    | (Zero, _) | (_, Zero) -> false
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
      List.fast_sort (fun (_e1, power1) (_e2, power2) -> power2 - power1) l
    in
    Sparse l

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | Zero -> [R.zero ()]
    | Sparse l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero () :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

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
    | Sparse _ ->
        let coefficients = get_dense_polynomial_coefficients polynomial in
        inner point coefficients (R.zero ())
    | Zero -> R.zero ()

  let assert_no_duplicate_point points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if R.eq e1 e2 then 0 else -1) points
    in
    assert (List.length points = List.length points_uniq)

  let to_string p =
    match p with
    | Zero -> "0"
    | Sparse p ->
        let rec inner l =
          match l with
          | [] -> failwith "Must never happen"
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
          | Zero -> Zero
          | Sparse acc ->
              let acc_1 = Sparse (List.map (fun (e, p) -> (e, p + 1)) acc) in
              let acc_2 = mult_by_scalar x_j (Sparse acc) in
              let acc = add acc_1 (opposite acc_2) in
              let acc_final =
                mult_by_scalar (R.inverse (R.add x_i (R.negate x_j))) acc
              in
              acc_final)
      (constants (R.one ()))
      xs

  let lagrange_interpolation points =
    assert_no_duplicate_point points ;
    let indexed_points = List.mapi (fun i (x_i, y_i) -> (i, x_i, y_i)) points in
    let evaluated_at = List.mapi (fun i (x_i, _) -> (i, x_i)) points in
    List.fold_left
      (fun acc (i, x_i, y_i) ->
        let l_i = intermediate_lagrange_interpolation x_i i evaluated_at in
        add acc (mult_by_scalar y_i l_i))
      Zero
      indexed_points

  let even_polynomial polynomial =
    match polynomial with
    | Zero -> Zero
    | Sparse l ->
        let l = List.filter (fun (_e, n) -> n mod 2 = 0) l in
        if List.length l = 0 then Zero else Sparse l

  let odd_polynomial polynomial =
    match polynomial with
    | Zero -> Zero
    | Sparse l ->
        let l = List.filter (fun (_e, n) -> n mod 2 = 1) l in
        if List.length l = 0 then Zero else Sparse l

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
            let zero = R.zero () in
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
    | Infinity -> Zero
    | Natural n when n > 0 ->
        let coefficients = List.init n (fun _i -> R.random ()) in
        Sparse
          ( (random_non_null (), n)
          :: List.mapi (fun i c -> (c, n - i - 1)) coefficients )
    | _ -> failwith "The degree must be positive"

  let get_highest_coefficient polynomial =
    match polynomial with
    | Zero -> R.zero ()
    | Sparse [] -> failwith "Must never happen"
    | Sparse ((c, _e) :: _) -> c

  let interpolation_fft ~generator ~power points =
    let polynomial =
      of_coefficients (List.rev (List.mapi (fun i p -> (p, i)) points))
    in
    let inverse_generator = R.inverse generator in
    let inverse_fft =
      evaluation_fft ~generator:inverse_generator ~power polynomial
    in
    let polynomial =
      of_coefficients (List.rev (List.mapi (fun i p -> (p, i)) inverse_fft))
    in
    mult_by_scalar (R.inverse (R.of_z power)) polynomial

  let polynomial_multiplication p q =
    match (degree p, degree q) with
    | (Infinity, _) | (_, Infinity) -> Zero
    | (Natural n, Natural m) ->
        let p =
          Array.of_list (List.rev (get_dense_polynomial_coefficients p))
        in
        let q =
          Array.of_list (List.rev (get_dense_polynomial_coefficients q))
        in
        let zero = R.zero () in
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
    assert (R.eq (R.pow generator power) (R.one ())) ;
    if is_null p || is_null q then zero ()
    else (
      assert (have_same_degree p q) ;
      assert (Z.pow (Z.of_string "2") (Z.log2 power) = power) ;
      let p_coefficients = get_dense_polynomial_coefficients p in
      let q_coefficients = get_dense_polynomial_coefficients q in
      let zero = R.zero () in
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
end
