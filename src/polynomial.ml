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
end

(** Represent A[X] *)
module type T = sig
  (* Element of A*)
  type scalar

  (* Represent A[X] *)
  type polynome

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

  (* Compute the Lagrange interpolation based on the given list of points *)
  val lagrange_interpolation : (scalar * scalar) list -> polynome

  val to_string : polynome -> string
end

module Make (R : RING_SIG) = struct
  type scalar = R.t

  (* We encode the two representations in a sum type.
     In the case of coefficients are given, we suppose the dominant factor is non null, and is the first element on the list.
     a_n * X^n + ... a_1 X + a0 with a_n non null is Coefficient [a_n ; ... ; a_1 ; a_0]
  *)
  type polynome =
    | Sparse of (scalar * int) list
    (* | Dense of (scalar * scalar) list *)
    | Zero

  let degree = function
    | Sparse l -> Natural (snd (List.hd l))
    (* | Dense l -> Natural (List.length l - 1) *)
    | Zero -> Infinity

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

  (* Evaluate the given polynome to a point *)
  let evaluation polynome point =
    match polynome with
    | Sparse polynome ->
        List.fold_left
          (fun acc (coef, power) ->
            R.add acc (R.mul coef (R.pow point (Z.of_int power))))
          (R.zero ())
          polynome
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
              else if p = 1 then Printf.sprintf "%sX + %s" (R.to_string e) (inner tail)
              else if p = 0 then Printf.sprintf "%s" (R.to_string e)
              else if R.is_one e then Printf.sprintf "X^%d + %s" p (inner tail)
              else Printf.sprintf "%s X^%d + %s" (R.to_string e) p (inner tail)
        in
        inner p

  let intermediate_lagrange_interpolation x_i i xs =
    (* Printf.printf
     *   "Length xs = %d. Current x_i = %s, pos %d\n"
     *   (List.length xs)
     *   (R.to_string x_i)
     *   i ; *)
    List.fold_left
      (fun acc (j, x_j) ->
        (* Printf.printf "Processing j = %d and x_j = %s\n" j (R.to_string x_j) ; *)
        if i = j then acc
        else
          match acc with
          | Zero -> Zero
          | Sparse acc ->
              let acc_1 = Sparse (List.map (fun (e, p) -> (e, p + 1)) acc) in
              (* Printf.printf
               *   "Multiply %s by X -> %s\n"
               *   (to_string (Sparse acc))
               *   (to_string acc_1) ; *)
              let acc_2 = mult_by_scalar x_j (Sparse acc) in
              (* Printf.printf
               *   "Multiply %s by the scalar x_j %s -> %s\n"
               *   (to_string (Sparse acc))
               *   (R.to_string x_j)
               *   (to_string acc_2) ; *)
              let acc = add acc_1 (opposite acc_2) in
              (* Printf.printf
               *   "Add %s to the opposite of %s (= %s) -> %s\n"
               *   (to_string acc_1)
               *   (to_string acc_2)
               *   (to_string (opposite acc_2))
               *   (to_string acc) ; *)
              let acc_final =
                mult_by_scalar (R.inverse (R.add x_i (R.negate x_j))) acc
              in
              (* Printf.printf
               *   "Multiply %s by the inverse of %s - %s (= %s) -> final result \
               *    is %s\n"
               *   (to_string acc)
               *   (R.to_string x_i)
               *   (R.to_string x_j)
               *   (R.to_string (R.inverse (R.add x_i (R.negate x_j))))
               *   (to_string acc_final) ; *)
              acc_final)
      (constants (R.one ()))
      xs

  let lagrange_interpolation points =
    assert_no_duplicate_point points ;
    let indexed_points = List.mapi (fun i (x_i, y_i) -> (i, x_i, y_i)) points in
    let evaluated_at = List.mapi (fun i (x_i, _) -> (i, x_i)) points in
    List.fold_left
      (fun acc (i, x_i, y_i) ->
        (* Printf.printf
         *   "Computing intermediate lagrange polynome: i = %d, (x, y) = (%s, %s)\n"
         *   i
         *   (R.to_string x_i)
         *   (R.to_string y_i) ; *)
        let l_i = intermediate_lagrange_interpolation x_i i evaluated_at in
        add acc (mult_by_scalar y_i l_i))
      Zero
      indexed_points
end