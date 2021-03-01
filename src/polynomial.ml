(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Polynomial_sig

module DomainEvaluation (R : Ff_sig.PRIME) = struct
  type t = { size : int; generator : R.t; domain_values : R.t list }

  let generate_domain generator n =
    let rec aux previous acc i =
      if i = n then List.rev acc
      else
        let current = R.mul previous generator in
        aux current (current :: acc) (i + 1)
    in
    aux R.one [R.one] 1

  let generate size generator =
    { size; generator; domain_values = generate_domain generator size }

  let _size d = d.size

  let _generator d = d.generator

  let domain_values d = d.domain_values
end

(* TODO: Functions should use DomainEvaluation *)
let generate_evaluation_domain (type a)
    (module Fp : Ff_sig.PRIME with type t = a) size (generator : a) : a list =
  let module D = DomainEvaluation (Fp) in
  let g = D.generate size generator in
  D.domain_values g

(* TODO: this function should be part of DomainEvaluation. However, for the
   moment, functions do not use this representation *)
let inverse_domain_values domain =
  let hd = List.hd domain in
  let domain = List.rev (List.tl domain) in
  hd :: domain

module MakeUnivariate (R : Ff_sig.PRIME) = struct
  type scalar = R.t

  (* We encode the polynomials as a list with decresaing degree.
     All coefficient are non zero.
     a_n * X^n + ... a_1 X + a0 is encoded as [a_n ; ... ; a_1 ; a_0] with a_i non zero for all i
  *)
  type polynomial = (scalar * int) list

  let degree p =
    match p with
    | [] -> Infinity
    | [(e, 0)] -> if R.is_zero e then Infinity else Natural 0
    | _ as l -> Natural (snd (List.hd l))

  let degree_int p = match degree p with Infinity -> -1 | Natural n -> n

  let have_same_degree p q = degree p = degree q

  (* let shift_by_n p n =
   *   assert (n >= 1) ;
   *   List.map (fun (c, e) -> (c, e + n)) p *)

  let zero = []

  let one = [(R.one, 0)]

  let constants c = if c = R.zero then [] else [(c, 0)]

  let is_null p = p = []

  let is_constant p =
    match p with
    | [] -> true
    | l ->
        if List.length l > 1 then false
        else
          let (_, p) = List.hd l in
          if p = 0 then true else false

  let of_coefficients ?(remove_null_coefficients = true) l =
    (* check if the powers are all positive *)
    assert (List.for_all (fun (_e, power) -> power >= 0) l) ;
    (* Remove null coefficients *)
    let l =
      if remove_null_coefficients then
        List.filter (fun (e, _power) -> not (R.is_zero e)) l
      else l
    in
    (* sort by the power, higher power first *)
    let l =
      List.fast_sort
        (fun (_e1, power1) (_e2, power2) -> Int.sub power2 power1)
        l
    in
    l

  let add p1 p2 =
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
    let l = inner [] p1 p2 in
    of_coefficients l

  let mult_by_scalar a p =
    List.filter_map
      (fun (coef, power) ->
        let c = R.mul coef a in
        if R.is_zero c then None else Some (c, power))
      p

  let opposite p = List.map (fun (e, p) -> (R.negate e, p)) p

  let sub p1 p2 = add p1 (opposite p2)

  let equal p1 p2 = p1 = p2

  let get_list_coefficients p = p

  let get_dense_polynomial_coefficients polynomial =
    match polynomial with
    | [] -> [R.zero]
    | l ->
        let l = List.rev l in
        let rec to_dense acc current_i l =
          match l with
          | [] -> acc
          | (e, n) :: xs ->
              if n = current_i then to_dense (e :: acc) (current_i + 1) xs
              else to_dense (R.zero :: acc) (current_i + 1) l
        in
        to_dense [] 0 l

  (* let get_dense_polynomial_coefficients_with_degree polynomial =
   *   let coefficients = get_dense_polynomial_coefficients polynomial in
   *   let n = List.length coefficients in
   *   List.mapi (fun i c -> (c, n - i - 1)) coefficients *)

  let evaluation polynomial point =
    let divide_by_xi polynomial i =
      List.map (fun (scalar, degree) -> (scalar, degree - i)) polynomial
    in
    let reversed_polynomial = List.rev polynomial in

    let rec aux reversed_polynomial (accumulated_point, degree_accumlated) =
      match reversed_polynomial with
      | [] -> R.zero
      | (scalar, degree) :: tail ->
          let point_degree =
            R.mul
              (R.pow point (Z.of_int @@ (degree - degree_accumlated)))
              accumulated_point
          in
          let degree_accumlated = degree in
          R.mul
            point_degree
            (R.add
               scalar
               (aux
                  (divide_by_xi tail degree)
                  (point_degree, degree_accumlated)))
    in
    aux reversed_polynomial (R.one, 0)

  let assert_no_duplicate_point points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if R.eq e1 e2 then 0 else -1) points
    in
    assert (List.length points = List.length points_uniq)

  let intermediate_lagrange_interpolation x_i i xs =
    List.fold_left
      (fun acc (j, x_j) ->
        if i = j then acc
        else
          match acc with
          | [] -> []
          | acc ->
              let acc_1 = List.map (fun (e, p) -> (e, p + 1)) acc in
              let acc_2 = mult_by_scalar x_j (of_coefficients acc) in
              let acc = add acc_1 (opposite acc_2) in
              let scalar = R.inverse_exn R.(x_i + R.negate x_j) in
              let acc_final = mult_by_scalar scalar acc in
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
      []
      indexed_points

  let even_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 0) l

  let odd_polynomial polynomial =
    match polynomial with
    | [] -> []
    | l -> List.filter (fun (_e, n) -> n mod 2 = 1) l

  let evaluation_fft ~domain polynomial =
    (* The naive algorithm has been refactorized without using copies of the
       coefficients and the domain to speed up the execution and avoid useless
       memory usage *)
    let n = List.length domain in
    (* Using Array to get a better complexity for `get` *)
    let domain = Array.of_list domain in
    let coefficients =
      Array.of_list (List.rev (get_dense_polynomial_coefficients polynomial))
    in
    assert (n = Array.length coefficients) ;
    (* i is the height in the rec call tree *)
    (* k is the starting index of the branch *)
    let rec inner height k =
      let step = 1 lsl height in
      if step = n then [| coefficients.(k) |]
      else
        let odd_fft = inner (height + 1) (k + step) in
        let even_fft = inner (height + 1) k in
        let output_length = n lsr height in
        let output = Array.init output_length (fun _i -> R.zero) in
        let length_odd = n lsr (height + 1) in
        for i = 0 to length_odd - 1 do
          let x = even_fft.(i) in
          let y = odd_fft.(i) in
          let right = R.mul y domain.(i * step) in
          output.(i) <- R.add x right ;
          output.(i + length_odd) <- R.add x (R.negate right)
        done ;
        output
    in
    Array.to_list (inner 0 0)

  let _evaluation_fft_imperative ~domain polynomial =
    ignore domain ;
    ignore polynomial ;
    ()

  let evaluation_fft_marc ~domain polynomial =
    (* The naive algorithm has been refactorized without using copies of the
       coefficients and the domain to speed up the execution and avoid useless
       memory usage *)
    let n = List.length domain in
    (* Using Array to get a better complexity for `get` *)
    let domain = Array.of_list domain in
    let coefficients =
      Array.of_list (List.rev (get_dense_polynomial_coefficients polynomial))
    in
    (* assert (n = Array.length coefficients) ; *)
    (* i is the height in the rec call tree *)
    (* k is the starting index of the branch *)
    let rec inner height k =
      let step = 1 lsl height in
      if step = n / 2 then [| coefficients.(k); coefficients.(k) |]
      else
        let odd_fft = inner (height + 1) (k + step) in
        let even_fft = inner (height + 1) k in
        let output_length = n lsr height in
        let output = Array.init output_length (fun _i -> R.zero) in
        let length_odd = n lsr (height + 1) in
        for i = 0 to length_odd - 1 do
          let x = even_fft.(i) in
          let y = odd_fft.(i) in
          let right = R.mul y domain.(i * step) in
          output.(i) <- R.add x right ;
          output.(i + length_odd) <- R.add x (R.negate right)
        done ;
        output
    in
    Array.to_list (inner 0 0)

  let generate_random_polynomial degree =
    let rec random_non_null () =
      let r = R.random () in
      if R.is_zero r then random_non_null () else r
    in
    match degree with
    | Infinity -> []
    | Natural n when n >= 0 ->
        let coefficients = List.init n (fun _i -> R.random ()) in
        let coefficients =
          (random_non_null (), n)
          :: List.mapi (fun i c -> (c, n - i - 1)) coefficients
        in
        of_coefficients coefficients
    | _ -> failwith "The degree must be positive"

  let get_highest_coefficient polynomial =
    match polynomial with [] -> R.zero | (c, _e) :: _ -> c

  let interpolation_fft ~domain points =
    let polynomial = List.rev (List.mapi (fun i p -> (p, i)) points) in
    let inverse_domain = inverse_domain_values domain in
    let power = Z.of_int (List.length domain) in
    let inverse_fft = evaluation_fft polynomial ~domain:inverse_domain in
    let polynomial = List.rev (List.mapi (fun i p -> (p, i)) inverse_fft) in
    mult_by_scalar (R.inverse_exn (R.of_z power)) polynomial

  let polynomial_multiplication p q =
    let mul_by_monom (scalar, int) p =
      List.map (fun (scalar_2, int_2) -> (R.mul scalar scalar_2, int + int_2)) p
    in
    List.fold_left (fun acc monom -> add acc (mul_by_monom monom q)) zero p

  let polynomial_multiplication_fft ~domain p q =
    let generator = List.nth domain 1 in
    let power = Z.of_int (List.length domain) in
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
      let p' = evaluation_fft ~domain (of_coefficients p_coefficients) in
      let q' = evaluation_fft ~domain (of_coefficients q_coefficients) in
      let coefficients = List.map2 (fun p_x q_x -> R.mul p_x q_x) p' q' in
      interpolation_fft ~domain coefficients )

  let euclidian_division_opt a b =
    if is_null b then None
    else
      let deg_b = degree_int b in
      let highest_coeff_b = get_highest_coefficient b in
      let rec aux q r =
        if degree_int r < deg_b then Some (q, r)
        else
          let diff_degree = degree_int r - deg_b in
          let rescale_factor =
            R.(get_highest_coefficient r / highest_coeff_b)
          in
          let to_sub =
            polynomial_multiplication b [(rescale_factor, diff_degree)]
          in
          aux (add q [(rescale_factor, diff_degree)]) (sub r to_sub)
      in
      aux zero a

  let extended_euclide polynomial_1 polynomial_2 =
    let n_1 = degree_int polynomial_1 and n_2 = degree_int polynomial_2 in
    if n_1 = -1 then (polynomial_2, zero, one)
    else if n_2 = -1 then (polynomial_1, one, zero)
    else
      let rec aux poly_1 u_1 v_1 poly_2 u_2 v_2 =
        let (q, r) = euclidian_division_opt poly_1 poly_2 |> Option.get in
        if is_null r then (poly_2, u_2, v_2)
        else
          aux
            poly_2
            u_2
            v_2
            r
            (sub u_1 (polynomial_multiplication q u_2))
            (sub v_1 (polynomial_multiplication q v_2))
      in
      if n_2 > n_1 then
        let (gcd, u, v) = aux polynomial_2 one zero polynomial_1 zero one in
        let rescale_factor = R.inverse_exn @@ get_highest_coefficient gcd in
        ( mult_by_scalar rescale_factor gcd,
          mult_by_scalar rescale_factor v,
          mult_by_scalar rescale_factor u )
      else
        let (gcd, u, v) = aux polynomial_1 one zero polynomial_2 zero one in
        let rescale_factor = R.inverse_exn @@ get_highest_coefficient gcd in
        ( mult_by_scalar rescale_factor gcd,
          mult_by_scalar rescale_factor u,
          mult_by_scalar rescale_factor v )

  let ( = ) = equal

  let ( + ) = add

  let ( * ) = polynomial_multiplication

  let ( - ) = sub
end
