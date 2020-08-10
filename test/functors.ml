let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module MakeTestDegree
    (Scalar : Polynomial.RING_SIG)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_degree_zero_is_infinity () =
    assert (Poly.degree Poly.zero = Polynomial.Infinity)

  let test_degree_of_constants_is_one () =
    assert (
      Poly.degree (Poly.constants (Scalar.random ())) = Polynomial.Infinity )

  let test_degree_int_test_vectors () =
    let vectors =
      [ (Poly.zero, -1);
        (Poly.generate_random_polynomial (Polynomial.Natural 10), 10);
        (Poly.generate_random_polynomial (Polynomial.Natural 100), 100);
        (Poly.generate_random_polynomial (Polynomial.Natural 0), 0);
        (Poly.generate_random_polynomial (Polynomial.Natural 42), 42) ]
    in
    List.iter
      (fun (p, expected_result) -> assert (Poly.degree_int p = expected_result))
      vectors

  let test_have_same_degree () =
    let rec generate_random_non_null () =
      let r = Scalar.random () in
      if Scalar.is_zero r then generate_random_non_null () else r
    in
    let random_non_null = generate_random_non_null () in
    let test_vectors =
      [ (Poly.zero, Poly.zero, true);
        (Poly.zero, Poly.constants random_non_null, false);
        (Poly.constants random_non_null, Poly.zero, false);
        (Poly.constants random_non_null, Poly.constants random_non_null, true);
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          true );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.zero,
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.constants (Scalar.random ()),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 20),
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 20),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          false ) ]
    in
    List.iter
      (fun (p, q, expected_result) ->
        assert (Poly.have_same_degree p q = expected_result))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Tests on degrees, field order = %s"
        (Z.to_string Scalar.order),
      [ test_case
          "degree of zero is infinity"
          `Quick
          test_degree_zero_is_infinity;
        test_case
          "degree of constants is one"
          `Quick
          test_degree_zero_is_infinity;
        test_case "degree int test vectors" `Quick test_degree_int_test_vectors;
        test_case "have same degree" `Quick test_have_same_degree ] )
end

module MakeTestEvaluation
    (Scalar : Polynomial.RING_SIG)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_eval_random_point_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero (Scalar.random ())))

  let test_eval_at_zero_of_zero_polynomial () =
    assert (Scalar.is_zero (Poly.evaluation Poly.zero Scalar.zero))

  let test_eval_at_zero_point_of_random_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.constants constant) Scalar.zero) constant
    )

  let test_eval_random_point_constant_polynomial () =
    let constant = Scalar.random () in
    assert (
      Scalar.eq
        (Poly.evaluation (Poly.constants constant) (Scalar.random ()))
        constant )

  let test_eval_x_to_random_point () =
    let p = Scalar.random () in
    assert (
      Scalar.eq (Poly.evaluation (Poly.of_coefficients [(Scalar.one, 1)]) p) p
    )

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test evaluation, field order = %s"
        (Z.to_string Scalar.order),
      [ test_case
          "evaluation at any point of the zero polynomial"
          `Quick
          (repeat 10000 test_eval_random_point_zero_polynomial);
        test_case
          "evaluation at any point of a random constant polynomial"
          `Quick
          (repeat 10000 test_eval_random_point_constant_polynomial);
        test_case
          "evaluation at zero of a random constant polynomial"
          `Quick
          (repeat 10000 test_eval_at_zero_point_of_random_constant_polynomial);
        test_case
          "evaluation at zero of the zero polynomial"
          `Quick
          (repeat 10000 test_eval_at_zero_of_zero_polynomial);
        test_case
          "evaluation at any point of the polynomial X"
          `Quick
          (repeat 10000 test_eval_x_to_random_point) ] )
end

module MakeTestLagrangeInterpolation
    (Scalar : Polynomial.RING_SIG)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let rec test_with_random_number_of_points () =
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = Scalar.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    let n = Random.int 30 in
    if n <= 0 then test_with_random_number_of_points ()
    else
      let points =
        List.combine
          (generate_evaluation_points 0 n [])
          (List.init n (fun _i -> Scalar.random ()))
      in
      let interpolated_polynomial = Poly.lagrange_interpolation points in
      match Poly.degree interpolated_polynomial with
      | Polynomial.Infinity ->
          if
            List.length points = 1
            &&
            let (_, x) = List.hd points in
            Scalar.is_zero x
          then assert true
          else assert false
      | Natural n ->
          assert (n <= List.length points - 1) ;
          List.iter
            (fun (x, y) ->
              assert (Scalar.eq (Poly.evaluation interpolated_polynomial x) y))
            points

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Test lagrange interpolation, prime field order %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test random number of points"
          `Quick
          (repeat 100 test_with_random_number_of_points) ] )
end

module MakeTestEuclidianDivision
    (Scalar : Polynomial.RING_SIG)
    (Poly : Polynomial.UNIVARIATE with type scalar = Scalar.t) =
struct
  let test_verify_equality_with_random () =
    let a = Poly.generate_random_polynomial (Polynomial.Natural 100) in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 50) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let test_verify_equality_with_random_divided_by_constant () =
    let a =
      Poly.generate_random_polynomial (Polynomial.Natural (Random.int 1000))
    in
    let b = Poly.generate_random_polynomial (Polynomial.Natural 0) in
    let res = Poly.euclidian_division_opt a b in
    match res with
    | None -> assert false
    | Some (q, r) ->
        assert (Poly.equal a (Poly.add (Poly.polynomial_multiplication b q) r))

  let rec test_with_constants () =
    let a = Scalar.random () in
    let b = Scalar.random () in
    if Scalar.is_zero b || Scalar.is_zero a then test_with_constants ()
    else
      let res =
        Poly.euclidian_division_opt (Poly.constants a) (Poly.constants b)
      in
      match res with
      | None -> assert false
      | Some (q, r) ->
          assert (Poly.equal (Poly.constants Scalar.(a / b)) q && Poly.is_null r)

  let get_tests () =
    let open Alcotest in
    ( Printf.sprintf
        "Euclidian division for prime field %s"
        (Z.to_string Scalar.order),
      [ test_case
          "test vectors for random"
          `Quick
          (repeat 100 test_verify_equality_with_random);
        test_case "test with constants" `Quick (repeat 100 test_with_constants);
        test_case
          "test vectors for random divided by constant"
          `Quick
          (repeat 100 test_verify_equality_with_random_divided_by_constant) ] )
end
