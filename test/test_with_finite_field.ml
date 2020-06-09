let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module F379 = Ff.MakeFp (struct
  let prime_order = Z.of_int 379
end)

module Poly = Polynomial.Make (F379)

module TestDegree = struct
  let test_degree_zero_is_infinity () =
    assert (Poly.degree (Poly.zero ()) = Polynomial.Infinity)

  let test_degree_of_constants_is_one () =
    assert (Poly.degree (Poly.constants (F379.random ())) = Polynomial.Infinity)

  let get_tests () =
    let open Alcotest in
    ( "Tests on degrees",
      [ test_case
          "degree of zero is infinity"
          `Quick
          test_degree_zero_is_infinity;
        test_case
          "degree of constants is one"
          `Quick
          test_degree_zero_is_infinity ] )
end

module TestEvaluation = struct
  let test_eval_random_point_zero_polynomial () =
    assert (F379.is_zero (Poly.evaluation (Poly.zero ()) (F379.random ())))

  let test_eval_at_zero_of_zero_polynomial () =
    assert (F379.is_zero (Poly.evaluation (Poly.zero ()) (F379.zero ())))

  let test_eval_at_zero_point_of_random_constant_polynomial () =
    let constant = F379.random () in
    assert (
      F379.eq
        (Poly.evaluation (Poly.constants constant) (F379.zero ()))
        constant )

  let test_eval_random_point_constant_polynomial () =
    let constant = F379.random () in
    assert (
      F379.eq
        (Poly.evaluation (Poly.constants constant) (F379.random ()))
        constant )

  let test_eval_x_to_random_point () =
    let p = F379.random () in
    assert (
      F379.eq (Poly.evaluation (Poly.of_coefficients [(F379.one (), 1)]) p) p )

  let test_eval_some_test_vectors () =
    let one = F379.one () in
    let p = Poly.of_coefficients [(one, 2); (one, 1); (one, 0)] in
    let evaluation_point_with_expected_value =
      [ (F379.of_string "5", F379.of_string "31");
        (F379.of_string "42", F379.of_string "291");
        (F379.of_string "3", F379.of_string "13");
        (F379.of_string "0", F379.of_string "1") ]
    in
    List.iter
      (fun (x, expected_value) ->
        assert (F379.eq (Poly.evaluation p x) expected_value))
      evaluation_point_with_expected_value ;
    let p =
      Poly.of_coefficients
        [(one, 17); (F379.of_string "-42", 2); (F379.of_string "10", 0)]
    in
    let evaluation_point_with_expected_value =
      [ (F379.of_string "5", F379.of_string "40");
        (F379.of_string "42", F379.of_string "148");
        (F379.of_string "3", F379.of_string "93");
        (F379.of_string "0", F379.of_string "10") ]
    in
    List.iter
      (fun (x, expected_value) ->
        assert (F379.eq (Poly.evaluation p x) expected_value))
      evaluation_point_with_expected_value

  let get_tests () =
    let open Alcotest in
    ( "Test evaluation",
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
        test_case "evaluation test vectors" `Quick test_eval_some_test_vectors;
        test_case
          "evaluation at any point of the polynomial X"
          `Quick
          (repeat 10000 test_eval_x_to_random_point) ] )
end

module TestAdd = struct
  let test_vectors () =
    (* 2 X^2 + 3X *)
    let p1 =
      Poly.of_coefficients [(F379.of_string "2", 2); (F379.of_string "3", 1)]
    in
    (* 10 X + 3 *)
    let p2 =
      Poly.of_coefficients [(F379.of_string "10", 1); (F379.of_string "3", 0)]
    in
    let expected_result =
      Poly.of_coefficients
        [ (F379.of_string "2", 2);
          (F379.of_string "13", 1);
          (F379.of_string "3", 0) ]
    in
    assert (Poly.equal (Poly.add p1 p2) expected_result)

  let get_tests () =
    let open Alcotest in
    ("Test add", [test_case "test vectors" `Quick test_vectors])
end

module TestMultByScalar = struct
  let test_vectors () =
    (* X/2 in F379 *)
    let p1 =
      Poly.mult_by_scalar
        (F379.inverse (F379.of_string "2"))
        (Poly.of_coefficients [(F379.of_string "1", 1)])
    in
    assert (Poly.equal (Poly.of_coefficients [(F379.of_string "190", 1)]) p1)

  let test_multiply_constants_by_scalar_zero_is_zero () =
    let p1 = Poly.constants (F379.random ()) in
    assert (Poly.is_null (Poly.mult_by_scalar (F379.zero ()) p1))

  let test_multiply_degree_one_by_scalar_zero_is_zero () =
    let p1 = Poly.of_coefficients [(F379.of_string "1", 1)] in
    assert (Poly.is_null (Poly.mult_by_scalar (F379.zero ()) p1))

  let get_tests () =
    let open Alcotest in
    ( "Test mult by scalar",
      [ test_case "test vectors" `Quick test_vectors;
        test_case
          "test multiply constants by scalar zero is zero"
          `Quick
          test_multiply_constants_by_scalar_zero_is_zero;
        test_case
          "test multiply degree one by scalar zero is zero"
          `Quick
          test_multiply_degree_one_by_scalar_zero_is_zero ] )
end

module TestOpposite = struct
  let test_property_of_twice_opposite () =
    let p1 = Poly.of_coefficients [(F379.of_string "10", 1)] in
    assert (Poly.equal (Poly.opposite (Poly.opposite p1)) p1)

  let test_property_opposite_of_constant () =
    let random = F379.random () in
    assert (
      Poly.equal
        (Poly.opposite (Poly.constants random))
        (Poly.constants (F379.negate random)) )

  let test_property_opposite_of_zero () =
    assert (Poly.equal (Poly.opposite (Poly.zero ())) (Poly.zero ()))

  let get_tests () =
    let open Alcotest in
    ( "Test opposite",
      [ test_case
          "test property opposite twice"
          `Quick
          test_property_of_twice_opposite;
        test_case
          "test property opposite of constant"
          `Quick
          test_property_opposite_of_constant;
        test_case
          "test property opposite of zero"
          `Quick
          test_property_opposite_of_zero ] )
end

module TestConstructor = struct
  let get_tests () = ()
end

module TestLagrangeInterpolation = struct
  let test_vector () =
    let points =
      [ (F379.of_string "2", F379.of_string "3");
        (F379.of_string "0", F379.of_string "1") ]
    in
    let interpolated_polynomial = Poly.lagrange_interpolation points in
    match Poly.degree interpolated_polynomial with
    | Polynomial.Infinity -> assert false
    | Natural n ->
        assert (n <= List.length points - 1) ;
        (* Printf.printf "Interpolation result is %s" (Poly.to_string interpolated_polynomial); *)
        assert (
          Poly.equal
            (Poly.of_coefficients
               [(F379.of_string "1", 1); (F379.of_string "1", 0)])
            interpolated_polynomial ) ;
        (* print_endline
         * @@ F379.to_string
         *      (Poly.evaluation interpolated_polynomial (F379.of_string "0")) ;
         * print_endline
         * @@ F379.to_string
         *      (Poly.evaluation interpolated_polynomial (F379.of_string "2")) ; *)
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "0"))
            (F379.of_string "1") ) ;
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "2"))
            (F379.of_string "3") ) ;
        (* Other random points *)
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "1"))
            (F379.of_string "2") ) ;
        assert (
          F379.eq
            (Poly.evaluation interpolated_polynomial (F379.of_string "17"))
            (F379.of_string "18") )

  let has_duplicates points =
    let points = List.map fst points in
    let points_uniq =
      List.sort_uniq (fun e1 e2 -> if F379.eq e1 e2 then 1 else 0) points
    in
    not (List.length points = List.length points_uniq)

  let rec test_with_random_number_of_points () =
    let n = Random.int 30 in
    if n <= 0 then test_with_random_number_of_points ()
    else
      let points = List.init n (fun _i -> (F379.random (), F379.random ())) in
      if has_duplicates points then test_with_random_number_of_points ()
      else
        let interpolated_polynomial = Poly.lagrange_interpolation points in
        match Poly.degree interpolated_polynomial with
        | Polynomial.Infinity -> assert false
        | Natural n ->
            assert (n <= List.length points - 1) ;
            List.iter
              (fun (x, y) ->
                assert (F379.eq (Poly.evaluation interpolated_polynomial x) y))
              points

  let get_tests () =
    let open Alcotest in
    ( "Test lagrange interpolation",
      [ test_case "test vector" `Quick test_vector;
        test_case
          "test random number of points"
          `Quick
          (repeat 100 test_with_random_number_of_points) ] )
end

let () =
  let open Alcotest in
  run
    "Test with F379"
    [ TestDegree.get_tests ();
      TestEvaluation.get_tests ();
      TestLagrangeInterpolation.get_tests ();
      TestMultByScalar.get_tests ();
      TestOpposite.get_tests ();
      TestAdd.get_tests () ]
