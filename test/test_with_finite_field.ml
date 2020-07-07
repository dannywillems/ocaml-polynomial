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
    assert (Poly.degree Poly.zero = Polynomial.Infinity)

  let test_degree_of_constants_is_one () =
    assert (Poly.degree (Poly.constants (F379.random ())) = Polynomial.Infinity)

  let test_have_same_degree () =
    let test_vectors =
      [ (Poly.zero, Poly.zero, true);
        (Poly.zero, Poly.constants (F379.random ()), false);
        (Poly.constants (F379.random ()), Poly.zero, false);
        (Poly.constants (F379.random ()), Poly.constants (F379.random ()), true);
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.generate_random_polynomial (Polynomial.Natural 10),
          true );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.zero,
          false );
        ( Poly.generate_random_polynomial (Polynomial.Natural 10),
          Poly.constants (F379.random ()),
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
    ( "Tests on degrees",
      [ test_case
          "degree of zero is infinity"
          `Quick
          test_degree_zero_is_infinity;
        test_case
          "degree of constants is one"
          `Quick
          test_degree_zero_is_infinity;
        test_case "have same degree" `Quick test_have_same_degree ] )
end

module TestEvaluation = struct
  let test_eval_random_point_zero_polynomial () =
    assert (F379.is_zero (Poly.evaluation Poly.zero (F379.random ())))

  let test_eval_at_zero_of_zero_polynomial () =
    assert (F379.is_zero (Poly.evaluation Poly.zero F379.zero))

  let test_eval_at_zero_point_of_random_constant_polynomial () =
    let constant = F379.random () in
    assert (
      F379.eq (Poly.evaluation (Poly.constants constant) F379.zero) constant )

  let test_eval_random_point_constant_polynomial () =
    let constant = F379.random () in
    assert (
      F379.eq
        (Poly.evaluation (Poly.constants constant) (F379.random ()))
        constant )

  let test_eval_x_to_random_point () =
    let p = F379.random () in
    assert (F379.eq (Poly.evaluation (Poly.of_coefficients [(F379.one, 1)]) p) p)

  let test_eval_some_test_vectors () =
    let one = F379.one in
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
        (F379.inverse_exn (F379.of_string "2"))
        (Poly.of_coefficients [(F379.of_string "1", 1)])
    in
    assert (Poly.equal (Poly.of_coefficients [(F379.of_string "190", 1)]) p1)

  let test_multiply_constants_by_scalar_zero_is_zero () =
    let p1 = Poly.constants (F379.random ()) in
    assert (Poly.is_null (Poly.mult_by_scalar F379.zero p1))

  let test_multiply_degree_one_by_scalar_zero_is_zero () =
    let p1 = Poly.of_coefficients [(F379.of_string "1", 1)] in
    assert (Poly.is_null (Poly.mult_by_scalar F379.zero p1))

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
    assert (Poly.(Poly.opposite Poly.zero = Poly.zero))

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
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = F379.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    if n <= 0 then test_with_random_number_of_points ()
    else
      let points =
        List.combine
          (generate_evaluation_points 0 n [])
          (List.init n (fun _i -> F379.random ()))
      in
      let interpolated_polynomial = Poly.lagrange_interpolation points in
      match Poly.degree interpolated_polynomial with
      | Polynomial.Infinity ->
          if
            List.length points = 1
            &&
            let (_, x) = List.hd points in
            F379.is_zero x
          then assert true
          else assert false
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

module TestSplitPolynomial = struct
  let test_even_polynomial () =
    let x = F379.random () in
    let test_vectors =
      [ (Poly.zero, Poly.zero);
        (Poly.constants x, Poly.constants x);
        (Poly.of_coefficients [(x, 2)], Poly.of_coefficients [(x, 2)]);
        (Poly.of_coefficients [(x, 1)], Poly.zero);
        (Poly.of_coefficients [(x, 3); (x, 1)], Poly.zero);
        (Poly.of_coefficients [(x, 4); (x, 1)], Poly.of_coefficients [(x, 4)]);
        ( Poly.of_coefficients
            [ (x, 34534);
              (x, 345);
              (x, 23);
              (x, 21);
              (x, 17);
              (x, 14);
              (x, 3);
              (x, 1);
              (x, 0) ],
          Poly.of_coefficients [(x, 34534); (x, 14); (x, 0)] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        assert (Poly.equal expected_result (Poly.even_polynomial v)))
      test_vectors

  let test_odd_polynomial () =
    let x = F379.random () in
    let test_vectors =
      [ (Poly.zero, Poly.zero);
        (Poly.constants x, Poly.zero);
        (Poly.of_coefficients [(x, 2)], Poly.zero);
        (Poly.of_coefficients [(x, 1)], Poly.of_coefficients [(x, 1)]);
        ( Poly.of_coefficients [(x, 3); (x, 1)],
          Poly.of_coefficients [(x, 3); (x, 1)] );
        (Poly.of_coefficients [(x, 4); (x, 1)], Poly.of_coefficients [(x, 1)]);
        ( Poly.of_coefficients
            [ (x, 34534);
              (x, 345);
              (x, 23);
              (x, 21);
              (x, 17);
              (x, 14);
              (x, 3);
              (x, 1);
              (x, 0) ],
          Poly.of_coefficients
            [(x, 345); (x, 23); (x, 21); (x, 17); (x, 3); (x, 1)] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        assert (Poly.equal expected_result (Poly.odd_polynomial v)))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( "Split polynomials",
      [ test_case
          "Test even polynomial with test vectors"
          `Quick
          test_even_polynomial;
        test_case
          "Test odd polynomial with test vectors"
          `Quick
          test_odd_polynomial ] )
end

module TestDensifiedPolynomial = struct
  let test_vectors () =
    let x = F379.random () in
    let zero = F379.zero in
    let test_vectors =
      [ (Poly.zero, [F379.zero]);
        (Poly.constants x, [x]);
        (Poly.of_coefficients [(x, 2)], [x; zero; zero]);
        (Poly.of_coefficients [(x, 1)], [x; zero]);
        (Poly.of_coefficients [(x, 3); (x, 1)], [x; zero; x; zero]);
        (Poly.of_coefficients [(x, 4); (x, 1)], [x; zero; zero; x; zero]);
        ( Poly.of_coefficients [(x, 17); (x, 14); (x, 3); (x, 1); (x, 0)],
          [ x;
            zero;
            zero;
            x;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            zero;
            x;
            zero;
            x;
            x ] ) ]
    in
    List.iter
      (fun (v, expected_result) ->
        assert (expected_result = Poly.get_dense_polynomial_coefficients v))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( "Get dense polynomial coefficients",
      [test_case "Test vectors" `Quick test_vectors] )
end

module TestFFT = struct
  let test_evaluation_fft_vectors () =
    let module F337 = Ff.MakeFp (struct
      let prime_order = Z.of_string "337"
    end) in
    let module Poly = Polynomial.Make (F337) in
    let test_vectors =
      [ ( Poly.of_coefficients
            [ (F337.of_string "6", 7);
              (F337.of_string "2", 6);
              (F337.of_string "9", 5);
              (F337.of_string "5", 4);
              (F337.of_string "1", 3);
              (F337.of_string "4", 2);
              (F337.of_string "1", 1);
              (F337.of_string "3", 0) ],
          F337.of_string "85",
          Z.of_string "8",
          [ F337.of_string "31";
            F337.of_string "70";
            F337.of_string "109";
            F337.of_string "74";
            F337.of_string "334";
            F337.of_string "181";
            F337.of_string "232";
            F337.of_string "4" ] ) ]
    in
    List.iter
      (fun (polynomial, generator, power, expected_result) ->
        let res = Poly.evaluation_fft polynomial ~generator ~power in
        assert (res = expected_result))
      test_vectors

  let test_evaluation_random_values_against_normal_evaluation () =
    let module F337 = Ff.MakeFp (struct
      let prime_order = Z.of_string "337"
    end) in
    let module Poly = Polynomial.Make (F337) in
    (* to keep 85 in F337 *)
    let degree = 8 in
    (* generate some evaluation points, not twice the same *)
    let rec generate_evaluation_points i n acc =
      if i < n then
        let r = F337.random () in
        if List.mem r acc then generate_evaluation_points i n acc
        else generate_evaluation_points (i + 1) n (r :: acc)
      else acc
    in
    let evaluation_points = generate_evaluation_points 0 degree [] in
    let polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural degree)
    in
    let expected_results =
      List.map (fun x -> Poly.evaluation polynomial x) evaluation_points
    in
    let results =
      Poly.evaluation_fft
        ~generator:(F337.of_string "85")
        ~power:(Z.of_int degree)
        polynomial
    in
    assert (expected_results = results)

  let get_tests () =
    let open Alcotest in
    ( "FFT",
      [ test_case
          "test vectors for evaluation"
          `Quick
          test_evaluation_fft_vectors;
        test_case
          "test evaluation at random points"
          `Quick
          (repeat 1000 test_evaluation_fft_vectors) ] )
end

module TestInverseFFT = struct
  module F337 = Ff.MakeFp (struct
    let prime_order = Z.of_string "337"
  end)

  module Poly = Polynomial.Make (F337)

  let nth_root_of_unity = F337.of_string "85"

  let power = Z.of_string "8"

  let test_interpolation_fft_vectors () =
    let test_vectors =
      [ ( [ F337.of_string "31";
            F337.of_string "70";
            F337.of_string "109";
            F337.of_string "74";
            F337.of_string "334";
            F337.of_string "181";
            F337.of_string "232";
            F337.of_string "4" ],
          Poly.of_coefficients
            [ (F337.of_string "6", 7);
              (F337.of_string "2", 6);
              (F337.of_string "9", 5);
              (F337.of_string "5", 4);
              (F337.of_string "1", 3);
              (F337.of_string "4", 2);
              (F337.of_string "1", 1);
              (F337.of_string "3", 0) ] ) ]
    in
    List.iter
      (fun (points, expected_polynomial) ->
        let res =
          Poly.interpolation_fft ~generator:nth_root_of_unity ~power points
        in
        print_endline (Poly.to_string expected_polynomial) ;
        print_endline (Poly.to_string res) ;
        assert (Poly.equal res expected_polynomial))
      test_vectors

  let test_interpolation_fft_random_values_against_lagrange_interpolation () =
    let random_polynomial =
      Poly.generate_random_polynomial (Polynomial.Natural (Z.to_int power - 1))
    in
    let evaluation_points =
      Poly.get_dense_polynomial_coefficients random_polynomial
    in
    let domain =
      List.init (Z.to_int power) (fun i ->
          F337.pow nth_root_of_unity (Z.of_int i))
    in
    let expected_results =
      Poly.lagrange_interpolation (List.combine domain evaluation_points)
    in
    let results =
      Poly.interpolation_fft
        ~generator:nth_root_of_unity
        ~power
        evaluation_points
    in
    assert (Poly.equal results expected_results)

  let get_tests () =
    let open Alcotest in
    ( "Inverse FFT",
      [ test_case
          "test vectors for interpolation fft"
          `Quick
          test_interpolation_fft_vectors;
        test_case
          "test interpolation at random points"
          `Quick
          (repeat
             1
             test_interpolation_fft_random_values_against_lagrange_interpolation)
      ] )
end

module TestPolynomialMultiplicationFFT = struct
  module F337 = Ff.MakeFp (struct
    let prime_order = Z.of_string "337"
  end)

  module Poly = Polynomial.Make (F337)

  let generator = F337.of_string "85"

  let power = Z.of_string "8"

  let test_vectors () =
    let vectors =
      [ ( Poly.zero,
          Poly.generate_random_polynomial (Polynomial.Natural 1000),
          Poly.zero );
        ( Poly.generate_random_polynomial (Polynomial.Natural 100),
          Poly.zero,
          Poly.zero );
        ( Poly.zero,
          Poly.generate_random_polynomial (Polynomial.Natural 1000),
          Poly.zero );
        ( Poly.of_coefficients
            [ (F337.of_string "3", 3);
              (F337.of_string "2", 2);
              (F337.of_string "1", 1);
              (F337.of_string "1", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "3", 3);
              (F337.of_string "2", 2);
              (F337.of_string "1", 1);
              (F337.of_string "1", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "9", 6);
              (F337.of_string "12", 5);
              (F337.of_string "10", 4);
              (F337.of_string "10", 3);
              (F337.of_string "5", 2);
              (F337.of_string "2", 1);
              (F337.of_string "1", 0) ] );
        ( Poly.of_coefficients
            [ (F337.of_string "23", 3);
              (F337.of_string "35", 2);
              (F337.of_string "213", 1);
              (F337.of_string "32", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "121", 3);
              (F337.of_string "43", 2);
              (F337.of_string "56", 1);
              (F337.of_string "82", 0) ],
          Poly.of_coefficients
            [ (F337.of_string "87", 6);
              (F337.of_string "169", 5);
              (F337.of_string "258", 4);
              (F337.of_string "27", 3);
              (F337.of_string "335", 2);
              (F337.of_string "49", 1);
              (F337.of_string "265", 0) ] ) ]
    in
    List.iter
      (fun (p, q, expected_result) ->
        assert (
          let res = Poly.polynomial_multiplication_fft ~generator ~power p q in
          print_endline (Poly.to_string res) ;
          Poly.equal expected_result res ))
      vectors

  let get_tests () =
    let open Alcotest in
    ( "Polynomial multiplication FFT",
      [ test_case
          "test vectors for polynomial multiplication"
          `Quick
          test_vectors ] )
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
      TestSplitPolynomial.get_tests ();
      TestDensifiedPolynomial.get_tests ();
      TestInverseFFT.get_tests ();
      TestPolynomialMultiplicationFFT.get_tests ();
      TestFFT.get_tests ();
      TestAdd.get_tests () ]
