let () = Random.self_init ()

let test_next_power_of_two () =
  let vectors = [(2, 2); (3, 4); (7, 8); (12, 16)] in
  List.iter
    (fun (x, expected_result) ->
      let res = Utils.next_power_of_two x in
      if res != expected_result then
        Alcotest.failf
          "Expected result is %d (for %d), got %d"
          expected_result
          x
          res)
    vectors

let test_reorg_polynomial_is_same_than_reorg_coefficients () =
  let module Fp = Ff.MakeFp (struct
    let prime_order = Z.of_string "379"
  end) in
  let module Poly = Polynomial.MakeUnivariate (Fp) in
  let logn = Random.int 16 in
  let n = Z.pow Z.(one + one) logn in
  let n = Z.to_int n in
  let degree = Polynomial.Natural n in
  let random_polynomial = Poly.generate_random_polynomial degree in
  let coefficients =
    Array.of_list @@ Poly.get_dense_polynomial_coefficients random_polynomial
  in
  Utils.reorg_coefficients n logn coefficients ;
  let coefficients = Array.to_list coefficients in
  let reorganized_polynomial =
    Utils.reorg_sparse_polynomial
      logn
      (Poly.get_list_coefficients random_polynomial)
  in
  let reorganized_polynomial = List.map fst reorganized_polynomial in
  if coefficients != reorganized_polynomial then
    Alcotest.failf
      "Reorganized coefficients [%s]\nReorganized polynomial: [%s]\n"
      (String.concat "; " (List.map Fp.to_string coefficients))
      (String.concat "; " (List.map Fp.to_string reorganized_polynomial))

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "Reorganizing the coefficients",
        [ test_case
            "Reorg polynomial vs reorg coefficients"
            `Quick
            test_reorg_polynomial_is_same_than_reorg_coefficients;
          test_case "Next power of two" `Quick test_next_power_of_two ] ) ]
