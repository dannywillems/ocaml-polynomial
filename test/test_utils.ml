let () = Random.self_init ()

let test_bitreverse () =
  let test_vectors =
    [ (0, 0);
      (1, 8);
      (2, 4);
      (3, 12);
      (4, 2);
      (5, 10);
      (6, 6);
      (7, 14);
      (8, 1);
      (9, 9);
      (10, 5);
      (11, 13);
      (12, 3);
      (13, 11);
      (14, 7);
      (15, 15) ]
  in
  let logn = 4 in
  List.iter
    (fun (l, expected_output) ->
      let output = Utils.bitreverse l logn in
      assert (output = expected_output))
    test_vectors

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

let () =
  let open Alcotest in
  run
    "Utils"
    [ ( "Reorganizing the coefficients",
        [ test_case "Next power of two" `Quick test_next_power_of_two;
          test_case "bitreverse unit test" `Quick test_bitreverse ] ) ]
