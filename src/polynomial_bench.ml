type 'a evaluation_domain = { generator : 'a; n : int }

let get_time () = Sys.time ()

module MakeBench
    (Scalar : Ff_sig.PRIME)
    (Poly : Polynomial_sig.UNIVARIATE with type scalar = Scalar.t) =
struct
  let compute_interpolation_fft_with_generator degree generator () =
    let points = List.init degree (fun _i -> Scalar.random ()) in
    let domain =
      Polynomial.generate_evaluation_domain (module Scalar) degree generator
    in
    let start = get_time () in
    ignore @@ Poly.interpolation_fft ~domain points ;
    let end_time = get_time () in
    Printf.printf
      "Time for FFT interpolation n = %d for one execution is %fs\n"
      degree
      (end_time -. start)

  let compute_euclidian_division deg1 deg2 () =
    let poly1 = Poly.generate_random_polynomial (Polynomial_sig.Natural deg1) in
    let poly2 = Poly.generate_random_polynomial (Polynomial_sig.Natural deg2) in
    let start = get_time () in
    ignore @@ Poly.euclidian_division_opt poly1 poly2 ;
    let end_time = get_time () in
    Printf.printf
      "Time for euclidian division with deg1 = %d and deg2 = %d for one \
       execution is %fs\n"
      deg1
      deg2
      (end_time -. start)

  let get_benches domain =
    [compute_interpolation_fft_with_generator domain.n domain.generator]
end
