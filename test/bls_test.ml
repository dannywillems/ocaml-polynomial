let rec repeat n = function
  | 0 -> []
  | i -> n :: (repeat n (i-1))

module BLSFr = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Poly = Polynomial.MakeUnivariate (BLSFr)


let main () =
  let g_string = "45578933624873246016802258050230213493140367389966312656957679049059636081617" in
  let generator = BLSFr.of_string g_string in
  let power = Z.to_int (Z.pow (Z.of_int 2) 16)  in

  let domain = Polynomial.generate_evaluation_domain (module BLSFr) power generator in

  let generate_polynomial () =
    let coefficients = List.init power (fun i -> (BLSFr.random (), i)) in
    let result_fft =
      Poly.evaluation_fft ~domain (Poly.of_coefficients coefficients)
    in
    (coefficients, result_fft)
  in

  let vectors = List.map generate_polynomial (repeat () 1) in

  let print_array p = Printf.sprintf "[|%s|]" (String.concat "; " (List.map (fun x -> Printf.sprintf "\"%s\"" x) p)) in
  let print_vector p res g n =
    Printf.sprintf "(%s, %s, \"%s\", %d)"
    (print_array (List.map (fun x -> BLSFr.to_string (fst x)) p))
    (print_array (List.map BLSFr.to_string res))
    g
    n
  in

  let vectors = List.map (fun (p, res) -> print_vector p res g_string power) vectors in

  let out = open_out "vectors_fft.ml" in
  Printf.fprintf out "let vectors = [ %s ]\n" (String.concat ";\n" vectors)

let () = main ()
