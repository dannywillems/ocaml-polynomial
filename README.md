# ocaml-polynomial

Play with polynomial

## Examples

Let's have an example with fields
```shell
opam switch create . 4.09.1
opam install . -y --with-test --deps-only
opam install . -y
opam install utop
dune utop
```

```ocaml
#require "ff";
module F379 = Ff.MakeFp(struct let prime_order = Z.of_int 379 end);;
module Poly379 = Polynomial.Make(F379);;
let points =
  [ (F379.of_string "2", F379.of_string "3");
    (F379.of_string "0", F379.of_string "1") ]
in
let interpolated_polynome = Poly.lagrange_interpolation points in
assert (
  Poly.equal
    (Poly.of_coefficients
       [ (F379.of_string "1", 1);
         (F379.of_string "1", 0) ])
    interpolated_polynome )

```
