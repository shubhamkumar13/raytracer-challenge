(*
   Tests for Sub2.A
*)

let is_a_point () =
  Alcotest.(check bool)
    "A tuple with w=1.0 is a point" true
    (let p = Raytracer.Tuple.point (4.3, -4.2, 3.1) in
     Raytracer.Tuple.is_point p)

let is_not_a_vector () =
  Alcotest.(check bool)
    "A tuple with w=1.0 is not a vector" false
    (let p = Raytracer.Tuple.point (4.3, -4.2, 3.1) in
     Raytracer.Tuple.is_vector p)

let tests =
  [
    ("Check if this is a point", `Quick, is_a_point);
    ("Check if this not a vector", `Quick, is_not_a_vector);
  ]
