(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites : unit Alcotest.test list =
  [
    ("Test Raytracer - Tuple", Test_raytracer.Test_tuple.tests);
    ("Test Raytracer - Canvas", Test_raytracer.Test_canvas.tests);
    ("Test Raytracer - Matrix", Test_raytracer.Test_matrix.tests);
  ]

let () = Alcotest.run "proj" test_suites
