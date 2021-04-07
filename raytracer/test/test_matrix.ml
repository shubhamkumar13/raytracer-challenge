let test_four_by_four () =
  let test =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.5; 6.5; 7.5; 8.5 |];
      [| 9.; 10.; 11.; 12. |];
      [| 13.5; 14.5; 15.5; 16.5 |];
    |]
  in
  let t =
    test.(0).(0) = 1.
    && test.(0).(3) = 4.
    && test.(1).(0) = 5.5
    && test.(1).(2) = 7.5
    && test.(2).(2) = 11.
    && test.(3).(0) = 13.5
    && test.(3).(2) = 15.5
  in
  Alcotest.(check bool) "Constructing and inspecting a 4x4 matrix" true t

let test_two_by_two () =
  let test = [| [| -3.; 5. |]; [| 1.; -2. |] |] in
  let t =
    test.(0).(0) = -3.
    && test.(0).(1) = 5.
    && test.(1).(0) = 1.
    && test.(1).(1) = -2.
  in
  Alcotest.(check bool) "Constructing and inspecting a 2x2 matrix" true t

let test_three_by_three () =
  let test = [| [| -3.; 5.; 0. |]; [| 1.; -2.; -7. |]; [| 0.; 1.; 1. |] |] in
  let t = test.(0).(0) = -3. && test.(1).(1) = -2. && test.(2).(2) = 1. in
  Alcotest.(check bool) "Constructing and inspecting 3x3 matrix" true t

let test_equality_of_identical_matrices () =
  let m1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.; 6.; 7.; 8. |];
      [| 9.; 8.; 7.; 6. |];
      [| 5.; 4.; 3.; 2. |];
    |]
  in
  let m2 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.; 6.; 7.; 8. |];
      [| 9.; 8.; 7.; 6. |];
      [| 5.; 4.; 3.; 2. |];
    |]
  in
  let t = Raytracer.Matrix.equal m1 m2 in
  Alcotest.(check bool) "Matrix equality with identical matrices" true t

let test_equality_of_different_matrices () =
  let m1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.; 6.; 7.; 8. |];
      [| 9.; 8.; 7.; 6. |];
      [| 5.; 4.; 3.; 2. |];
    |]
  in
  let m2 =
    [|
      [| 2.; 3.; 4.; 5. |];
      [| 6.; 7.; 8.; 9. |];
      [| 8.; 7.; 6.; 5. |];
      [| 4.; 3.; 2.; 1. |];
    |]
  in
  let t = Raytracer.Matrix.equal m1 m2 in
  Alcotest.(check bool) "Matrix equality with identical matrices" false t

let test_matrix_multiplication () =
  let m1 =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 5.; 6.; 7.; 8. |];
      [| 9.; 8.; 7.; 6. |];
      [| 5.; 4.; 3.; 2. |];
    |]
  in
  let m2 =
    [|
      [| -2.; 1.; 2.; 3. |];
      [| 3.; 2.; 1.; -1. |];
      [| 4.; 3.; 6.; 5. |];
      [| 1.; 2.; 7.; 8. |];
    |]
  in
  let prod_m1_m2 =
    [|
      [| 20.; 22.; 50.; 48. |];
      [| 44.; 54.; 114.; 108. |];
      [| 40.; 58.; 110.; 102. |];
      [| 16.; 26.; 46.; 42. |];
    |]
  in
  let t =
    let m3 = Raytracer.Matrix.mult_m m1 m2 in
    Raytracer.Matrix.equal m3 prod_m1_m2
  in
  Alcotest.(check bool) "Test Multiplying two matrices" true t

let test_tuple_matrix_multiplication () =
  let m =
    [|
      [| 1.; 2.; 3.; 4. |];
      [| 2.; 4.; 4.; 2. |];
      [| 8.; 6.; 4.; 1. |];
      [| 0.; 0.; 0.; 1. |];
    |]
  in
  let p = Raytracer.Tuple.point (1., 2., 3.) in
  let prod_m_p = Raytracer.Matrix.mult_t m p in
  let t =
    Raytracer.Tuple.equal (Raytracer.Tuple.point (18., 24., 33.)) prod_m_p
  in
  Alcotest.(check bool) "A matrix multiplied by a tuple" true t

let test_matrix_identity_multiplication () =
  let m =
    [|
      [| 0.; 1.; 2.; 4. |];
      [| 1.; 2.; 4.; 8. |];
      [| 2.; 4.; 8.; 16. |];
      [| 4.; 8.; 16.; 32. |];
    |]
  in
  let a = Raytracer.Matrix.identity_m 4 in
  let t = Raytracer.Matrix.equal (Raytracer.Matrix.mult_m m a) m in
  Alcotest.(check bool) "Multiplying a matrix by the identity matrix" true t

let test_transposing_matrix () =
  let m =
    [|
      [| 0.; 9.; 3.; 0. |];
      [| 9.; 8.; 0.; 8. |];
      [| 1.; 8.; 5.; 3. |];
      [| 0.; 0.; 5.; 8. |];
    |]
  in
  let m' =
    [|
      [| 0.; 9.; 1.; 0. |];
      [| 9.; 8.; 8.; 0. |];
      [| 3.; 0.; 5.; 5. |];
      [| 0.; 8.; 3.; 8. |];
    |]
  in
  let t = Raytracer.Matrix.equal (Raytracer.Matrix.transpose m) m' in
  Alcotest.(check bool) "Transposing a matrix" true t

let test_transposing_identity_matrix () =
  let m = Raytracer.Matrix.transpose (Raytracer.Matrix.identity_m 4) in
  let m' = Raytracer.Matrix.identity_m 4 in
  let t = Raytracer.Matrix.equal m m' in
  Alcotest.(check bool) "Transposing an identity matrix" true t

let test_two_by_two_determinant () =
  let m = Raytracer.Matrix.create (2, 2) in
  let _ = Raytracer.Matrix.add m (0, 0) 1. in
  let _ = Raytracer.Matrix.add m (0, 1) 5. in
  let _ = Raytracer.Matrix.add m (1, 0) (-3.) in
  let _ = Raytracer.Matrix.add m (1, 1) 2. in
  Alcotest.(check bool)
    "Calculating the determinant of a 2x2 matrix" true
    (17. = Raytracer.Matrix.det m)

let test_submatrix_for_three_by_three () =
  let m = [| [| 1.; 5.; 0. |]; [| -3.; 2.; 7. |]; [| 0.; 6.; -3. |] |] in
  let m' = [| [| -3.; 2. |]; [| -0.; 6. |] |] in
  Alcotest.(check bool)
    " A submatrix of a 3x3 matrix is a 2x2 matrix" true
    (Raytracer.Matrix.equal m' (Raytracer.Matrix.submatrix m 0 2))

let test_submatrix_for_four_by_four () =
  let m =
    [|
      [| -6.; 1.; 1.; 6. |];
      [| -8.; 5.; 8.; 6. |];
      [| -1.; 0.; 8.; 2. |];
      [| -7.; 1.; -1.; 1. |];
    |]
  in
  let m' = [| [| -6.; 1.; 6. |]; [| -8.; 8.; 6. |]; [| -7.; -1.; 1. |] |] in
  Alcotest.(check bool)
    " A submatrix of a 4x4 matrix is a 3x3 matrix" true
    (Raytracer.Matrix.equal m' (Raytracer.Matrix.submatrix m 2 1))

let test_minor_for_three_by_three () =
  let m = [| [| 3.; 5.; 0. |]; [| 2.; -1.; -7. |]; [| 6.; -1.; 5. |] |] in
  let res = Raytracer.Matrix.submatrix m 1 0 |> Raytracer.Matrix.det in
  Alcotest.(check bool)
    " Calculating a minor of a 3x3 matrix" true
    (res = Raytracer.Matrix.minor m 1 0)

let test_determinant_for_three_by_three () =
  let m = [| [| 1.; 2.; 6. |]; [| -5.; 8.; -4. |]; [| 2.; 6.; 4. |] |] in
  let res1 = 1. *. Raytracer.Matrix.cofactor m 0 0 in
  let res2 = 2. *. Raytracer.Matrix.cofactor m 0 1 in
  let res3 = 6. *. Raytracer.Matrix.cofactor m 0 2 in
  let det = Raytracer.Matrix.det m in
  Alcotest.(check bool)
    "Calculating a determinant of a 3x3 matrix" true
    (( res1 +. res2 +. res3 ) = det )

let test_determinant_for_four_by_four () =
  let m = [| [|-2.; -8.; 3.; 5.|]; [| -3.; 1.; 7.; 3.|]; [| 1.; 2.; -9.; 6.|]; [|-6.; 7.; 7.; -9.|];|] in
  let res1 = -2. *. Raytracer.Matrix.cofactor m 0 0 in
  let res2 = -8. *. Raytracer.Matrix.cofactor m 0 1 in
  let res3 = 3. *. Raytracer.Matrix.cofactor m 0 2 in
  let res4 = 5. *. Raytracer.Matrix.cofactor m 0 3 in
  let det = Raytracer.Matrix.det m in
  Alcotest.(check bool)
    "Calculating the determinant of a 4x4 matrix" 
    true
    ((res1 +. res2 +. res3 +. res4) = det)

let test_invertible_matrix () =
  let m = [|
    [|6.; 4.; 4.; 4.|];
    [|5.; 5.; 7.; 6.|];
    [|4.; -9.; 3.; -7.|];
    [|9.; 1.; 7.; -6.|];
  |] in
  Alcotest.(check bool)
  "Testing an invertible matrix for invertibility"
  true
  (Raytracer.Matrix.is_invertible m)

let test_noninvertible_matrix () =
  let m = [|
    [|-4.; 2.; -2.; -3.|];
    [|9.; 6.; 2.; 6.|];
    [|0.; -5.; 1.; -5.|];
    [|0.; 0.; 0.; 0.|];
  |] in
  Alcotest.(check bool)
  "Testing a noninvertible matrix for invertibility"
  false
  (Raytracer.Matrix.is_invertible m)

let test_inverse_matrix () =
  let m1 = [|
    [| 8.; -5.; 9.; 2. |];
    [| 7.; 5.; 6.; 1. |];
    [| -6.; 0.; 9.; 6. |];
    [| -3.; 0.; -9.; -4. |];
  |] in
  let m2 = [|
    [| 9.; 3.; 0.; 9. |];
    [| -5.; -2.; -6.; -3. |];
    [| -4.; 9.; 6.; 4. |];
    [| -7.; 6.; 6.; 2. |];
  |] in
  let inv_m1 = [|
    [| -0.15385; -0.15385; -0.28205; -0.53846 |];
    [| -0.07692; 0.12308; 0.02564; 0.03077 |];
    [| 0.35897; 0.35897; 0.43590; 0.92308 |];
    [| -0.69231; -0.69231; -0.76923; -1.92308 |];
  |] in
  let inv_m2 = [|
    [| -0.04074; -0.07778; 0.14444; -0.22222 |];
    [| -0.07778; 0.03333; 0.36667; -0.33333 |];
    [| -0.02901; -0.14630; -0.10926; 0.12963 |];
    [| 0.17778; 0.06667; -0.26667; 0.33333 |];
  |] in
  Alcotest.(check bool)
  "Calculating the inverse of a matrix"
  true
  (Raytracer.Matrix.equal (Raytracer.Matrix.inverse m1) inv_m1 && Raytracer.Matrix.equal (Raytracer.Matrix.inverse m2) inv_m2)

let test_a_dot_a_inv () =
  let a = [|
    [||];
    [||];
    [||];
    [||];
  |] in

let tests =
  [
    ("Constructing and inspecting a 4x4 matrix", `Quick, test_four_by_four);
    ("Constructing and inspecting a 2x2 matrix", `Quick, test_four_by_four);
    ("Constructing and inspecting a 3x3 matrix", `Quick, test_three_by_three);
    ( "Matrix equality with identical matrices",
      `Quick,
      test_equality_of_identical_matrices );
    ( "Matrix equality with differen matrices",
      `Quick,
      test_equality_of_different_matrices );
    ("Multiplying two matrices", `Quick, test_matrix_multiplication);
    ("Multiplying matrix by a tuple", `Quick, test_tuple_matrix_multiplication);
    ( "Multiplying a matrix by the identity matrix",
      `Quick,
      test_matrix_identity_multiplication );
    ("Transposing a matrix", `Quick, test_transposing_matrix);
    ("Transposing an identity matrix", `Quick, test_transposing_identity_matrix);
    ( "Calculating the determinant of a 2x2 matrix",
      `Quick,
      test_two_by_two_determinant );
    ( "A submatrix of a 3x3 matrix is a 2x2 matrix",
      `Quick,
      test_submatrix_for_three_by_three );
    ( "A submatrix of a 4x4 matrix is a 3x3 matrix",
      `Quick,
      test_submatrix_for_four_by_four );
    ( "Calculating a minor of a 3x3 matrix",
      `Quick,
      test_minor_for_three_by_three );
    ( "Calculating the determinant of a 3x3 matrix",
      `Quick,
      test_determinant_for_three_by_three );
    ( "Calculating the determinant of a 4x4 matrix",
      `Quick,
      test_determinant_for_four_by_four );
    ( "Testing an invertible matrix for invertibility",
      `Quick,
      test_invertible_matrix);
    ( "Testing a noninvertible matrix for invertibility",
      `Quick,
      test_noninvertible_matrix);
    ( "Testing inverse for two 4x4 matrix",
      `Quick,
      test_inverse_matrix);
  ]
