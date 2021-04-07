open Raytracer

let a = Matrix.create (3, 3)

let _ = Matrix.add a (0, 0) 3.

let _ = Matrix.add a (0, 1) 5.

let _ = Matrix.add a (0, 2) 0.

let _ = Matrix.add a (1, 0) 2.

let _ = Matrix.add a (1, 1) (-1.)

let _ = Matrix.add a (1, 2) (-7.)

let _ = Matrix.add a (2, 0) 6.

let _ = Matrix.add a (2, 1) (-1.)

let _ = Matrix.add a (2, 2) 5.

let _ = Printf.printf "%f\n" (Matrix.det @@ Matrix.submatrix a 1 0)
