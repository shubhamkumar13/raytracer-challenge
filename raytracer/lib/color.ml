type color = { red : float; green : float; blue : float }

let black = { red = 0.; green = 0.; blue = 0. }

let white = { red = 1.; green = 1.; blue = 1. }

let color : float * float * float -> color =
 fun (r, g, b) -> { red = r; green = g; blue = b }

let scalar_to_color : float -> color = fun a -> { red = a; green = a; blue = a }

let map : (float -> float -> float) -> color -> color -> color =
 fun f a b ->
  let red = f a.red b.red in
  let green = f a.green b.green in
  let blue = f a.blue b.blue in
  { red; green; blue }

let equal : color -> color -> bool =
 fun a b ->
  let red_diff = Float.equal a.red b.red in
  let green_diff = Float.equal a.green b.green in
  let blue_diff = Float.equal a.blue b.blue in
  red_diff && green_diff && blue_diff

let add : color -> color -> color = fun a b -> map Float.add a b

let subtract : color -> color -> color = fun a b -> map Float.sub a b

let scalar_mul : color -> float -> color =
 fun a c -> map Float.mul a (scalar_to_color c)

let mul : color -> color -> color = fun a b -> map Float.mul a b
