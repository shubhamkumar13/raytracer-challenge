type tuple = { x : float; y : float; z : float; w : float }

let point : float * float * float -> tuple =
 fun (x, y, z) -> { x; y; z; w = 1.0 }

let vector : float * float * float -> tuple =
 fun (x, y, z) -> { x; y; z; w = 0.0 }

let origin = point (0., 0., 0.)

let is_vector : tuple -> bool =
 fun t -> match Float.compare t.w 0. with x when x = 0 -> true | _ -> false

let is_point : tuple -> bool =
 fun t -> match Float.compare t.w 1. with x when x = 0 -> true | _ -> false

let scalar_to_vector : float -> tuple = fun a -> { x = a; y = a; z = a; w = a }

let build : (float -> float -> float) -> tuple -> tuple -> tuple =
 fun f a b ->
  let x = f a.x b.x in
  let y = f a.y b.y in
  let z = f a.z b.z in
  let w = f a.w b.w in
  { x; y; z; w }

let epsilon = 0.00001

let equal : tuple -> tuple -> bool =
 fun a b ->
  build (fun a b -> Float.sub a b |> Float.abs) a b |> fun t ->
  let cmp : float -> float -> bool =
   fun a e -> match Float.compare a e with x when x <= 0 -> true | _ -> false
  in
  cmp t.x epsilon && cmp t.y epsilon && cmp t.z epsilon && cmp t.w epsilon

let add : tuple -> tuple -> tuple = fun a b -> build Float.add a b

let subtract : tuple -> tuple -> tuple = fun a b -> build Float.sub a b

let neg : tuple -> tuple = fun a -> build Float.sub origin a

let mults : tuple -> float -> tuple =
 fun a b -> build Float.mul a (scalar_to_vector b)

let divs : tuple -> float -> tuple =
 fun a b -> build Float.div a (scalar_to_vector b)

let mag : tuple -> float =
 fun a ->
  assert (is_vector a);
  build (fun a _ -> Float.mul a a) a origin |> fun t ->
  Float.sqrt @@ (t.x +. t.y +. t.z +. t.w)

let norm : tuple -> tuple =
 fun a ->
  assert (is_vector a);
  divs a (mag a)

let dot : tuple -> tuple -> float =
 fun a b ->
  assert (is_vector a);
  assert (is_vector b);
  build Float.mul a b |> fun { x; y; z; w } -> x +. y +. z +. w

let cross : tuple -> tuple -> tuple =
 fun a b ->
  assert (is_vector a);
  assert (is_vector b);
  vector
    ( (a.y *. b.z) -. (a.z *. b.y),
      (a.z *. b.x) -. (a.x *. b.z),
      (a.x *. b.y) -. (a.y *. b.x) )
