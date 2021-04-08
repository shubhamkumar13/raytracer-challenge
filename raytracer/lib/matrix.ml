module Float = struct
  include Float

  let equal a b = Float.abs @@ (a -. b) <= 0.001
end

type 'a matrix = 'a array array

let create : int * int -> float matrix = fun (r, c) -> Array.make_matrix r c 0.0

let is_square_matrix : float matrix -> bool =
 fun m -> Array.length m = Array.length m.(0)

let add : float matrix -> int * int -> float -> unit =
 fun m (i, j) v -> m.(i).(j) <- v

let print : float matrix -> unit =
 fun m ->
  Array.iter
    (fun ival ->
      Array.iter (fun jval -> Printf.printf "| %f " jval) ival;
      Printf.printf "|\n")
    m

let fmt : float matrix -> string =
 fun m ->
  let s = ref "" in
  Array.iter
    (fun ival ->
      Array.iter (fun jval -> s := !s ^ Printf.sprintf "| %f " jval) ival;
      s := !s ^ Printf.sprintf "|\n")
    m;
  !s

let equal : float matrix -> float matrix -> bool =
 fun m1 m2 ->
  assert (Array.length m1 = Array.length m2);
  assert (Array.length m1.(0) = Array.length m2.(0));
  let decision = ref true in
  for i = 0 to pred @@ Array.length m1 do
    for j = 0 to pred @@ Array.length m1.(0) do
      decision := !decision && Float.equal m1.(i).(j) m2.(i).(j);
    done
  done;
  !decision

let mult_m : float matrix -> float matrix -> float matrix =
 fun m1 m2 ->
  assert (Array.length m1.(0) = Array.length m2);
  let m3 = Array.make_matrix (Array.length m1) (Array.length m1.(0)) 0. in
  for i = 0 to pred @@ Array.length m1 do
    for j = 0 to pred @@ Array.length m1.(0) do
      for k = 0 to pred @@ Array.length m2.(0) do
        m3.(i).(j) <- m3.(i).(j) +. (m1.(i).(k) *. m2.(k).(j))
      done
    done
  done;
  m3

let mult_t : float matrix -> Tuple.tuple -> Tuple.tuple =
 fun m v ->
  assert (v.w = 1.);
  assert (Array.length m.(0) = 4);
  let x =
    (m.(0).(0) *. v.x)
    +. (m.(0).(1) *. v.y)
    +. (m.(0).(2) *. v.z)
    +. (m.(0).(3) *. v.w)
  in
  let y =
    (m.(1).(0) *. v.x)
    +. (m.(1).(1) *. v.y)
    +. (m.(1).(2) *. v.z)
    +. (m.(1).(3) *. v.w)
  in
  let z =
    (m.(2).(0) *. v.x)
    +. (m.(2).(1) *. v.y)
    +. (m.(2).(2) *. v.z)
    +. (m.(2).(3) *. v.w)
  in
  let w =
    (m.(3).(0) *. v.x)
    +. (m.(3).(1) *. v.y)
    +. (m.(3).(2) *. v.z)
    +. (m.(3).(3) *. v.w)
  in
  { x; y; z; w }

let identity_m : int -> float matrix =
 fun i ->
  Array.init i (fun r -> Array.init i (fun c -> if r = c then 1. else 0.))

let transpose : float matrix -> float matrix =
 fun m -> Array.mapi (fun r row -> Array.mapi (fun c _ -> m.(c).(r)) row) m

let submatrix : float matrix -> int -> int -> float matrix =
 fun m row col ->
  assert (is_square_matrix m);
  let m' = Array.make_matrix (Array.length m - 1) (Array.length m - 1) 0. in
  for r = 0 to pred @@ Array.length m do
    if r <> row then
      let r' = if r < row then r else r - 1 in
      for c = 0 to pred @@ Array.length m do
        if c <> col then
          let c' = if c < col then c else c - 1 in
          m'.(r').(c') <- m.(r).(c)
      done
  done;
  m'

let det : float matrix -> float =
 fun m ->
  let rec det' : float matrix -> float =
   fun m ->
    match m with
    | [| [| a; b |]; [| c; d |] |] -> (a *. d) -. (c *. b)
    | m ->
        let headers = m.(0) in
        Array.mapi
          (fun i ival ->
            if Int.rem i 2 = 0 then
              let x = ival *. det' (submatrix m 0 i) in
              (* Printf.printf "%f\t" x; *)
              x
            else
              let x = -1. *. ival *. det' (submatrix m 0 i) in
              (* Printf.printf "%f\t" x; *)
              x)
          headers
        |> fun m -> Array.fold_left (fun acc x -> acc +. x) 0. m
  in
  det' m

let minor : float matrix -> int -> int -> float =
 fun m row col -> submatrix m row col |> det

let cofactor : float matrix -> int -> int -> float =
 fun m row col ->
  if Int.rem (row + col) 2 = 0 then minor m row col else -1. *. minor m row col

let is_invertible : float matrix -> bool =
 fun m -> match det m with 0. -> false | _ -> true

let inverse : float matrix -> float matrix =
 fun m ->
  match is_invertible m with
  | false -> failwith "Matrix is not invertible"
  | _ ->
      let m2 = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
      let det_m = det m in
      for row = 0 to pred @@ Array.length m do
        for col = 0 to pred @@ Array.length m do
          let c = cofactor m row col in
          m2.(col).(row) <- c /. det_m
        done
      done;
      m2
