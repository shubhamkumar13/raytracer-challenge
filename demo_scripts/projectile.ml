(* A lot of the stuff might not work from here the best way to run the script is to copy this code in main.ml and then run *)

type projectile = { position : Tuple.tuple; velocity : Tuple.tuple }

type enviroment = { gravity : Tuple.tuple; wind : Tuple.tuple }

let tick env proj =
  let position = Tuple.add proj.position proj.velocity in
  let velocity = Tuple.add proj.velocity env.gravity |> Tuple.add env.wind in
  { position; velocity }

(* let projectile =
  {
    position = Tuple.point (0., 1., 0.);
    velocity = Tuple.norm @@ Tuple.vector (2., 4., 0.);
  }

let enviroment =
  { gravity = Tuple.vector (0., -0.1, 0.); wind = Tuple.vector (-0.01, 0., 0.) } *)

let start = Tuple.point (0., 1., 0.)

let velocity = Tuple.mults (Tuple.norm @@ Tuple.vector (1., 1.8, 0.)) 11.25

let gravity = Tuple.vector (0., -0.1, 0.)

let wind = Tuple.vector (-0.01, 0., 0.)

let e = { gravity; wind }

let projectile = { position = start; velocity }

let c = Canvas.build (900, 500)

let c =
  Printf.printf "height = %d\n" @@ Array.length c.pixels;
  Printf.printf "width = %d\n" @@ Array.length c.pixels.(0);
  for j = 0 to pred @@ Array.length c.pixels do
    for i = 0 to pred @@ Array.length c.pixels.(0) do
      Canvas.write_pixel c (i, j) Color.black
    done
  done;
  c

let position () =
  let rec aux projectile =
    match Float.compare projectile.position.y 0.01 with
    | x when x <= 0 ->
        Canvas.canvas_to_ppm c |> fun c ->
        let oc = open_out "file.ppm" in
        Printf.fprintf oc "%s\n" c;
        close_out oc;
        ()
    | 1 ->
        let y = abs (c.height - int_of_float projectile.position.y) in
        let x = int_of_float projectile.position.x in
        let _ = Canvas.write_pixel c (x, y) Color.white in
        let projectile = tick e projectile in
        Printf.printf "(%d, %d)\n" x y;
        aux projectile
    | _ -> failwith "the loop isn't working"
  in
  aux projectile

let _ = position ()
