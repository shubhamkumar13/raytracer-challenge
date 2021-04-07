module C = Color

type canvas = { width : int; height : int; pixels : C.color array array }

let build : int * int -> canvas =
 fun (w, h) -> { width = w; height = h; pixels = Array.make_matrix h w C.black }

let write_pixel : canvas -> int * int -> C.color -> unit =
 fun c (x, y) color -> c.pixels.(y).(x) <- color

let pixel_at : canvas -> int * int -> C.color = fun c (x, y) -> c.pixels.(y).(x)

let ppm_header : canvas -> string =
 fun c -> Printf.sprintf "P3\n%d %d\n255\n" c.width c.height

let canvas_to_ppm : canvas -> string =
 fun c ->
  let color_value (color : C.color) : string * string * string =
    let scale x = max 0 (min 255 @@ int_of_float (0.5 +. (255. *. x))) in
    let red = Printf.sprintf "%d" @@ scale @@ color.red in
    let green = Printf.sprintf "%d" @@ scale @@ color.green in
    let blue = Printf.sprintf "%d" @@ scale @@ color.blue in
    (red, green, blue)
  in
  let buffer = Buffer.create ((c.width * c.height) + 1024) in
  let counter = ref 0 in
  Array.iter
    (fun ival ->
      Array.iter
        (fun jval ->
          let r, g, b = color_value jval in
          match !counter > 64 with
          | true ->
              (* Printf.printf "%d\n" !counter; *)
              counter := !counter - 70;
              Buffer.add_string buffer "\n";
              Buffer.add_string buffer r;
              Buffer.add_string buffer " ";
              Buffer.add_string buffer g;
              Buffer.add_string buffer " ";
              Buffer.add_string buffer b;
              Buffer.add_string buffer " ";
              counter := !counter + 6
          | _ ->
              (* Printf.printf "%d\n" !counter; *)
              Buffer.add_string buffer r;
              Buffer.add_string buffer " ";
              Buffer.add_string buffer g;
              Buffer.add_string buffer " ";
              Buffer.add_string buffer b;
              Buffer.add_string buffer " ";
              counter := !counter + 6)
        ival)
    c.pixels;
  ppm_header c ^ Buffer.contents buffer ^ "\n"
