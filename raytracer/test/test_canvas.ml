let remove_space_and_newline ?(remove_ppm_header = false) ?(substring = 7) s =
  String.to_seq s |> List.of_seq
  |> List.map (fun c -> Printf.sprintf "%c" c)
  |> List.filter (fun s -> not (s = " " || s = "\n"))
  |> List.map (fun s -> String.get s 0)
  |> List.to_seq |> String.of_seq
  |> fun s ->
  if remove_ppm_header then String.sub s substring (String.length s - substring)
  else s

let pixel_test () =
  Alcotest.(check bool)
    "Checking whether the pixel written is the same or not" true
    (let c = Raytracer.Canvas.build (10, 20) in
     let red = Raytracer.Color.color (1., 0., 0.) in
     let _ = Raytracer.Canvas.write_pixel c (2, 3) red in
     Raytracer.Canvas.pixel_at c (2, 3) = red)

let ppm_pixel_data () =
  let c = Raytracer.Canvas.build (5, 3) in
  let c1 = Raytracer.Color.color (1.5, 0., 0.) in
  let c2 = Raytracer.Color.color (0., 0.5, 0.) in
  let c3 = Raytracer.Color.color (-0.5, 0., 1.) in
  let _ = Raytracer.Canvas.write_pixel c (0, 0) c1 in
  let _ = Raytracer.Canvas.write_pixel c (2, 1) c2 in
  let _ = Raytracer.Canvas.write_pixel c (4, 2) c3 in
  let ppm =
    Raytracer.Canvas.canvas_to_ppm c |> fun s ->
    remove_space_and_newline ~remove_ppm_header:true s
  in
  let test =
    "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n\
    \        0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n\
    \        0 0 0 0 0 0 0 0 0 0 0 0 0 0 255" |> remove_space_and_newline
  in
  Alcotest.(check bool)
    "Constructing the ppm pixel data" true (String.equal ppm test)

let split_long_lines_in_ppm_files () =
  let c = Raytracer.Canvas.build (10, 2) in
  let color = Raytracer.Color.color (1., 0.8, 0.6) in
  let _ =
    Array.iteri
      (fun i arr -> Array.iteri (fun j _ -> c.pixels.(i).(j) <- color) arr)
      c.pixels
  in
  let ppm =
    Raytracer.Canvas.canvas_to_ppm c |> fun s ->
    remove_space_and_newline ~remove_ppm_header:true ~substring:8 s
  in
  let test =
    "255 204 153 255 204 153 255 204 153 255 204\n\
    \ 153\n\
    \ 255 204 153 255 204\n\
     153 255 204 153 255 204 153 255 204 153 255\n\
    \ 204\n\
    \ 153\n\
     255 204 153 255 204 153 255 204 153 255 204\n\
    \ 153\n\
    \ 255 204 153 255 204\n\
     153 255 204 153 255 204 153 255 204 153 255\n\
    \ 204\n\
    \ 153\n" |> remove_space_and_newline
  in
  Alcotest.(check bool)
    "Splitting long lines in PPM files" true (String.equal ppm test)

let terminated_by_newline () =
  let c = Raytracer.Canvas.build (5, 3) in
  let ppm = Raytracer.Canvas.canvas_to_ppm c in
  Alcotest.(check char)
    "PPM files are terminated by a newline character" '\n'
    (String.get ppm (String.length ppm - 1))

let tests =
  [
    ("Writing Pixel to a canvas", `Quick, pixel_test);
    ("Constructing the PPM pixel data", `Quick, ppm_pixel_data);
    ("Splitting long lines in PPM files", `Quick, split_long_lines_in_ppm_files);
    ( "PPM files are terminated by a newline character",
      `Quick,
      terminated_by_newline );
  ]
