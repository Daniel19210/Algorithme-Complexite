(*open Gsl;;*)

(*Photo.show_image "../images/garcon.jpeg";;*)

let image_path = "../images/garcon.jpeg" in
let taux_compression = 0.5 in
let array_image = (Photo.open_image_as_array image_path) in
let array_image_float = Array.map (fun ligne -> Array.map float_of_int ligne) array_image in
let u, sigma, v = Svd.make_sgd array_image_float in
let image_compresse = Svd.make_compression taux_compression u sigma v in
let image_compresse_int = Array.map (fun ligne -> Array.map int_of_float ligne) image_compresse in


Graphics.open_graph "1920x1200";
(*let height = Array.length image_compresse.(0) in*)
let graphe_image_compresse = Graphics.make_image (Array.map (Array.map (fun pixel -> Graphics.rgb (pixel lsr 16 land 0xff) (pixel lsr 8 land 0xff) (pixel land 0xff))) image_compresse_int) in
let graphe_image = Photo.open_image_as_graph image_path in

Graphics.draw_image graphe_image 0 0;
Graphics.draw_image graphe_image_compresse (Array.length image_compresse) 0;
Photo.interactive();;

(*
Array.iter (fun ligne -> Array.iter (fun valeur -> Printf.printf "#%X " (valeur land 0xffffff)) ligne; print_newline())array_image;print_newline();;
let color = array_image.(0).(0) in
let red = (color lsr 16) land 0xFF in
let green = (color lsr 8) land 0xFF in
let blue = color land 0xFF in
Printf.printf "La valeur en hexadecimal de la couleur %d est : #%X%X%X\n" color red green blue
*)
(*
(*Les matrices doivent avoir plus de ligne que de colonne pour ne pas être modifiée*)
let m1 = Gsl.Matrix.of_arrays [|
    [| 1.; 0.; 0.; 0.; 2. |];
    [| 0.; 0.; 3.; 0.; 0. |];
    [| 0.; 0.; 0.; 0.; 0. |];
    [| 0.; 4.; 0.; 0.; 0. |];
|] in
*)
(*
match (make_sgd_padding m1) with
    | a, b, c -> print_string("m1 = \n");show_svd (Matrix.to_arrays m1) a b c;
*)
(*
let m2 = Matrix.of_arrays [|
    [| 1.; 0.; 0.; 0.; 2. |];
    [| 0.; 0.; 3.; 0.; 0. |];
    [| 0.; 0.; 0.; 0.; 0. |];
    [| 0.; 2.; 0.; 0.; 0. |];
|] in

*)
(*
let m3 = [|
    [| 0.; 1.; 0.; 0. |];
    [| 0.; 0.; 2.; 0. |];
    [| 0.; 0.; 0.; 3. |];
    [| 0.; 0.; 0.; 0. |];
|] in
let (u, sigma, v) = Svd.make_sgd m3 in
let m3_compresse = Svd.make_compression 0.5 u sigma v in
Svd.print_2D_array m3_compresse;;
*)
(*
print_newline ();
print_string("m2 = \n");print_2D_array (Matrix.to_arrays m2) 0;
match (make_sgd_padding m2) with
    | a, b, c -> show_svd a b c;

print_newline ();
print_string("m3 = \n");print_2D_array (Matrix.to_arrays m3) 0;
match (make_sgd_padding m3) with
    | a, b, c -> show_svd a b c;
*)
