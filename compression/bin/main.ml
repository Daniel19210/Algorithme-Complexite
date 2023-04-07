(*let image = "../images/femme3.jpeg" in*)
let image = "../images/homme1.jpeg" in (* Image qui a plus de colonne que de ligne *)
(*let image = "../images/homme3.jpeg" in (* Image qui a plus de ligne que de colonne*)*)
(*let image = "../images/homme_gris.jpeg" in*)
(*let image = "../images/femme3_gris.jpeg" in*)
let taux_compression = 0.1 in
let array_image = Graphic_image.array_of_image (Jpeg.load image []) in
let (array_image_red, array_image_green, array_image_blue) = Affichage.get_colors array_image in
let array_rang_ratio = [| 0; 0 |] in

let image_red_compresse = Compression.convert_array_float_to_int (Compression.make_compression (Compression.convert_array_int_to_float array_image_red) taux_compression array_rang_ratio) in
let image_green_compresse = Compression.convert_array_float_to_int (Compression.make_compression (Compression.convert_array_int_to_float array_image_green) taux_compression array_rang_ratio) in
let image_blue_compresse = Compression.convert_array_float_to_int (Compression.make_compression (Compression.convert_array_int_to_float array_image_blue) taux_compression array_rang_ratio) in
let image_compresse = Affichage.assign_value image_red_compresse image_green_compresse image_blue_compresse in

Printf.printf "Le rang de la matrice est : %d\n" array_rang_ratio.(0);
Printf.printf "Ratio de compression : %d%%\n" array_rang_ratio.(1);
Affichage.compare_result image image_compresse;;
