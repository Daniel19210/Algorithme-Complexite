(*let image = "../images/femme1.jpeg" in (* paysage *)*)
(*let image = "../images/femme2.jpeg" in (* carrée *)*)
(*let image = "../images/femme3.jpeg" in (* portrait *)*)
let image = "../images/homme1.jpeg" in (* paysage *)
(*let image = "../images/homme2.jpeg" in (* carrée *)*)
(*let image = "../images/homme3.jpeg" in (* portrait *)*)
let taux_compression = 0.5 in
let array_image = Graphic_image.array_of_image (Jpeg.load image []) in
let (array_image_red, array_image_green, array_image_blue) = Affichage.get_colors array_image in

let image_red_compresse = Compression.convert_array_float_to_int
    (Compression.make_compression
        (Compression.convert_array_int_to_float array_image_red) taux_compression true) in
let image_green_compresse = Compression.convert_array_float_to_int
    (Compression.make_compression
        (Compression.convert_array_int_to_float array_image_green) taux_compression false) in
let image_blue_compresse = Compression.convert_array_float_to_int
    (Compression.make_compression
        (Compression.convert_array_int_to_float array_image_blue) taux_compression false) in
let image_compresse = Affichage.assign_value image_red_compresse image_green_compresse image_blue_compresse in

Affichage.compare_result image image_compresse;;
