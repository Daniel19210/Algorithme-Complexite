print_string "\n";;

open Gsl;;
open Printf;;

let rec print_2D_array array i =
    if (i == (Array.length array)-1) then(
        Array.iter (printf "%.2f ") array.(i); print_newline ()
    )
    else(
        Array.iter (printf "%.2f ") array.(i); print_newline ();
        print_2D_array array (i+1)
    );;
(*
let inv i = -.i ;;

let rec inverse_arr array i =
    if (i == (Array.length array)-1) then(
        array.(i) <- Array.map inv array.(i);
        array
    )
    else(
        array.(i) <- Array.map inv array.(i);
        inverse_arr array (i+1)
    );;
*)

let show_svd matrix u s v =
    print_string "a =\n";print_2D_array (Matrix.to_arrays matrix) 0; print_newline();
    print_string("U = \n");print_2D_array u 0; print_string("\n");
    print_string("sigma = \n");Array.iter (printf "%f ") s; print_string("\n\n");
    print_string("V = \n"); print_2D_array v 0; print_string("\n");;


let pad_matrix matrix =
    let (nb_ligne, nb_colonne) = Matrix.dims matrix in
    let array_matrix = Matrix.to_arrays matrix in
    let pad = Array.make_matrix (nb_colonne - nb_ligne) nb_colonne 0. in
    let padded_array_matrix = Array.append array_matrix pad in
    Matrix.of_arrays padded_array_matrix;;


let make_sgd_padding matrix = 
    let nb_ligne, nb_colonne = Matrix.dims matrix in
    let make_sgd matrix nb_colonne =
        let v = Matrix.create ?init:(Some 0.) nb_colonne nb_colonne in
        let s = Vector.create ?init:(Some 0.) nb_colonne in
        let work = Vector.create ?init:(Some 0.) nb_colonne in

        let vecMat_u = Vectmat.mat_convert (`M matrix) in
        let vecMat_v = Vectmat.mat_convert (`M v) in
        let vecMat_s = Vectmat.vec_convert (`V s) in
        let vecMat_work = Vectmat.vec_convert (`V work) in

        Linalg._SV_decomp ~a:vecMat_u ~v:vecMat_v ~s:vecMat_s ~work:vecMat_work;

        show_svd matrix (Vectmat.to_arrays vecMat_u) (Vectmat.to_array vecMat_s) (Vectmat.to_arrays vecMat_v)
    in
    if (nb_ligne >= nb_colonne) then
        make_sgd matrix nb_colonne
    else
        make_sgd (pad_matrix matrix) nb_colonne;;


let a = Matrix.of_arrays [|
    [| 1.; 0.; 0.; 0.; 2. |];
    [| 0.; 0.; 3.; 0.; 0. |];
    [| 0.; 0.; 0.; 0.; 0. |];
    [| 0.; 4.; 0.; 0.; 0. |];
|] in

make_sgd_padding a;;

(*let a_transpose = Matrix.create ?init:(Some 0.) nb_colonne nb_ligne in
Matrix.transpose a_transpose a;
print_string "transpose =\n"; print_2D_array (Matrix.to_arrays a_transpose) 0; print_string("\n");*)


(*let vecMat_v = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) 4 5)) in*)
(*Vectmat.transpose vecMat_v vecMat_v;*)

(*let array_v = inverse_arr (Vectmat.to_arrays vecMat_v) 0 in
let array_u = inverse_arr (Vectmat.to_arrays vecMat_u) 0 in*)

