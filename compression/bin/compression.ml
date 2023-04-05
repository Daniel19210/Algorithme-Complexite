open Gsl;;
open Printf;;

let print_2D_array array =
    let rec print_2D_array_aux array i =
        if (i == (Array.length array)-1) then(
            Array.iter (printf "%.1f ") array.(i); print_newline ()
        )
        else(
            Array.iter (printf "%.1f ") array.(i); print_newline ();
            print_2D_array_aux array (i+1)
        )
    in print_2D_array_aux array 0;;

let convert_array_float_to_int array_float =
    Array.map (fun ligne -> Array.map int_of_float ligne) array_float;;

let convert_array_int_to_float array_int =
    Array.map (fun ligne -> Array.map float_of_int ligne) array_int;;

(* Ajoute autant de ligne que nécessaire pour avoir une matrice carée *)
let pad_matrix matrix =
    let (nb_row, nb_column) = Matrix.dims matrix in
    let array_matrix = Matrix.to_arrays matrix in
    let pad = Array.make_matrix (nb_column - nb_row) nb_column 0. in
    let padded_array_matrix = Array.append array_matrix pad in
    Matrix.of_arrays padded_array_matrix;;

(* Effectue la Sgd avec le librairie Gsl
    Prend en entrée une Matrix(M,N) et renvoie un tuple
    (Array(MxN), Array(N), Array(NxN))
 *)
let exec_sgd matrix =
    let (_, nb_column) = Matrix.dims matrix in
    let v = Matrix.create ?init:(Some 0.) nb_column nb_column in (* NxN *)
    let s = Vector.create ?init:(Some 0.) nb_column in (* 1xN *)
    let work = Vector.create ?init:(Some 0.) nb_column in (* 1xN *)

    let vecMat_u = Vectmat.mat_convert (`M (Matrix.copy matrix)) in (* MxN *)
    let vecMat_v = Vectmat.mat_convert (`M v) in
    let vecMat_s = Vectmat.vec_convert (`V s) in
    let vecMat_work = Vectmat.vec_convert (`V work) in

    Linalg._SV_decomp ~a:vecMat_u ~v:vecMat_v ~s:vecMat_s ~work:vecMat_work; (* Renvoie la matrice v, pas la transposée*)
    (Vectmat.to_arrays vecMat_u), (Vectmat.to_array vecMat_s), (Vectmat.to_arrays vecMat_v) ;;

(* Effectue le compression du tableau de taille MxN selon le taux_compression (un pourcentage)
   Renvoie une matrie MxC avec C = N*taux_compression
 *)
let make_compression array taux_compression =
    let matrix = Matrix.of_arrays array in
    let nb_row, nb_column = Matrix.dims matrix in
    let nb_column_compressed = int_of_float ((float_of_int (Array.length array.(0))) *. taux_compression) in
    let m = if (nb_row < nb_column) then (pad_matrix matrix) else matrix in (* Padding dans le cas où l'image est en mode portrait (nb_ligne<nb_colonne)*)

    let (arrays_u, array_s, arrays_v) = exec_sgd m in (* On récupère les matrices après la SGD*)

    let vecMat_u_compressed = Vectmat.mat_convert (`M (Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_column_compressed) arrays_u))) in (* Compression en matrice de taille MxC*)
    (* Création de la matrice compressée qui aura les valeurs singulière sur la diagonale*)
    let array_s_compressed = Array.sub array_s 0 nb_column_compressed in (* Compression du tableau des valeurs singulières *)
    let vecMat_s_compressed_array = Matrix.to_arrays (Matrix.create ?init:(Some 0.) nb_column_compressed nb_column_compressed) in (* CxC *)
    Array.iteri (fun i valeur_singuliere -> vecMat_s_compressed_array.(i).(i) <- valeur_singuliere) array_s_compressed; (* assignation de valeur dans la diagonale *)
    let vecMat_s_compressed = Vectmat.mat_convert (`M (Matrix.of_arrays vecMat_s_compressed_array)) in

    let mat_v_compressed = Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_column_compressed) arrays_v) in (* Compression en matrice de taille NxC *)
    let mat_vT_compressed = Matrix.create nb_column_compressed nb_row in (* Création de la matrice transposée CxN *)
    Matrix.transpose mat_vT_compressed mat_v_compressed;
    let vecMat_vT_compressed = Vectmat.mat_convert (`M mat_vT_compressed) in

    let matrix_inter = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_column_compressed)) in (* MxC *)
    let matrix_res = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_column)) in (* MxN *)

    Linalg.matmult ~a:vecMat_u_compressed ~b:vecMat_s_compressed matrix_inter; (* MxC *. CxC *)
    Linalg.matmult ~a:matrix_inter ~b:vecMat_vT_compressed matrix_res; (* MxC *. CxN *)
    Vectmat.to_arrays matrix_res;;
