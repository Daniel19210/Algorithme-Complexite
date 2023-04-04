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

let show_svd array u s v =
    print_2D_array array;
    print_string("U = \n");print_2D_array u;
    print_string("sigma = \n");Array.iter (printf "%f ") s; print_string("\n");
    print_string("V = \n"); print_2D_array v;;

let make_sgd array =
    let matrix = Matrix.of_arrays array in
    let pad_matrix matrix =
        let (nb_row, nb_column) = Matrix.dims matrix in
        let array_matrix = Matrix.to_arrays matrix in
        let pad = Array.make_matrix (nb_column - nb_row) nb_column 0. in
        let padded_array_matrix = Array.append array_matrix pad in
        Matrix.of_arrays padded_array_matrix in

    let exec_sgd matrix nb_column =
        let v = Matrix.create ?init:(Some 0.) nb_column nb_column in
        let s = Vector.create ?init:(Some 0.) nb_column in
        let work = Vector.create ?init:(Some 0.) nb_column in

        let vecMat_u = Vectmat.mat_convert (`M (Matrix.copy matrix)) in
        let vecMat_v = Vectmat.mat_convert (`M v) in
        let vecMat_s = Vectmat.vec_convert (`V s) in
        let vecMat_work = Vectmat.vec_convert (`V work) in

        Linalg._SV_decomp ~a:vecMat_u ~v:vecMat_v ~s:vecMat_s ~work:vecMat_work;

        (* Permet d'afficher la transposée de V et non V*)
        let vecMat_vT = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_column nb_column)) in
        Vectmat.transpose vecMat_vT vecMat_v;
        ((Vectmat.to_arrays vecMat_u), (Vectmat.to_array vecMat_s), (Vectmat.to_arrays vecMat_vT)) in

    let nb_row, nb_column = Matrix.dims matrix in
    if (nb_row >= nb_column) then
        exec_sgd matrix nb_column
    else
        exec_sgd (pad_matrix matrix) nb_column;;

let make_compression taux_compression u sigma v_transposed =
    assert (taux_compression <= 1.);
    let nb_row = Array.length u in
    let size_v = Array.length v_transposed in
    let nb_column = int_of_float ((float_of_int (Array.length u.(0))) *. taux_compression) in
    let v = Matrix.create ?init:(Some 0.) size_v size_v in
    Matrix.transpose v (Matrix.of_arrays v_transposed);
    let compressed_u = Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_column) u) in
    let compressed_v = Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_column) (Matrix.to_arrays v)) in
    let compressed_v_transposed = Matrix.create ?init:(Some 0.) nb_column nb_row in
    Matrix.transpose compressed_v_transposed compressed_v;
    let matrix_inter = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_column)) in
    let matrix_res = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_column)) in
    let compressed_sigma_vector = Array.sub sigma 0 nb_column in
    let compressed_sigma_array = Array.make_matrix nb_row nb_column 0. in
    Array.iteri (fun i _ -> compressed_sigma_array.(i) <- compressed_sigma_vector) compressed_sigma_array;
    let compressed_sigma_matrix = Matrix.of_arrays compressed_sigma_array in
    printf "(nbLigne, nbColonne) = (%d, %d)\n" (fst (Matrix.dims compressed_u)) (snd (Matrix.dims compressed_u));
    (*Problème de dimension, regarder au niveau de sigma*)
    printf "(nbLigne, nbColonne) = (%d, %d)\n" (fst (Matrix.dims compressed_v_transposed)) (snd (Matrix.dims compressed_v_transposed));
    printf "(nbLigne, nbColonne) = (%d, %d)\n" (fst (Matrix.dims compressed_sigma_matrix)) (snd (Matrix.dims compressed_sigma_matrix));
    printf "(nbLigne, nbColonne) = (%d, %d)\n" (fst (Vectmat.dims matrix_inter)) (snd (Vectmat.dims matrix_inter));
    printf "(nbLigne, nbColonne) = (%d, %d)\n" (fst (Vectmat.dims matrix_res)) (snd (Vectmat.dims matrix_res));
    Linalg.matmult ~a:(Vectmat.mat_convert (`M compressed_u)) ~b:(Vectmat.mat_convert (`M compressed_sigma_matrix)) matrix_inter;
    Linalg.matmult ~a:matrix_inter ~b:(Vectmat.mat_convert (`M compressed_v_transposed)) matrix_res;
    (Vectmat.to_arrays matrix_res);;
