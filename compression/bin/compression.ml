open Gsl;;

let print_2D_array array =
    print_string "[ ";
    Array.iter (fun ligne ->
        Array.iter (fun el ->
            Printf.printf "%f " el
        ) ligne;
        print_endline ""
    ) array;
    print_string " ]\n";;

let convert_array_float_to_int array_float =
    Array.map (fun ligne -> Array.map int_of_float ligne) array_float;;

let convert_array_int_to_float array_int =
    Array.map (fun ligne -> Array.map float_of_int ligne) array_int;;


(* Effectue la Sgd avec le librairie Gsl
    Prend en entrée une Matrix(M,N) et renvoie un tuple
    (Array(MxN), Array(N), Array(NxN))
 *)
let exec_svd matrix =
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


(* Ajoute autant de ligne que nécessaire pour avoir une matrice carrée *)
let pad_matrix matrix nb_row nb_column=
    let array_matrix = Matrix.to_arrays matrix in
    let pad = Array.make_matrix (nb_column - nb_row) nb_column 0. in
    Matrix.of_arrays (Array.append array_matrix pad) ;;


(* Initialisation des variables *)
let init_var array compression_rate =
    let nb_row, nb_column = ((Array.length array), (Array.length array.(0))) in
    let nb_comp_column = int_of_float ((float_of_int (min nb_row nb_column)) *. compression_rate) in
    let padding = nb_row < nb_column in
    let m = if padding then (pad_matrix (Matrix.of_arrays array) nb_row nb_column) else (Matrix.of_arrays array) in (* Padding dans le cas où l'image est en mode paysage (nb_ligne<nb_colonne)*)
    let nb_row = (fst (Matrix.dims m)) in (* Pour gérer le cas du padding *)
    let vecMat_inter = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_comp_column)) in (* MxC *)
    let vecMat_res = Vectmat.mat_convert (`M (Matrix.create ?init:(Some 0.) nb_row nb_column)) in (* MxN *)
    (nb_row, nb_column, nb_comp_column, m, vecMat_inter, vecMat_res, padding) ;;


(* Redéfénir taux de compression comme étant le pourcentage de valeur singulière à garder plutôt que le pourcentage de colonnes à garder*)
let compress_svd arrays_u array_s arrays_v nb_comp_column nb_column=
    let vecMat_u_comp = Vectmat.mat_convert (`M (Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_comp_column) arrays_u))) in (* Compression en matrice de taille MxC*)
    (* Création de la matrice compressée qui aura les valeurs singulière sur la diagonale *)
    let array_s_comp = Array.sub array_s 0 nb_comp_column in (* Compression du tableau des valeurs singulières *)
    let vecMat_s_comp_array = Matrix.to_arrays (Matrix.create ?init:(Some 0.) nb_comp_column nb_comp_column) in (* CxC *)
    Array.iteri (fun i valeur_singuliere -> vecMat_s_comp_array.(i).(i) <- valeur_singuliere) array_s_comp; (* assignation de valeur dans la diagonale *)
    let vecMat_s_comp = Vectmat.mat_convert (`M (Matrix.of_arrays vecMat_s_comp_array)) in

    let mat_vT_comp = Matrix.create nb_comp_column nb_column in (* Création de la matrice transposée CxN *)
    let mat_v_comp = Matrix.of_arrays (Array.map (fun row -> Array.sub row 0 nb_comp_column) arrays_v) in (* Compression en matrice de taille NxC *)
    Matrix.transpose mat_vT_comp mat_v_comp;
    let vecMat_vT_comp = Vectmat.mat_convert (`M mat_vT_comp) in
    (vecMat_u_comp, vecMat_s_comp, vecMat_vT_comp) ;;


(* Effectue le compression du tableau de taille MxN selon le compression_rate (un pourcentage)
Renvoie une matrie MxN qui est le résultat de la SVD compressée *)
let make_compression array compression_rate verbose =
    assert(compression_rate <= 1.);
        (*  M        N            C      MxN/NxN    MxC         MxN       bool *)
    let (nb_row, nb_column, nb_comp_column, m, vecMat_inter, vecMat_res, padded) = init_var array compression_rate in

    let (arrays_u, array_s, arrays_v) = exec_svd m in
        (*     MxC          CxC            CxN          *)
    let (vecMat_u_comp, vecMat_s_comp, vecMat_vT_comp) = compress_svd arrays_u array_s arrays_v nb_comp_column nb_column in

    Linalg.matmult ~a:vecMat_u_comp ~b:vecMat_s_comp vecMat_inter;
    Linalg.matmult ~a:vecMat_inter ~b:vecMat_vT_comp vecMat_res;

    if verbose then (   (* Optionnel pour avoir des informations supplémentaires sur la matrice en console *)
        let non_zero_s = Array.of_list (List.filter (fun x -> x <> 0. ) (Array.to_list array_s)) in
        Printf.printf "\nTaille de l'image = (%d, %d)\n" nb_row nb_column;
        Printf.printf "Rang de l'image = %d; Rang de la matrice compressée = %d\n" (Array.length non_zero_s) (Array.length (Vectmat.to_arrays vecMat_s_comp));
    );

    if padded then Array.sub (Vectmat.to_arrays vecMat_res) 0 (Array.length array) (* Retire les lignes qui ont été ajoutées*)
    else Vectmat.to_arrays vecMat_res;;
