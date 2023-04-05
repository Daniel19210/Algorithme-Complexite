let compare_result image image_compresse =
    (* Fonction qui permet de créer une sesion interactive que l'on peut quitter avec la touche 'q' *)
    let rec interactive () =
        let event = Graphics.wait_next_event [Graphics.Key_pressed] in
        if event.key == 'q' then exit 0
        else (print_char event.key; print_newline ()); interactive () in

    let graphe_image_compresse = Graphics.make_image (Array.map (Array.map (fun pixel -> Graphics.rgb (pixel lsr 16 land 0xff) (pixel lsr 8 land 0xff) (pixel land 0xff))) image_compresse) in (* Création du tablean en l'image graphique avec le RGB, source d'erreur potentielle*)
    let graphe_image = Graphic_image.of_image (Jpeg.load image []) in (* Ouverture de l'image de base*)

    Graphics.draw_image graphe_image 25 100; (* Affichage de l'image de base *)
    Graphics.draw_image graphe_image_compresse ((Array.length image_compresse)+50) 100; (* Affichage de l'image compressée*)
    interactive();; (* Création d'une session interactive pour voir les résultats *)
