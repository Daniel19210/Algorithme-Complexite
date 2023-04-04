let rec interactive () =
    let event = Graphics.wait_next_event [Graphics.Key_pressed] in
    if event.key == 'q' then exit 0
    else print_char event.key; print_newline (); interactive ();;

let show_image image =
    Graphics.open_graph "";
    let img = Jpeg.load image [] in Graphics.set_window_title image;
    let g = Graphic_image.of_image img in
    Graphics.draw_image g 0 0;
    interactive ();;

let open_image_as_array image =
    Graphic_image.array_of_image (Jpeg.load image []);;

let open_image_as_graph image =
    Graphic_image.of_image (Jpeg.load image []);;

let draw_image_array img_array =
    let img = Graphics.make_image (Array.map (Array.map (fun pixel -> Graphics.rgb (pixel lsr 16 land 0xff) (pixel lsr 8 land 0xff) (pixel land 0xff))) img_array) in
    img;;
