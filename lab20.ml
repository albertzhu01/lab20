(* images are lists of lists of floats between 0. (white) and 1. (black) *)
type image = float list list ;;

(* threshold threshold image -- image where pixels above the threshold
value are black *)
let threshold (img : image) (thvalue : float) : image =
  List.map  (fun row -> List.map (fun v -> if v <= thvalue then 0. else 1.)
                                 row) img ;;

(* dither max image -- dithered image *)
let dither (img : image) : image =
  List.map
    (fun row ->
     List.map
       (fun v -> if v > Random.float 1.
                 then 1.
                 else 0.) row)
    img ;;
  
(* show the image *)
let depict (img : image) : unit =
  Graphics.open_graph "";
  Graphics.clear_graph ();
  let rows = List.hd img in
  let x, y = List.length rows, List.length img in Graphics.resize_window x y;
  let depict_pix (v : float) (r : int) (c : int) : unit = 
    let lvl = int_of_float (255. *. (1. -. v)) in 
    Graphics.set_color (Graphics.rgb lvl lvl lvl);
    Graphics.plot c (y - r) in
    List.iteri (fun r row -> List.iteri (fun c pix -> depict_pix pix r c)
                                        row) img;
  Unix.sleep 2; 
  Graphics.close_graph () ;;

let mona = Monalisa.image ;;
  
depict mona ;;
    
let mona_threshold = threshold mona 0.75 ;;
depict mona_threshold ;;
      
let mona_dither = dither mona ;;
depict mona_dither ;;