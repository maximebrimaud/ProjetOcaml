open Directions;;
#load "graphics.cma";;
#load "directions.cmo";;
#load "images50.cmo";;
#load "dessiner.cmo";;
#load "interprete.cmo";;

module Images = Images50;;
let perso = Images.hornGirl;;
(* Graphics.close_graph ();; *)
Graphics.open_graph " 350x350";;
Graphics.set_color (Graphics.rgb 0 40 180);;
Graphics.fill_rect 0 0 350 350;;
Graphics.set_color (Graphics.rgb 0 128 255);;
let f i =
  Graphics.draw_segments [|
      (50*i,0,50*i,350);
      (0,50*i,350,50*i)
     |];
in
List.iter f [1;2;3;4;5;6];;

Graphics.remember_mode false;;

let afficher img (i,j) =
  Dessiner.dessiner_image img (!j * 50) (300 - !i * 50);;

let etoiles =  [(ref 0,ref 0);(ref 6,ref 6);(ref 4, ref 5)
                ;(ref 2, ref 2);(ref 3,ref 5)];;
let rock = [(ref 6,ref 0);(ref 6,ref 1)];;

let perso_i, perso_j = ref 3, ref 3;;

let afficher_decor () =
  Graphics.synchronize();;
let afficher_perso() =
  afficher perso (perso_i,perso_j);;
let afficher_mobiles () =
  List.iter (afficher Images.star) etoiles;
  afficher_perso ();;
let afficher_rock () =
  List.iter (afficher Images.rock) rock;
  afficher_perso ();;


let deplacer direction =
  let () = match direction with
    | E -> perso_j := !perso_j + 1
    | O -> perso_j := !perso_j - 1
    | S -> perso_i := !perso_i + 1
    | N -> perso_i := !perso_i - 1
  in
  let blok_rock (ri,rj) = 
      if (!ri,!rj) = (!perso_i,!perso_j) then (match direction with
	| E -> begin perso_j := !perso_j - 1; print_string "Vous avez rencontrer un obstacle \n" end
	| O -> begin perso_j := !perso_j + 1; print_string "Vous avez rencontrer un obstacle \n" end
	| S -> begin perso_j := !perso_j - 1; print_string "Vous avez rencontrer un obstacle \n" end
	| N -> begin perso_j := !perso_j + 1; print_string "Vous avez rencontrer un obstacle \n" end)
  in
  let ramasser_etoile (ri,rj) =
    if (!ri,!rj) = (!perso_i,!perso_j) then
      begin
        ri := -1;
        rj := 7;
      end
  in
  List.iter ramasser_etoile etoiles;
  List.iter blok_rock rock;
  afficher_decor ();
  afficher_mobiles ();
  afficher_rock();;

	


let jouer p = Interprete.run p deplacer;;


print_string "*** StarHunt***\n";;
print_string "Capture toutes les étoiles\n";;
print_string "Maxime Brimaud et Jessica Curpen\n";;
print_string " Crédit image : lostgarden.com\n";;
afficher_mobiles ();;
afficher_rock();;

jouer Est;;
jouer Ouest;;
jouer Sud;;
jouer Nord;;

Interprete.run Nord deplacer;;
