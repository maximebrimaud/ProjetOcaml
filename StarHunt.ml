open Directions;;
open Interprete;;

module Images = Images50;;
let perso = Images.hornGirl;;
(* Graphics.close_graph ();; *)
Graphics.open_graph " 350x350";;
Graphics.set_color (Graphics.rgb 102 130 0);;
Graphics.fill_rect 0 0 350 350;;
Graphics.set_color (Graphics.rgb 51 102 0);;
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

let etoiles =  [(ref 0,ref 0);(ref 6,ref 6);(ref 5, ref 3);(ref 4, ref 1);(ref 4,ref 4);(ref 1,ref 6)];;

let rock = [(ref 5,ref 1);(ref 5,ref 2);(ref 4,ref 2);(ref 4,ref 3);(ref 3,ref 4);(ref 5,ref 4);(ref 6,ref 4);(ref 3,ref 1);(ref 1,ref 0);(ref 1,ref 1);
(ref 1,ref 2);(ref 1,ref 4);(ref 0,ref 5);(ref 2,ref 6);(ref 4,ref 6);(ref 5,ref 5)];;

let teleporteur1 =[(ref 0,ref 4)];;

let teleporteur2 =[(ref 5,ref 6)];;

let limite = [(ref 7,ref 0);(ref 7,ref 1);(ref 7,ref 2);(ref 7,ref 3);(ref 7,ref 4);(ref 7,ref 5);(ref 7,ref 6);
(ref 6,ref 7);(ref 5,ref 7);(ref 4,ref 7);(ref 3,ref 7);(ref 2,ref 7);(ref 1,ref 7);(ref 0,ref 7);
(ref (0-1),ref 0);(ref (0-1),ref 1);(ref (0-1),ref 2);(ref (0-1),ref 3);(ref (0-1),ref 4);(ref (0-1),ref 5);(ref (0-1),ref 6);
(ref 6,ref (0-1));(ref 5,ref (0-1));(ref 4,ref (0-1));(ref 3,ref (0-1));(ref 2,ref (0-1));(ref 1,ref (0-1));(ref 0,ref (0-1))];;


let perso_i, perso_j = ref 6, ref 0;;

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
let afficher_tele1 () =
  List.iter (afficher Images.pastillerose) teleporteur1;
  afficher_perso ();;
let afficher_tele2 () =
  List.iter (afficher Images.pastillerose) teleporteur2;
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
	| S -> begin perso_i := !perso_i - 1; print_string "Vous avez rencontrer un obstacle \n" end
	| N -> begin perso_i := !perso_i + 1; print_string "Vous avez rencontrer un obstacle \n" end)
  in
  let limite_carte (ri,rj) = 
      if (!ri,!rj) = (!perso_i,!perso_j) then (match direction with
	| E -> begin perso_j := !perso_j - 1; print_string "Ne fuyez pas, accomplissez votre devoir chasseur \n" end
	| O -> begin perso_j := !perso_j + 1; print_string "Ne fuyez pas, accomplissez votre devoir chasseur \n" end
	| S -> begin perso_i := !perso_i - 1; print_string "Ne fuyez pas, accomplissez votre devoir chasseur \n" end
	| N -> begin perso_i := !perso_i + 1; print_string "Ne fuyez pas, accomplissez votre devoir chasseur \n" end)
  in
  let tele_One (ri,rj) = 
      if (!ri,!rj) = (!perso_i,!perso_j) then (match direction with
	| E -> begin perso_j := 5; perso_i := 6; print_string "TELEPORTATIOOOOON \n" end
	| O -> begin perso_j := 5; perso_i := 6; print_string "TELEPORTATIOOOOON \n" end
	| S -> begin perso_j := 5; perso_i := 6; print_string "TELEPORTATIOOOOON \n" end
	| N -> begin perso_j := 5; perso_i := 6; print_string "TELEPORTATIOOOOON \n" end)
  in
  let tele_Two (ri,rj) = 
      if (!ri,!rj) = (!perso_i,!perso_j) then (match direction with
	| E -> begin perso_j := 3; perso_i := 0; print_string "TELEPORTATIOOOOON \n" end
	| O -> begin perso_j := 3; perso_i := 0; print_string "TELEPORTATIOOOOON \n" end
	| S -> begin perso_j := 3; perso_i := 0; print_string "TELEPORTATIOOOOON \n" end
	| N -> begin perso_j := 3; perso_i := 0; print_string "TELEPORTATIOOOOON \n" end)
  in
  let ramasser_etoile (ri,rj) =
    if (!ri,!rj) = (!perso_i,!perso_j) then
      begin
        ri := -1;
        rj := 7;
      end
  in
  let youWin ()=
    let rec nbetoile p = match p with
      | [] -> print_string "Euuuuuuuuuuh"
      | [(i,j)] -> if (!i,!j)=(-1,7) then print_string "Felicitation, vous avez gagne \n"
      | (i,j)::xs -> if (!i,!j)=(-1,7) then nbetoile xs else print_string "il reste encore du boulot \n"
  in nbetoile etoiles
in
  List.iter blok_rock rock;
  List.iter limite_carte limite;
  List.iter tele_One teleporteur1;
  List.iter tele_Two teleporteur2;
  List.iter ramasser_etoile etoiles;
  afficher_decor ();
  afficher_mobiles ();
  afficher_rock();
  afficher_tele1();
  afficher_tele2();
  youWin();;
	
let jouer p = Interprete.run p deplacer;;


print_string "*** StarHunt***\n";;
print_string "Capture toutes les etoiles\n";;
print_string "Maxime Brimaud et Jessica Curpen\n";;
print_string " Credit image : lostgarden.com\n";;
afficher_mobiles ();;
afficher_rock();;
afficher_tele1();;
afficher_tele2();;


(*jouer (Bloc[Est;Est;Est;Nord;Sud;Ouest;Ouest;Ouest;Nord;Nord;Est;Ouest;Nord]);;
jouer (Bloc[Nord;Est;Est;Est;Nord;Nord;Est]);;
jouer (Bloc[Est;Nord]);;
jouer (Bloc[Ouest;Ouest;Ouest;Est;Est;Est;Sud;Sud;Est;Est;Nord;Est;Ouest;Sud;Sud;Sud;Ouest]);;*)
