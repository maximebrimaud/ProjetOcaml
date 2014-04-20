open Directions;;

type instruction = Est | Ouest | Nord | Sud | Bloc of instruction list;;

let rec run p deplacer =
  match p with
  | Est -> deplacer E
  | Ouest -> deplacer O
  | Sud -> deplacer S
  | Nord -> deplacer N 
  (*| Saut x -> deplacer x;deplacer x*)
  | Bloc (x::xs) -> run x deplacer; run (Bloc xs) deplacer
  | Bloc [] -> ();;
