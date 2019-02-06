
open Engine

module Engine = GameEngine

(**[how_many_players ()] checks if the person wants to play against
   another human or against an AI. *)
let rec which_game () = 
  match String.trim (read_line ()) with
  | s when s = "1" -> 1
  | s when s = "2" -> 2
  | s when s = "3" -> 3
  | s when s = "4" -> 4
  | s when s |> String.lowercase_ascii = "quit" -> exit 0
  | _ -> 
    (print_string "
    Invalid input, try again. Human vs Easy AI (1), Human vs Hard AI (2), 
    Human vs Human (3), or watch an easy AI vs hard AI (4)?\n\n> "; 
     which_game ())

(**[main ()] is the game engine. *)
let main () =
  print_string "\n\nWelcome to the 3110 Chess!!\n\n";
  print_endline "Please enter moves in the following form:\n";
  print_string  "move from here  (char * int)  to  (char * int)";
  print_string  "\n Human vs Easy Computer (1), Human vs Hard Computer (2),
  Human vs Human (3), or watch an easy AI vs hard AI (4) ? \n> ";
  match which_game () with 
  | 1 -> GameEngine.play_with_AI ()
  | 2 -> GameEngine.play_with_advance_AI ()
  | 3 -> GameEngine.play_with_humans ()
  | 4 -> GameEngine.watch_AI ()
  | _ -> failwith ""


(* Execute the game engine. *)
let () = main ()