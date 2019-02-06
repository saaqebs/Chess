open Board
open Vector
open Ai
open Player

module type Game =  sig
  exception GameOver
  exception Invalid
  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t
  val turn : GameBoard(MapVector).t 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t
  val play : unit -> unit
end

module type GameAI = sig
  val parse : string -> int
  val ai_turn : Board.GameBoard(Vector.MapVector).t 
    -> Board.GameBoard(Vector.MapVector).color 
    -> Board.GameBoard(Vector.MapVector).t
  val one_round :
    (GameBoard(MapVector).t 
     -> GameBoard(MapVector).color 
     -> GameBoard(MapVector).t) 
    -> GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> Board.GameBoard(Vector.MapVector).t
  val one_game : GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> 'a
  val go_first_or_second : unit -> GameBoard(MapVector).color 
  val play : unit -> unit 
end

module HumanGame : Game = struct

  module B = GameBoard(MapVector)
  module P = Human

  type command = 
    | Movement of ((char * int) * (char * int))
    | Castle of (bool)
    | Quit
    | Help of string

  exception GameOver
  exception Invalid
  exception Empty

  (* AF: A human game module that runs through one full game with black and 
     white pieces. Player one has white pieces and player two has black pieces. 
     The game ends if one human player captures the other's king. 
   * RI: Movements are restricted in accordance to chess rules. The game
     prevents any invalid movements, and requires a valid movement to continue.
     The game only ends if one player captures the other's king. *)

  (**[match_move s] matches the string [s] inputted with a movement. *)
  let match_move (s:string) = 
    let s = s |> String.trim |> String.uppercase_ascii in 
    let c1 = Char.code (s.[0]) in 
    let _ = 
      if (c1 > 64 && c1 < 73) || (c1 > 96 && c1 < 105) 
      then () else (raise Invalid) in
    let c2 = Char.code (s.[1]) in 
    let check_c2 = if (c2 > 47 && c2 < 56) then c2-48 else (raise Invalid) in
    (s.[0], check_c2) 

  (**[parse str] parses through the string [str] to find the desired command.
     Raises: [Invalid] if the command inputted is invalid. *)
  let parse str =
    let tokens = 
      str |> String.split_on_char ' ' |> List.filter (fun x -> x <> "") in

    match tokens with 
    | [] -> raise Invalid
    |h :: [] when h = "help" -> Help("Enter moves as pos-1 to pos-2
  EX: \"C1 to D0\" or \"c1 D0\"
  \"quit\" to quit game")
    | h :: [] -> if h = "quit" then Quit else raise Invalid
    | h :: t when h |> String.lowercase_ascii = "castle" ->
      (match t with
       |h1::[] -> let str = h1|> String.lowercase_ascii in if str = "queenside"
                                                           || str = "queen"
         then Castle(true) else if 
           str = "kingside" || str = "king" then 
           Castle (false) else raise Invalid
       |h1::t2::[] -> let str1 = (h1|>String.lowercase_ascii) in 
         let str2 = (t2|>String.lowercase_ascii) in
         if str2 = "side" then if str1 = "queen" then Castle(true) else
           if str1 = "king" then Castle(false) else raise Invalid
         else raise Invalid
       |_ -> raise Invalid)
    | h :: m ::t :: [] when m = "to" ->Movement((match_move h),(match_move t))
    | h :: t :: [] -> Movement(match_move h,match_move t)
    | _ -> (raise Invalid)

  (**[check_msg ()] prints that you are entering check. *)
  let check_msg () = print_string "\nYou are in check.\n"

  (**[print_invalid] prints invalid move to the standard output. *)
  let print_invalid () = print_string " Invalid input, Try again"

  let rec move b x y c = 
    match P.move b x y c with 
    | exception P.Invalid -> print_invalid (); (raise Invalid)
    | exception Invalid_argument s -> print_string (" "^s); (raise Invalid)
    | r -> r

  let rec turn b c =
    print_string "\n\n Whats your move? \n> ";
    match parse (read_line ()) with 
    | Quit -> exit 0
    |Castle (s) -> (match B.castle s c b with
        |Illegal (m) -> print_string m; turn b c
        |Legal(r) -> r)
    | Help (m) -> print_string m; turn b c;
    | exception Invalid -> print_invalid (); turn b c
    | Movement (x,y) -> 
      match move b x y c with 
      | exception GameOver -> raise GameOver
      | exception Invalid -> turn b c
      | r -> r


  (**[one_round b] is one round on a board [b].
     Raises: [GameOver] if the king is captured. *)
  let one_round b = 
    try 
      print_newline ();
      B.print_board b;
      print_string "\n White's turn!\n";
      let black_board = turn b B.White in 
      print_newline ();
      B.print_board black_board;
      print_string "\n Black's turn!\n";
      turn black_board B.Black 
    with GameOver -> 
      print_string "\n Final Board: \n";
      B.print_board b;
      print_string "\n Checkmate. Game Over!! \n\n";
      raise GameOver

  (**[one_game b] iterates through game of chess.
     Raises: [GameOver] if the king is captured. *)
  let rec one_game b =
    try one_game (one_round b) with GameOver -> raise GameOver


  let play () =
    try one_game (B.init_board) with GameOver -> () 

end

module AIGame : GameAI = struct
  include HumanGame

  (* AF: An AI game module that runs through one full game with black and 
     white pieces. Player one has white pieces and player two has black pieces. 
     The game ends if one player captures the other's king. This module is an
     extension of the human game module. This module encorporates an easy
     and random AI computer.
   * RI: Movements are restricted in accordance to chess rules. The game
     prevents any invalid movements, and requires a valid movement to continue.
     The game only ends if one player captures the other's king. *)

  module B = GameBoard(MapVector)
  module A = EasyAIPlayer


  let parse = function 
    | s when String.trim s = "1" -> 1 
    | s when String.trim s = "2" -> 2 
    | s when s |> String.lowercase_ascii = "quit" -> exit 0
    | _ -> raise Invalid 


  (**[ai_turn b c] iterates through the Artificial Intelligence's turn on 
     the board [b] with color [c].
     Raises: [failwith] if the Artificial Intelligence has failed *)
  let ai_turn b c = 
    try A.move b c with A.GameOver -> raise GameOver

  let one_round f b c = 
    try 
      print_newline ();
      B.print_board b;
      let _ = 
        if c = B.White then 
          print_string "\n Your turn!\n" 
        else 
          print_string "\n Computer's turn!\n"  in 
      let first_board = 
        if c = B.White then turn b B.White else f b B.White in 
      print_newline ();
      B.print_board first_board;
      let _ = 
        if c = B.White then 
          print_string "\n Computer's turn!\n" 
        else 
          print_string "\n Your turn!\n"  in 
      if c = B.White 
      then f first_board B.Black 
      else turn first_board B.Black
    with GameOver -> 
      print_string "\n Final Board: \n";
      B.print_board b;
      print_string "\n Checkmate. Game Over!! \n\n";
      raise GameOver

  let rec one_game b c =
    try one_game (one_round ai_turn b c) c with GameOver -> raise GameOver


  let rec go_first_or_second () =
    print_string "\n Would you like to go first(1) or second(2)?\n> ";
    match parse (read_line ()) with 
    | 1 -> B.White 
    | 2 -> B.Black
    | exception Invalid -> print_string "\n Invalid input."; 
      go_first_or_second ()
    | _ -> print_string "\n Invalid input."; go_first_or_second ()

  let play () =
    try 
      one_game (B.init_board) (go_first_or_second ()) 
    with GameOver -> () 
end

module AdvancedAIGame : GameAI = struct
  include HumanGame
  include AIGame

  (* AF: An AI game module that runs through one full game with black and 
     white pieces. Player one has white pieces and player two has black pieces. 
     The game ends if one player captures the other's king. This module is an
     extension of the human game module. This module involves an advanced
     artificial intelligence 
   * RI: Movements are restricted in accordance to chess rules. The game
     prevents any invalid movements, and requires a valid movement to continue.
     The game only ends if one player captures the other's king. *)

  module B = GameBoard(MapVector)
  module A = AdvancedAIPlayer

  (**[ai_turn b c] iterates through the Artificial Intelligence's turn on 
     the board [b] with color [c].
     Raises: [failwith] if the Artificial Intelligence has failed *)
  let ai_turn b c = 
    try A.move b c with A.GameOver -> raise GameOver

  let rec one_game b c =
    try one_game (one_round ai_turn b c) c with GameOver -> raise GameOver

  let play () =
    try 
      one_game (B.init_board) (go_first_or_second ()) 
    with GameOver -> () 

end


module AIvAIGame : GameAI = struct
  include HumanGame
  include AIGame

  (* AF: An AI game module that runs through one full game with black and 
     white pieces. The game ends if one player captures the other's king. 
     This module is an extension of the human game module. This module 
     involves an advanced artificial intelligence and an easy artificial 
     intelligence who play against one another.
   * RI: Movements are restricted in accordance to chess rules. The game
     prevents any invalid movements, and requires a valid movement to continue.
     The game only ends if one player captures the other's king. *)

  module B = GameBoard(MapVector)

  let easy_ai_turn = ai_turn

  open AdvancedAIGame

  let adv_ai_turn = ai_turn

  let one_round f b c = 
    try 
      print_newline ();
      B.print_board b;
      let _ = 
        if c = B.White then 
          print_string "\n Hard Computer's turn!\n" 
        else 
          print_string "\n Easy Computer's turn!\n"  in 
      let first_board = 
        if c = B.White then adv_ai_turn b B.White else f b B.White in 
      print_newline ();
      B.print_board first_board;
      let _ = 
        if c = B.White then 
          print_string "\n Easy Computer's turn!\n" 
        else 
          print_string "\n Hard Computer's turn!\n"  in 
      if c = B.White 
      then f first_board B.Black 
      else adv_ai_turn first_board B.Black
    with GameOver -> 
      print_string "\n Final Board: \n";
      B.print_board b;
      print_string "\n Checkmate. Game Over!! \n\n";
      raise GameOver

  (** This is a counter to insure that the AI game caps at 200 rounds. *)
  let counter = ref 0
  let incr () = counter := !counter+1

  let rec one_game b c =
    try 
      incr ();
      if !counter = 400 then (raise Invalid) else 
        one_game (one_round easy_ai_turn b c) c 
    with GameOver -> raise GameOver | Invalid -> raise Invalid

  let rec go_first_or_second () =
    print_string "\n Should the Advanced AI go first(1) or second(2)?\n> ";
    match parse (read_line ()) with 
    | 1 -> B.White 
    | 2 -> B.Black
    | exception Invalid -> print_string "\n Invalid input."; 
      go_first_or_second ()
    | _ -> print_string "\n Invalid input."; go_first_or_second ()

  let play () =
    try 
      one_game (B.init_board) (go_first_or_second ()) 
    with
    | Invalid -> print_string "\n Stalemate, AI game over! \n"; ()
    | GameOver -> () 

end

