open Board 
open Vector

(** 
   A full game of chess.

   This module represents one iteration of a full chess game.
*)

module type Game = sig

  exception GameOver
  exception Invalid

  (**[move b x y c] moves position [x] to position [y] on a certain game 
     board [b], while checking that the piece at [x] has a certain color [c]
     Raises: 
        - [Invalid] if the user inputs a bad position
        - [GameOver] if the king is captured *)
  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

  (**[turn b c] is one turn on a board [b] based on the player's color [c]. 
     Raises: [GameOver] if the king is captured. *)
  val turn : GameBoard(MapVector).t 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

  (**[play ()] iterates through one full game of chess. User may quit or
     ask for help at any point during the game. The game ends if one player 
     captures the other's king. *)
  val play : unit -> unit

end

module type GameAI = sig

  (**[parse s] parses a string [s] to see if it is 1 or 2. 
     Raises: [Invalid] if [s] does not equal 1 or 2 *)
  val parse : string -> int

  (** [ai_turn b c] is an iteration of a turn using the AI on [b] with [c].
      Raises [GameOver] if the game is over. *)
  val ai_turn : Board.GameBoard(Vector.MapVector).t 
    -> Board.GameBoard(Vector.MapVector).color 
    -> Board.GameBoard(Vector.MapVector).t

  (**[one_round f b c] runs through one round in the chess game on a 
     board [b]. Color [c] signifies which color the human represents. Runs
     using an AI's turn based on difficulty using a function [f].
     Raises: [GameOver] if the king is captured. *)
  val one_round :
    (GameBoard(MapVector).t 
     -> GameBoard(MapVector).color 
     -> GameBoard(MapVector).t) 
    -> GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> Board.GameBoard(Vector.MapVector).t

  (**[one_game b] iterates through game of chess.
       Raises: [GameOver] if the king is captured. *)
  val one_game : GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> 'a

  (**[go_first_or_second ()] checks if the user would like to go first
     or second. The method continues to run until the user 
     inputs "1", "2", or "quit". *)
  val go_first_or_second : unit -> GameBoard(MapVector).color 

  (**[play ()] plays a game of chess with the specified
     AI. Returns () once the game has completeted.*)
  val play : unit -> unit 
end

module HumanGame : Game 

module AIGame : GameAI

module AdvancedAIGame : GameAI

module AIvAIGame : GameAI 