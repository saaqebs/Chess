open Board 
open Vector

(** 
   Advanced AI

   This module represents chooses the optimal move for chess.
*)

module type Ai = sig

  exception GameOver
  exception NoMoves

  (**[get_move b c] returns a move for [c] given the state [b]
     Returns start and end position for the move
     Will return a random move for random ai and the optimal move
     for advanced ai.
     Raises:[NoMoves] if there are no valid moves
  
  *)
  val get_move : GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).position*GameBoard(MapVector).position

end

module AdvancedAI : Ai