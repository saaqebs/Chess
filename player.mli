
open Board
open Vector

module type HumanPlayer = sig 

  exception GameOver
  exception Invalid 

  (**[Move b pos1 pos2 col] is the board resulting in moving the piece at [pos1]
     to [pos2] in the board [b]. This reads an input form the command line for
     the human player and will raise [Invalid] if an illegal input is supplied*)
  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

end 

module type AIPlayer = sig 

  exception GameOver
  exception Invalid 

  (**[move board color] makes an AI move on the [board] for the [color] and 
     returns the resulting gameboard from the AI. This raises [GameOver] if 
     there are no more valid moves remaining on the [board] for [color]. *)
  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

end 

module Human : HumanPlayer

module EasyAIPlayer : AIPlayer

module AdvancedAIPlayer : AIPlayer