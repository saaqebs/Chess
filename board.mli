open Vector

(** 
   Representation of a chess board.

   This module represents the board of the current game being played including
   positions containing pieces.
*)
module type Board = functor (Vec:Vector) -> sig
  (** The abstract type of values representing the game board. *)
  type t 
  type piece
  type color = White | Black
  type value = Pawn | King | Queen | Rook | Knight | Bishop 

  (** The type representing the result of an attempted movement. *)
  type result = Legal of t | Illegal of string
  type position = (char*int)

  exception NoMoves

  (** [init_board ()] is the initial board at the start of a game
      contains 16 pieces for each side. The board returned resembles that of 
      a chess game. *)
  val init_board : t

  (** [print_board b] is the printed representation of the board [b]. *)
  val print_board : t -> unit

  (** [get_piece p b] is the piece at position [p] on board [b] or
      None if there is no piece at [p]. *)
  val get_piece : position -> t -> piece option

  (** [get_piece_color p] returns the color of the piece [p]. *)
  val get_piece_col : piece -> color 

  (** [get_piece_val p] returns the value of the piece [p]. *)
  val get_piece_val : piece -> value

  (** [determine_check b c] determines if a player with color [c] is in check
      on a certain board [b]. *)
  val determine_check : t -> color -> bool 

  (** [move board pos1 pos2] is [r] of attempting to move a piece from one
      location to another using positions on the board. 
      Raises: 
        - [Illegal] if there is no piece to move
        - [Illegal] if the desired position is already full *)
  val move : t -> position -> position -> color -> result

  (** [castle bool c b] returns a result of a castle on a certain board [b]. *)
  val castle : bool -> color -> t -> result

  (** [check_move b x y c] checks if a move is valid on [b] given a color [c]
      and a start [x] and end position [y]. *)
  val check_move : t -> position -> position -> color -> bool

end

module GameBoard : Board