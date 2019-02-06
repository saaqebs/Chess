
open Board
open Vector
open Ai

module type HumanPlayer = sig 

  exception GameOver
  exception Invalid 

  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).position 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

end 

module type AIPlayer = sig 

  exception GameOver
  exception Invalid 

  val move : GameBoard(MapVector).t 
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).t

end 


module Human : HumanPlayer = struct

  exception GameOver
  exception Invalid

  module B = GameBoard(MapVector)

  let rec move b x y c = 
    match B.move b x y c with 
    | Illegal s -> print_string (s^". Try again\n"); 
      (raise Invalid) (** catch exception*)
    | Legal r -> r

end 

module EasyAIPlayer : AIPlayer = struct

  exception GameOver
  exception Invalid

  module B = GameBoard(MapVector)

  (**[str_from_int] is the string a certain int represents in our chess game*)
  let str_from_int = function
    | 0 -> "A"
    | 1 -> "B"
    | 2 -> "C"
    | 3 -> "D"
    | 4 -> "E"
    | 5 -> "F"
    | 6 -> "G"
    | 7 -> "H"
    | _ -> failwith "internal error"

  (**[char_to_position c] turns a character [c] into an index for the vector.*)
  let char_to_pos = function
    | 'A' -> 0
    | 'B' -> 1
    | 'C' -> 2
    | 'D' -> 3
    | 'E' -> 4
    | 'F' -> 5
    | 'G' -> 6
    | 'H' -> 7
    | _ -> raise (invalid_arg "Cannot get index for this number, board is 8x8")

  (**[pos_to_char] converts and integer to a char given our coordinate scheme*)
  let pos_to_char = function
    |0 -> 'A'
    |1 -> 'B'
    |2 -> 'C'
    |3 -> 'D'
    |4 -> 'E'
    |5 -> 'F'
    |6 -> 'G'
    |7 -> 'H'
    |_ -> failwith "shouldnt happen"

  type spot = {p:B.piece; x:int; y:int}

  (**[cord_to_poss c] is the char*int representation of a position on the board
  *)
  let cord_to_poss (c:int*int) = 
    ((pos_to_char (fst c)), snd c)

  (**[spot_to_cord s] interprets the spot on a board to solety the numerical
     location of that spot*)
  let spot_to_cord s = (s.x, s.y)

  (**[incr_loc (x,y)] increments the tuple provided in either [x] or [y] but
     not both*)
  let incr_loc (l:int*int) =
    match l with
    |(x, 7) -> ((x+1), 0)
    |(x, y) -> (x, (y+1)) 

  (*Finds all the pices with a certain color*)
  let rec get_pieces (c:B.color) b (accum: spot list) xi yi=
    if xi = 8 then accum else
      let next_loc = incr_loc (xi,yi) in
      match B.get_piece ((pos_to_char xi), yi) b with
      |None -> get_pieces c b accum (fst next_loc) (snd next_loc)
      |Some(pi) -> if (B.get_piece_col pi) = c 
        then get_pieces c b ( {p = pi;x =xi;y = yi} :: accum) 
            (fst next_loc) (snd next_loc)
        else get_pieces c b  accum (fst next_loc) (snd next_loc)

  (*Gets a list of possible paces to move*)
  let rec get_moves (c:B.color) b (accum: (int*int) list) xi yi =
    if xi = 8 then accum else
      let next_loc = incr_loc (xi,yi) in
      match B.get_piece ((pos_to_char xi), yi) b with
      |None -> get_moves c b ((xi, yi) :: accum) (fst next_loc) (snd next_loc)
      |Some(pi) -> if (B.get_piece_col pi) <> c 
        then get_moves c b ( (xi, yi) :: accum) (fst next_loc) 
            (snd next_loc)
        else get_moves c b  accum (fst next_loc) (snd next_loc)

  (* checks a list of moves dest and returns list of valid moves*)
  let rec check_moves (f) init (dest: (int*int) list) accum c =
    match dest with
    |[]  -> accum
    |h::t -> if (f (cord_to_poss init) (cord_to_poss h) c) 
      then check_moves f init t (h::accum) c
      else check_moves f init t accum c

  (*Removes the nth item from a list. *)
  let rec remove_nth n (oldl : spot list) (newl: spot list) = 
    match oldl with
    | [] -> newl
    |h::t -> if n = 0
      then t @ newl
      else remove_nth (n-1) t (h :: newl)

  (*Finds all of pieces, chooses one at random to move. 
    If no moves are possible for that piece, is randomly chooses another *)
  let rec find_valid color board lst : (int*int)*(int*int)=
    if (List.length lst) = 0 then raise B.NoMoves else
      Random.self_init();
    let n = Random.int (List.length lst) in
    let to_move = List.nth lst n in
    let poss_moves = get_moves color board [] 0 0 in
    let valid_moves = check_moves (B.check_move board) 
        (spot_to_cord to_move) poss_moves [] color in
    match valid_moves with
    | [] -> find_valid color board (remove_nth n lst [])
    | h::t -> let ll = List.length valid_moves in
      if ll = 1 then ((spot_to_cord to_move), h)
      else 
        ((spot_to_cord to_move),(List.nth valid_moves (Random.int ll)))

  (**[random_move col b] makes a random move for color [col] in board [b].
     This is used as a helper funciton for our "easy" AI to select a move to 
     make on the board. Raises NoMoves if there are no valid moves remaining on 
     the board for color [col]*)
  let random_move color board = 
    let sol = 
      match (find_valid color board (get_pieces color board [] 0 0)) with 
      | exception B.NoMoves -> raise B.NoMoves
      | e -> e
    in
    ((cord_to_poss (fst sol)), (cord_to_poss (snd sol)))

  let move b c = 
    match random_move c b with 
    | exception B.NoMoves -> raise GameOver
    | (x,y) -> 
      match B.move b x y c with 
      | Illegal s -> raise (Invalid_argument s)
      | Legal r -> r

end

module AdvancedAIPlayer : AIPlayer = struct

  exception GameOver
  exception Invalid

  module B = GameBoard(MapVector)
  module R = EasyAIPlayer
  module A = AdvancedAI

  let rec move b c = 
    match A.get_move b c with 
    | exception A.GameOver -> raise GameOver
    | exception A.NoMoves -> R.move b c
    | (x,y) -> 
      match B.move b x y c with 
      | exception A.GameOver -> raise GameOver
      | Illegal s -> failwith s
      | Legal r -> r

end
