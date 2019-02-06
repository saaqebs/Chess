open Vector

module type Board = functor (Vec:Vector) -> sig
  type color = White|Black
  type value = Pawn | King | Queen | Rook | Knight | Bishop 
  type t 
  type piece
  type result = Legal of t | Illegal of string
  type position = (char*int)

  exception NoMoves

  val init_board : t
  val print_board: t -> unit
  val get_piece : position -> t -> piece option
  val get_piece_col : piece -> color 
  val get_piece_val : piece -> value
  val determine_check : t -> color -> bool 
  val move : t -> position -> position -> color -> result
  val castle : bool -> color -> t -> result
  val check_move : t -> position -> position -> color -> bool
end

module GameBoard : Board = functor(Vec:Vector) -> struct
  type color = White|Black
  type value = Pawn | King | Queen | Rook | Knight | Bishop 

  type piece = { v:value; c:color }

  type t = piece Vec.t Vec.t
  type result = Legal of t | Illegal of string
  type position = (char*int)

  exception NoMoves

  (* AF: A gameboard that allows only White and Black pieces. The pieces 
     resemble the game chess, with pawns, a king, a queen, rooks, knights, 
     and bishops. The positions of these pieces are given as characters 
     for the user, but converted into integers. The gameboard utilizes 
     a vector to map integer indexes to another vector. The second vector 
     maps indexes to game pieces. There are 16 game pieces of each color 
     given to the user to move around. 
   * RI: The gameboard is strictly 8x8. The white and black pieces may move 
     in accordance to chess rules (i.e. pawns move one space ahead except for 
     capturing another peice). Capturing is allowed, as in accordance to chess 
     rules. *)

  (**[rep_ok_row r] is a helper that checks if the RI is violated in [r]. *)
  let rep_ok_row r = 
    for i = 0 to 8 do 
      try 
        match Vec.get i r with 
        | None -> ();
        | Some p-> 
          if p.c = Black || p.c = White then () else failwith "RI violated"
      with Invalid_argument _ -> if i = 8 then () else failwith "RI violated"
    done

  (**[rep_ok_board b] is a helper thatchecks if the RI is violated in [b] *)
  let rep_ok_board b = 
    for i = 0 to 8 do 
      try
        match Vec.get i b with 
        | Some r -> rep_ok_row r;
        | _ -> ();
      with Invalid_argument _ -> if i = 8 then () else failwith "RI violated"
    done 

  (**[rep_ok b] checks if the Rep Invariant is violated in [b].
     Raises: [Failure] with "RI violated"  *)
  let rep_ok b =
    rep_ok_board b; b

  (**[new_pawn c] returns a new pawn piece with color [c]. *)
  let new_pawn c = {c = c; v = Pawn }

  (**[pawns_row c] makes a new row of pawns based on the color [c]. *)
  let pawns_row c = 
    Vec.(empty 8 |> set 0 (new_pawn c)|> set 1 (new_pawn c)|>
         set 2 (new_pawn c) |> set 3 (new_pawn c) |>
         set 4 (new_pawn c) |>set 5 (new_pawn c)|> set 6 (new_pawn c) 
         |> set 7 (new_pawn c))

  (**[new_row_black] makes a new back row of black pieces. *)
  let new_row_black = 
    Vec.(empty 8 |> set 0 {c = Black; v = Rook}
         |> set 1 ({c = Black; v =Knight})
         |> set 2 {c = Black; v = Bishop} |> set 3 {c = Black; v = King} |>
         set 4 {c = Black; v = Queen} |>set 5 {c = Black; v = Bishop}
         |> set 6  {c = Black; v =Knight}
         |> set 7 {c = Black; v = Rook})

  (**[new_row_white] makes a new back row of white pieces. *)
  let new_row_white = 
    Vec.(empty 8 |> set 0 {c = White; v = Rook}|> set 1 ({c=White ; v=Knight})
         |> set 2 {c = White; v = Bishop} |> set 3 {c = White; v = Queen} |>
         set 4 {c = White; v = King} |> set 5 {c = White; v = Bishop}
         |> set 6  {c = White; v =Knight}
         |> set 7 {c = White; v = Rook})

  let init_board = 
    Vec.(empty 8 |> 
         set 0 new_row_white |> set 1 (pawns_row White)
         |> set 2 (empty 8) 
         |> set 3 (empty 8) |> set 4 (empty 8)
         |> set 5 (empty 8) |>
         set 6 (pawns_row Black) |> set 7 new_row_black )

  (**[piece_to_string p] turns a peice [p] into a string. *)
  let piece_to_string {c = col; v = pi} =
    match pi with 
    |King -> if col = White then "\027[31m♔\027[0m" else "\027[34m♚\027[0m"
    |Queen -> if col = White then "\027[31m♕\027[0m" else "\027[34m♛\027[0m"
    |Rook -> if col = White then "\027[31m♖\027[0m" else "\027[34m♜\027[0m"
    |Pawn -> if col = White then "\027[31m♙\027[0m" else "\027[34m♟\027[0m"
    |Bishop -> if col = White then "\027[31m♗\027[0m" else "\027[34m♝\027[0m"
    |Knight -> if col = White then "\027[31m♘\027[0m" else "\027[34m♞\027[0m"

  let rec make_printable_row vec i (accum: string list) (e: bool): string list=
    if i < 0 then accum else
      match Vec.get i vec with
      |None -> if e then
          if (i mod 2 = 1) 
          then make_printable_row vec (i-1) ("||||" :: accum) e
          else make_printable_row vec (i-1) ("|   " :: accum) e
        else
        if (i mod 2 = 0) 
        then make_printable_row vec (i-1) ("||||" :: accum) e
        else make_printable_row vec (i-1) ("|   " :: accum) e

      |Some(p) -> make_printable_row vec (i-1) 
                    ( ("| "^ (piece_to_string p)^" ") :: accum) e

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

  let rec make_printable_board board i (accum:(string list) list)= 
    if i < 0 then accum else
      match  (Vec.get i board) with
      |None -> raise (invalid_arg "Error printing board")
      |Some(v) -> 
        make_printable_board board (i-1) 
          (( (str_from_int i) :: 
             (make_printable_row v 7 [] (i mod 2 = 0))) :: accum)

  let rec print_row b = 
    match b with
    |[] -> print_string "|\n"
    |h::t -> print_string h; print_row t

  let rec print_help b = 
    match b with
    | [] -> print_string " |---|---|---|---|---|---|---|---|\n";
    | h::t -> print_string " |---|---|---|---|---|---|---|---|\n";
      print_row h; print_help t

  let print_board board =
    let print_str = make_printable_board board 7 [] in
    print_string "   0   1   2   3   4   5   6   7  \n";
    print_help print_str;
    print_string "   0   1   2   3   4   5   6   7  \n"

  let get_piece (x, y) board =
    let x_idx = char_to_pos x in
    match Vec.get x_idx board with
    | Some(by) -> Vec.get y by
    | None -> None

  let get_piece_pos (x, y) board = 
    match Vec.get x board with
    | Some(by) -> Vec.get y by
    | None -> None

  let force_get_piece (x, y) board =
    match (get_piece (x, y) board) with
    | None -> failwith "rip"
    | Some(p) -> p

  let force_get_piece_pos (x, y) board =
    match (get_piece_pos (x, y) board) with
    | None -> failwith "rip"
    | Some(p) -> p

  (** [is_piece (x, y) p b] returns whether the position [(x, y)] on [b]
      has a piece of type [p] or if there is no piece there when [p] is None *)
  let is_piece (x, y) piece_opt board =
    match piece_opt with
    | None -> get_piece (x, y) board = None
    | Some(pt) -> (force_get_piece (x, y) board).v = pt

  let get_piece_col p = p.c

  let get_piece_val p = p.v

  let check_bounds (xc, y) = 
    let x = char_to_pos xc in
    x >= 0 && x < 8 && y >= 0 && y < 8

  let check_start_piece board pos color =
    match get_piece pos board with
    | None -> false
    | Some(p) -> p.c = color

  let check_pass (c1, n1) (c2, n2) =
    not (c1 = c2 && n1 = n2)

  let check_self_destroy board epos color =
    match get_piece epos board with
    | Some({c = ec}) when ec = color -> false
    | _ -> true

  let rec check_path board (c1, n1) (c2, n2) (dx, dy) =
    let npos = (c1 |> char_to_pos 
                |> fun c -> c + dx |> pos_to_char, n1 + dy) in
    match npos with
    | (nc, nn) when nc = c2 && nn = n2 -> true
    | (nc, nn) -> 
      if (get_piece npos board) = None 
      then (check_path board npos (c2, n2) (dx, dy))
      else false

  (**[check_str_in_way] checks to see if there is a piece in the way while 
     performing a vertical or horizontal move along the board. Returns true if 
     there is no obstruction and false otherwise*)
  let rec check_str_in_way board spos epos =
    let (c1, n1) = spos in
    let (c2, n2) = epos in
    if (c1 = c2 && n2 > n1) then check_path board spos epos (0, 1)
    else if (c1 = c2 && n2 < n1) then check_path board spos epos (0, ~-1)
    else if (n1 = n2 && c2 > c1) then check_path board spos epos (1, 0)
    else if (n1 = n2 && c2 < c1) then check_path board spos epos (~-1, 0)
    else false

  (**[check_diag_in_way] checks to see if there is a piece in the way while 
     performing a diagonal move along the board. Returns true if there is no 
     obstruction and false otherwise *)
  let rec check_diag_in_way board spos epos =
    let (c1, n1) = spos in
    let (c2, n2) = epos in
    if (c2 > c1 && n2 > n1) then check_path board spos epos (1, 1)
    else if (c2 > c1 && n2 < n1) then check_path board spos epos (1, ~-1)
    else if (c2 < c1 && n2 > n1) then check_path board spos epos (~-1, 1)
    else if (c2 < c1 && n2 < n1) then check_path board spos epos (~-1, ~-1)
    else false

  let has_enemy_piece board epos color = 
    match get_piece epos board with
    | Some(p) when p.c <> color -> true
    | _ -> false

  let enemy_color = function
    | White -> Black
    | Black -> White

  let rec check_piece board (sc, sn) (ec, en) piece =
    let (c1, n1) = (char_to_pos sc, sn) in
    let (c2, n2) = (char_to_pos ec, en) in
    match piece.v with
    | Pawn when piece.c = White ->
      (c2 - c1 = 1 && abs(n2 - n1) = 1 && has_enemy_piece board (ec, en) White)
      || ((c2 - c1 = 1 || (c2 - c1 = 2 && c1 = 1))
          && (not (has_enemy_piece board (ec, en) White))
          && (check_str_in_way board (sc, sn) (ec, en)))
    | Pawn when piece.c = Black ->
      (c1 - c2 = 1 && abs(n2 - n1) = 1 && has_enemy_piece board (ec, en) Black)
      || ((c1 - c2 = 1 || (c1 - c2 = 2 && c1 = 6))
          && (not (has_enemy_piece board (ec, en) Black))
          && (check_str_in_way board (sc, sn) (ec, en)))
    | King ->
      abs (c2 - c1) <= 1 && abs (n2 - n1) <= 1
    | Rook ->
      check_str_in_way board (sc, sn) (ec, en)
    | Bishop ->
      abs (c2 - c1) = abs (n2 - n1)
      && check_diag_in_way board (sc, sn) (ec, en)
    | Queen ->
      check_piece board (sc, sn) (ec, en) {v = Bishop; c = piece.c}
      || check_piece board (sc, sn) (ec, en) {v = Rook; c = piece.c}
    | Knight ->
      (abs(c2 - c1) = 1 && abs (n2 - n1) = 2)
      || (abs(c2 - c1) = 2 && abs (n2 - n1) = 1)
    | _ -> failwith "Movement of nonexistant piece"

  let remove_piece (c, n) board =
    let (x, y) = (char_to_pos c, n) in
    let updated = 
      match Vec.get x board with
      | Some(h) -> Vec.del y h
      | None -> failwith "Out of bounds"
    in
    Vec.set x updated board

  let set_piece (c, n) piece board =
    let (x, y) = (char_to_pos c, n) in
    let updated = 
      match Vec.get x board with
      | Some(h) -> 
        if piece.v = Pawn then 
          if piece.c = Black && c = 'A' then Vec.set y {c = Black; v = Queen} h
          else
          if piece.c = White && c = 'H' then Vec.set y {c = White; v = Queen} h
          else Vec.set y piece h
        else
          Vec.set y piece h
      | None -> failwith "Out of bounds"
    in
    Vec.set x updated board

  let find_king_pos board color =
    let rec find_king_helper x y =
      if x > 7 then failwith "king nonexistent"
      else if y > 7 then find_king_helper (x + 1) 0
      else
        match get_piece_pos (x, y) board with
        | Some(p) when p.c = color && p.v = King -> (pos_to_char x, y)
        | _ -> find_king_helper x (y + 1)
    in
    find_king_helper 0 0

  let all_of_color board color =
    let rec all_of_color_helper x y acc =
      if x > 7 then acc
      else if y > 7 then all_of_color_helper (x + 1) 0 acc
      else
        match get_piece_pos (x, y) board with
        | Some(p) when p.c = color -> 
          all_of_color_helper x (y + 1) ((pos_to_char x, y) :: acc)
        | _ -> all_of_color_helper x (y + 1) acc
    in
    all_of_color_helper 0 0 []

  let move_pre_check board pos1 pos2 pc = 
    if not (check_bounds pos1) then 
      Illegal "start position out of bounds"
    else if not (check_bounds pos2) then 
      Illegal "end position out of bounds"
    else if not (check_start_piece board pos1 pc) then
      Illegal "Not your piece, or no piece is there"
    else if not (check_pass pos1 pos2) then
      Illegal "You must move somewhere"
    else if not (check_self_destroy board pos2 pc) then
      Illegal "One of your own pieces is already there"
    else if not (check_piece board pos1 pos2 (force_get_piece pos1 board)) then
      Illegal "That piece can't move that way"
    else Legal(board)

  let determine_check board color = 
    let kingpos = find_king_pos board color in
    let enemycolor = (enemy_color color) in
    let enemies = all_of_color board enemycolor in
    let possiblecheck = 
      List.find_opt
        (fun enemypos -> 
           match move_pre_check board enemypos kingpos enemycolor with
           | Legal _ -> true
           | _ -> false)
        enemies
    in
    match possiblecheck with
    | None -> false
    | Some _ -> true

  let move board pos1 pos2 pc =
    match move_pre_check board pos1 pos2 pc with
    | Illegal msg -> Illegal msg
    | Legal _ -> begin
        let piece = force_get_piece pos1 board in
        let moved = board |> remove_piece pos1 |> set_piece pos2 piece in
        if (determine_check moved pc) then 
          Illegal "Invalid move; you would either remain in or enter check"
        else
          Legal moved
      end

  let check_move board pos1 pos2 pc=
    match move board pos1 pos2 pc with
    | Legal _ -> true
    | Illegal _ -> false

  let castle q_side c board =
    if determine_check board c then Illegal "Can't castle while in check"
    else match (q_side, c) with
      | (true, White) ->
        let can_move = 
          (is_piece ('A', 1) None board)
          && (is_piece ('A', 2) None board)
          && (is_piece ('A', 3) None board)
          && (is_piece ('A', 0) (Some Rook) board)
          && (is_piece ('A', 4) (Some King) board)
          && check_start_piece board ('A', 0) White
          && check_start_piece board ('A', 4) White
        in
        let move_1 = 
          board 
          |> remove_piece ('A', 4) 
          |> set_piece ('A', 3) {c = White; v = King}
        in
        let move_2 = 
          move_1
          |> remove_piece ('A', 3) 
          |> set_piece ('A', 2) {c = White; v = King}
        in
        if not can_move then 
          Illegal "Can't castle: pieces in way, or rook+king not in position"
        else if determine_check move_1 White then 
          Illegal "Can't castle through check"
        else if determine_check move_2 White then 
          Illegal "Can't castle into check"
        else
          Legal (move_2 
                 |> remove_piece ('A', 0) 
                 |> set_piece ('A', 3) {c = White; v = Rook})
      | (false, White) ->
        let can_move = 
          (is_piece ('A', 5) None board)
          && (is_piece ('A', 6) None board)
          && (is_piece ('A', 7) (Some Rook) board)
          && (is_piece ('A', 4) (Some King) board)
          && check_start_piece board ('A', 7) White
          && check_start_piece board ('A', 4) White
        in
        let move_1 = 
          board 
          |> remove_piece ('A', 4) 
          |> set_piece ('A', 5) {c = White; v = King}
        in
        let move_2 = 
          move_1
          |> remove_piece ('A', 5) 
          |> set_piece ('A', 6) {c = White; v = King}
        in
        if not can_move then 
          Illegal "Can't castle: pieces in way, or rook+king not in position"
        else if determine_check move_1 White then 
          Illegal "Can't castle through check"
        else if determine_check move_2 White then 
          Illegal "Can't castle into check"
        else
          Legal (move_2 
                 |> remove_piece ('A', 7) 
                 |> set_piece ('A', 5) {c = White; v = Rook})
      | (true, Black) ->
        let can_move = 
          (is_piece ('H', 4) None board)
          && (is_piece ('H', 5) None board)
          && (is_piece ('H', 6) None board)
          && (is_piece ('H', 7) (Some Rook) board)
          && (is_piece ('H', 3) (Some King) board)
          && check_start_piece board ('H', 7) Black
          && check_start_piece board ('H', 3) Black
        in
        let move_1 = 
          board 
          |> remove_piece ('H', 3) 
          |> set_piece ('H', 4) {c = Black; v = King}
        in
        let move_2 = 
          move_1
          |> remove_piece ('H', 4) 
          |> set_piece ('H', 5) {c = Black; v = King}
        in
        if not can_move then 
          Illegal "Can't castle: pieces in way, or rook+king not in position"
        else if determine_check move_1 Black then 
          Illegal "Can't castle through check"
        else if determine_check move_2 Black then 
          Illegal "Can't castle into check"
        else
          Legal (move_2 
                 |> remove_piece ('H', 7) 
                 |> set_piece ('H', 4) {c = Black; v = Rook})
      | (false, Black) ->
        let can_move = 
          (is_piece ('H', 2) None board)
          && (is_piece ('H', 1) None board)
          && (is_piece ('H', 0) (Some Rook) board)
          && (is_piece ('H', 3) (Some King) board)
          && check_start_piece board ('H', 0) Black
          && check_start_piece board ('H', 3) Black
        in
        let move_1 = 
          board 
          |> remove_piece ('H', 3) 
          |> set_piece ('H', 2) {c = Black; v = King}
        in
        let move_2 = 
          move_1
          |> remove_piece ('H', 2) 
          |> set_piece ('H', 1) {c = Black; v = King}
        in
        if not can_move then 
          Illegal "Can't castle: pieces in way, or rook+king not in position"
        else if determine_check move_1 Black then 
          Illegal "Can't castle through check"
        else if determine_check move_2 Black then 
          Illegal "Can't castle into check"
        else
          Legal (move_2 
                 |> remove_piece ('H', 0) 
                 |> set_piece ('H', 2) {c = Black; v = Rook})



end