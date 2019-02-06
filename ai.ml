open Board
open Vector

module type Ai = sig
  exception GameOver
  exception NoMoves

  val get_move : GameBoard(MapVector).t  
    -> GameBoard(MapVector).color 
    -> GameBoard(MapVector).position*GameBoard(MapVector).position

end

module AdvancedAI : Ai = struct

  exception GameOver
  exception NoMoves

  module B = GameBoard(MapVector)

  (*None is the top node*)
  type move = 
    |None 
    |Some of {cin: (int*int); cout: (int*int)}

  type state = {m : move; b: B.t;  util: int; c : B.color}

  type t  = LeafNode of {st:state} | Node of {tree: t list; state: state}

  type full_move = {pin:(int*int); pout:(int*int); util:int; p : B.piece}

  (*Next three convert a full move to move, utility, or negative utility*)
  let fmtm fm = 
    match (fm.pin, fm.pout) with
    | (a,b) -> Some {cin =a; cout =b}
  let fmtu fm = fm.util
  let fmtnu fm = -1*fm.util

  (*Util of simply forward pawn move, max depth of tree, the utility of check*)
  let pawn_fwd = 3
  let max_depth = 3
  let check_util = -10000

  (*Capture utility: Utility for capturing a piece*)
  let get_util (p:B.value) = 
    match p with
    | Pawn -> 15
    | King -> 70
    | Queen -> 50
    | Rook -> 22
    | Knight ->20
    | Bishop -> 25

  (*Debug helper*)
  let match_color c=
    match c with
    |B.Black -> "B"
    |B.White -> "W"

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
    | _ -> raise 
          (invalid_arg "Can't get index for this number, board is 8x8 In ai")

  (**[pos_to_char] converts and integer to a char given our coordinate scheme*)
  let pos_to_char = function
    | 0 -> 'A'
    | 1 -> 'B'
    | 2 -> 'C'
    | 3 -> 'D'
    | 4 -> 'E'
    | 5 -> 'F'
    | 6 -> 'G'
    | 7 -> 'H'
    | _ -> failwith "shouldnt happen"

  (*Converts move start and end positions respectively to type positon*)
  let move_in_to_position m : (B.position)=
    match m with
    |None -> ('Z', 10)
    |Some(m) -> ((pos_to_char (fst m.cin)),(snd m.cin))
  let move_out_to_position m : (B.position)=
    match m with
    |None -> ('Z', 10)
    |Some(m) -> ((pos_to_char (fst m.cout)),(snd m.cout))

  (*Checks if move is vailid*)
  let check_move board pos1 pos2 pc=
    match B.move board pos1 pos2 pc with
    | Legal _ -> true
    | Illegal _ -> false

  (*Helper to call check move with cords(int*int)*)
  let check_move_cord cord1 cord2 b color= 
    let p1 = ((pos_to_char (fst cord1)),(snd cord1)) in 
    let p2 = ((pos_to_char (fst cord2)),(snd cord2)) in 
    check_move b p1 p2 color

  (*Gets a piece from a cord*)
  let get_piece_cord cord b = 
    B.get_piece ((pos_to_char (fst cord)),(snd cord)) b

  (*Helper for pawn_side to better handle edges*)
  let get_pawn_piece_left b row col : B.piece option= 
    if col = 0 then None 
    else
      B.get_piece ((pos_to_char row),col-1) b
  let get_pawn_piece_right b row col : B.piece option= 
    if col = 7 then None 
    else
      B.get_piece ((pos_to_char row),col+1) b

  (*Check for piece of oponent color in (row),(col +1, -1)*)
  let pawn_sides b color row col : ((int*int)*int) list= 
    let pl = get_pawn_piece_left b row col in
    let pr = get_pawn_piece_right b row col in
    match pl with
    |None ->(
        match pr with
        | None -> []
        | Some(pi) -> if (B.get_piece_col pi) <> color 
          then ((row, col+1), (get_util (B.get_piece_val pi))) :: []
          else [])
    |Some(po) when (B.get_piece_col po) <> color -> (
        match pr with
        |None -> ((row, col-1), (get_util (B.get_piece_val po))) :: []
        |Some(pi)-> if (B.get_piece_col pi) <> color 
          then ((row, col+1), (get_util (B.get_piece_val pi))) 
               :: ((row, col-1),(get_util (B.get_piece_val po))) :: []
          else ((row, col-1), (get_util (B.get_piece_val po))) :: [])
    |Some(po) ->   (
        match pr with
        | None -> []
        | Some(pi) -> if (B.get_piece_col pi) <> color 
          then ((row, col+1), (get_util (B.get_piece_val pi))) :: []
          else [])
  (*Finds all moves possible for a pawn and calculates their utility*)
  let pawn_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list= 
    let f = (fst cord) in
    let s = (snd cord) in
    let check =  (fun cin -> check_move_cord cord cin b color) in
    let side = (fun rin -> pawn_sides b color rin s) in 
    match color with
    | White when (f = 1) -> if (check (3,s)) 
      then ((3, s), (2 * pawn_fwd)) :: [] 
      else (side (f+1))
    | White when (f = 4)-> if (check (f+1, s)) 
      then ((f+1, s), 15) :: (side (f+1)) 
      else (side (f+1))
    | White when (f = 5)-> if (check (f+1, s)) 
      then ((f+1, s), 20) :: (side (f+1)) 
      else (side (f+1))
    | White when (f = 6)-> if (check (f+1, s)) 
      then ((f+1, s), 40) :: (side (f+1)) 
      else (side (f+1))
    | White -> if (check (f+1, s)) 
      then ((f+1, s), pawn_fwd) :: (side (f+1)) 
      else (side (f+1))
    | Black when (f = 6) -> if (check (4, s)) 
      then ((4, s),(2*pawn_fwd)) :: [] 
      else (side (f-1))
    | Black when (f = 3)-> if (check (f-1, s)) 
      then ((f-1, s), 15) :: (side (f-1)) 
      else (side (f-1))
    | Black when (f = 2)-> if (check (f-1, s)) 
      then ((f-1, s), 20) :: (side (f-1)) 
      else (side (f-1))
   | Black when (f = 1)-> if (check (f-1, s)) 
      then ((f-1, s), 40) :: (side (f-1)) 
      else (side (f-1))
    | Black -> if (check (f-1, s)) 
      then ((f-1, s), pawn_fwd) :: (side (f-1)) 
      else (side (f-1))

  (*Helper function to ensure location is valid*)
  let outside_board cord = 
    match cord with
    | (a,b) when (-1< a) && (-1< b) && (a < 8) && (b < 8) -> 
      false
    | _ -> true

  (*Used with direction incrementers bellow to find all moves in give direction
    Can be one of many differnt directions (NSEW+diag+knight moves )*)
  let rec check_dir board color cord f (accum: ((int*int)*int) list ) 
    : ((int*int)*int) list = 
    let cout = f cord in 
    if outside_board cout then accum else
      let p = get_piece_cord cout board in
      match p with
      | None -> check_dir board color cout f (((cout), 0)::accum)
      | Some(pn) when (B.get_piece_col pn) <> color -> 
        (cout, (get_util (B.get_piece_val pn))) :: accum
      | Some(pn) -> accum

  (*For piecs that can move only 1 space ie king*)
  let single_check board color cord f (accum: ((int*int)*int) list ) 
    : ((int*int)*int) list = 
    let cout = f cord in 
    if outside_board cout then accum else
      let p = get_piece_cord cout board in
      match p with
      | None -> ((cout), 0)::accum
      | Some(pn) when (B.get_piece_col pn) <> color -> 
        (cout, (get_util (B.get_piece_val pn))) :: accum
      | Some(pn) -> accum

  (*check_dir and single_check helper functions to increment positon to check*)
  let north_incr = (fun c -> ((fst c)+1, snd c))
  let south_incr = (fun c -> ((fst c)-1, snd c))
  let west_incr = (fun c -> (fst c, (snd c)-1))
  let east_incr = (fun c -> (fst c, (snd c)+1))
  let nw_incr = (fun c -> ((fst c)+1, (snd c)-1))
  let ne_incr = (fun c -> ((fst c)+1, (snd c)+1))
  let sw_incr = (fun c -> ((fst c)-1, (snd c)-1))
  let se_incr = (fun c -> ((fst c)-1, (snd c)+1))

  (*8 possible knight moves*)
  let nw_knight c = c |> nw_incr |> north_incr
  let ne_knight c = c |> ne_incr |> north_incr
  let wn_knight c = c |> nw_incr |> west_incr
  let ws_knight c = c |> sw_incr |> west_incr
  let sw_knight c = c |> sw_incr |> south_incr
  let se_knight c = c |> se_incr |> south_incr
  let es_knight c = c |> se_incr |> east_incr
  let en_knight c = c |> ne_incr |> east_incr

  (*Next 5 function call single_check or check_dir with appropriate increment 
    helper*)
  let rook_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list = 
    let check = (fun f -> check_dir b color cord f) in
    [] |> check north_incr |> check south_incr 
    |> check west_incr |> check east_incr

  let queen_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list = 
    let check = (fun f -> check_dir b color cord f) in
    [] |> check north_incr |> check south_incr |> check west_incr 
    |> check east_incr |> check nw_incr |> check ne_incr 
    |> check sw_incr |> check se_incr 

  let bishop_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list = 
    let check = (fun f -> check_dir b color cord f) in
    [] |> check nw_incr |> check ne_incr |> check sw_incr |> check se_incr 

  let king_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list = 
    let check = (fun f -> single_check b color cord f) in
    [] |> check north_incr |> check south_incr |> check west_incr 
    |> check east_incr |> check nw_incr |> check ne_incr |> check sw_incr 
    |> check se_incr 

  let knight_moves (b:B.t) (color : B.color) (cord:(int*int)) 
    : ((int*int)*int) list = 
    let check = (fun f -> single_check b color cord f) in
    [] |> check nw_knight |> check ne_knight |> check wn_knight 
    |> check ws_knight |> check sw_knight |> check se_knight 
    |> check es_knight |> check en_knight 

  (*Type implemented to make checking for move easier*)
  type spot = {p:B.piece; x:int; y:int}

  (*Used to change a cord st it explores from top to bottom each 
    row from left to right *)
  let incr_loc (l:int*int) =
    match l with
    |(x, 7) -> ((x+1), 0)
    |(x, y) -> (x, (y+1)) 

  (*Type conversion helpers*)
  let cord_to_poss (c:int*int) = 
    ((pos_to_char (fst c)), snd c)
  let spot_to_cord s = (s.x, s.y)

  (*Gets a list of c color pieces*)
  let rec get_pieces (c:B.color) b (accum: spot list) xi yi=
    (* print_string " [";
       print_string (match_color c);
       print_string "] "; *)
    if xi = 8 then accum else
      let next_loc = incr_loc (xi,yi) in
      match (B.get_piece (cord_to_poss (xi, yi)) b) with
      |None -> get_pieces c b accum (fst next_loc) (snd next_loc)
      |Some(pi) -> if ((B.get_piece_col pi) = c )
        then get_pieces c b ( {p = pi;x =xi;y = yi} :: accum) 
            (fst next_loc) (snd next_loc)
        else get_pieces c b accum (fst next_loc) (snd next_loc)


  (*Finds all possible moves for given pieces*)
  let rec get_moves (p: spot list) b c (accum: full_move list):full_move list=
    match p with
    | [] -> accum
    |h::t ->let cin = spot_to_cord h in  
      let tfm = fun t -> {pin = cin; pout = (fst  t); util = (snd t); p = h.p} 
      in
      match (B.get_piece_val h.p) with
      | Pawn -> (get_moves t b c (List.map tfm 
                                    (pawn_moves b c cin)) @ accum) 
      | Rook -> (get_moves t b c (List.map tfm 
                                    (rook_moves b c cin)) @ accum)
      | King -> (get_moves t b c (List.map tfm 
                                    (king_moves b c cin)) @ accum)
      | Queen -> (get_moves t b c (List.map tfm 
                                     (queen_moves b c cin)) @ accum)
      | Bishop -> (get_moves t b c (List.map tfm 
                                      (bishop_moves b c cin)) @ accum)
      | Knight -> (get_moves t b c (List.map tfm 
                                      (knight_moves b c cin)) @ accum)

  (*incs color*)
  let next_color c= 
    match c with
    |B.Black -> B.White
    |B.White -> B.Black

  (*Debug helper*)
  let move_debug m = 
    match m with
    |None -> "No move"
    |Some(m) -> (string_of_int (fst m.cin))^(string_of_int (snd m.cin))

  (*Helper function to call B.move*)
  let move_ai b curr_move c depth= 
    (* print_int depth;
       print_string " "; *)
    if (curr_move = None) then (1, b) else
      let new_b = B.move b (move_in_to_position curr_move) 
          (move_out_to_position curr_move) c in
      (* print_string (move_debug curr_move);
         print_string " "; *)
      match new_b with
      | B.Illegal(s) -> (2, b)
      | B.Legal(nb) -> (3, nb)

  (*NOT CURRENTLY USED: was to computationally intense
    Function to add some entropy to ai*)
  let rec random_modifier accum fml = 
    let y = Random.int 10 in
    let x = y - 5 in
    match fml with
    |[] -> accum
    |h::t -> random_modifier t 
               (accum @[{pin = h.pin; pout = h.pout;
                         util = (h.util+x); p = h.p}])

  (*Adds extra utility to central pawns*)
  let pri_center_pawns (fm: full_move) = 
    if ((B.get_piece_val fm.p) <> B.Pawn) then fm 
    else if ((fst fm.pin)<>1)&&((fst fm.pin)<>6) then fm else
      match snd fm.pin with
      |2 -> {pin = fm.pin; pout = fm.pout; util = (fm.util+1); p = fm.p}
      |3 -> {pin = fm.pin; pout = fm.pout; util = (fm.util+2); p = fm.p}
      |4 -> {pin = fm.pin; pout = fm.pout; util = (fm.util+2); p = fm.p}
      |5 -> {pin = fm.pin; pout = fm.pout; util = (fm.util+1); p = fm.p}
      |_-> fm

  (*Filter function to modify utilies if desired/neccesarry*)
  let filter fml board color depth : full_move list= 
    if depth > 0 then fml else 
      fml |> (List.map pri_center_pawns)

  (*Input is the state *)
  let rec build_tree (curr_move: move) board node_color ut depth max_depth=
    (* print_string "[";
       print_string (match_color node_color); *)
    if depth > max_depth 
    then LeafNode{st = {m=curr_move;b=board; util= ut; c =node_color}} 
    else
      let child_color = next_color node_color in
      let got_pieces = get_pieces node_color board [] 0 0 in
      let all_moves_pre_filter = get_moves got_pieces board node_color [] in
      let all_moves = filter all_moves_pre_filter board node_color depth in
      (* print_string " "; *)
      let (success, new_board)= move_ai board curr_move child_color depth in
      (* print_string " ";
         print_int success;
         print_string "]\n"; *)
      if (success <> 3)&&(depth <> 0) then
        LeafNode{st = {m=curr_move;b=board; util= check_util; c =node_color}}
      else
        let build_rec_fst = fun p q -> 
          build_tree p new_board child_color q (depth+1) max_depth in
        let build_rec_new = fun p q -> 
          build_tree p new_board child_color (q+ut) (depth+1) max_depth in

        match curr_move with 
        |None -> Node{ tree = (List.map2 build_rec_fst
                                 (List.map fmtm all_moves) 
                                 (List.map fmtu all_moves));
                       state = {m = None; b = board; util = 0;
                                c = node_color}}
        |Some(cm) when (depth mod 2 = 1 )-> 
          Node{ tree = (List.map2 build_rec_new 
                        (List.map fmtm all_moves) (List.map fmtnu all_moves));
                state = {m = curr_move; b = board; util = ut; c = node_color}}
        |Some(cm) ->
          Node{ tree = (List.map2 build_rec_new 
                        (List.map fmtm all_moves) (List.map fmtu all_moves));
                state = {m = curr_move; b = board; util = ut; c = node_color}}

  type best_path = {m: move list; util: int}

  (*State helper functions*)
  let get_st_util t =
    match t with
    |Node(t) -> t.state.util
    |LeafNode(t) -> t.st.util
  let get_st_move t : move =
    match t with
    |Node(t) -> t.state.m
    |LeafNode(t) -> t.st.m

  (*Finds best move in tree*)
  let rec find_best t (curr_best: best_path) (curr_path: move list)= 
    let m = get_st_move t in 
    let new_curr_path = curr_path @ [m] in
    let fb = fun a b -> find_best b a new_curr_path in
    match t with
    |Node(t) -> List.fold_left fb curr_best t.tree
    |LeafNode(l) -> if((get_st_util t) > curr_best.util) 
      then {m = new_curr_path; util = (get_st_util t)}
      else if ((get_st_util t) = curr_best.util)&&((Random.int 3) = 1)
      then {m = new_curr_path; util = (get_st_util t)}
      else curr_best 
  
  let get_red_num b = 
    (List.length (get_pieces B.White b [] 0 0))
  
  let get_black_num b = 
    (List.length (get_pieces B.Black b [] 0 0))

  let find_max_depth b = 
    let tp = (get_black_num b)+(get_red_num b)in
    if tp > 15 then 3
    else  4
    


  let get_move b c =
    Random.self_init();
    let md = find_max_depth b in
    if ((Random.int 8) = 1)&&(((get_black_num b)< 3)||((get_red_num b)< 3)) 
    then raise NoMoves else
    let tree = build_tree None b c 0 0 md in 
    let best_path = find_best tree {m=[]; util = 0} [] in 
    match best_path.m with
    | [] -> raise NoMoves
    | h::t -> match t with
      | [] -> raise NoMoves
      | h::t ->  ((move_in_to_position h),(move_out_to_position h))

end