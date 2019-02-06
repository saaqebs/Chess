open OUnit2
open Vector
open Board

module B = GameBoard(MapVector)
module Chess = GameBoard(MapVector)

let make_init_board_test (name:string) (expected) = 
  name >:: fun _ -> assert_equal expected B.init_board  

let make_move_illegal_test (name:string) (input) =
  name >:: fun _ -> 
    match input with
    | Chess.Illegal(_) -> assert_bool "" true
    | _ -> assert_bool "" false

let make_move_legal_test (name:string) (input) =
  name >:: fun _ -> 
    match input with
    | Chess.Illegal(_) -> assert_bool "" false
    | _ -> assert_bool "" true

let make_get_piece_test name input expected = 
  name >:: fun _ -> assert_equal expected input 

let make_get_piece_col_test name input expected = 
  name >:: fun _ -> assert_equal expected input

let make_get_piece_val_test name input expected = 
  name >:: fun _ -> assert_equal expected input

let orig = B.init_board 

(** APLHA TEST CASES **
    let b1_move_1 = 
    match B.move orig ('A',1) ('D',7) with Legal e -> e | _ -> failwith "b1 1"
    let b1_help = 
    match B.move orig ('C',7) ('D',7) with Legal e -> e | 
    _ -> failwith "b1 help"
    let b1_move_2 =
    match B.move b1_help ('A',1) ('C',7) with Legal e -> e | 
    _ -> failwith "b1 2"

    let b2_move = 
    match B.move orig ('G',4) ('B',6) with Illegal e -> e | _ -> failwith "b2"

    let b3_move = 
    match B.move orig ('F',6) ('Z',99) with Illegal e -> e | _ -> failwith "b3"

    let b4_move = 
    match B.move b1_move_2 ('E',2)('Z',99) with Illegal e->e |_->failwith "b4"*)

(** BETA TEST CASES *)
let piece1 = B.get_piece ('A',0) orig 
let piece2 = B.get_piece ('F',5) orig
let piece3 = B.get_piece ('G',3) orig

let piece1_ans = B.get_piece ('A',7) orig
let piece3_ans = B.get_piece ('G',6) orig

let piece1_col = match piece1 with Some e -> B.get_piece_col e |_ ->failwith ""
let piece3_col = match piece3 with Some e -> B.get_piece_col e |_ ->failwith ""

let piece1_val = match piece1 with Some e -> B.get_piece_val e |_ ->failwith ""
let piece3_val = match piece3 with Some e -> B.get_piece_val e |_ ->failwith ""

let board_from_result = function
  | Chess.Legal(b) -> b
  | _ -> failwith ""


let starting =  Chess.init_board
let other_team = Chess.move starting ('B', 0) ('C', 0) Chess.Black
let pass = Chess.move starting ('A', 0) ('A', 0) Chess.White
let self_capture = Chess.move starting ('A', 0) ('B', 0) Chess.White
let out_of_bounds_1 = Chess.move starting ('A', 8) ('C', 1) Chess.White
let out_of_bounds_2 = Chess.move starting ('A', 0) ('C', 8) Chess.White

let impossible_rook = Chess.move starting ('A', 0) ('C', 0) Chess.White
let impossible_bish = Chess.move starting ('A', 2) ('C', 0) Chess.White
let impossible_knt = Chess.move starting ('A', 1) ('C', 1) Chess.White
let impossible_qn_1 = Chess.move starting ('A', 3) ('C', 3) Chess.White
let impossible_qn_2 = Chess.move starting ('A', 3) ('C', 2) Chess.White
let impossible_king = Chess.move starting ('A', 4) ('C', 4) Chess.White
let impossible_pawn_1 = Chess.move starting ('B', 0) ('C', 1) Chess.White
let impossible_pawn_2 = 
  Chess.move
    (board_from_result (Chess.move starting ('B', 0) ('C', 0) Chess.White))
    ('C', 0) 
    ('E', 0) 
    Chess.White

let leg_gen_1 = 
  Chess.move starting ('A', 1) ('C', 0) Chess.White
let leg_gen_2 = 
  Chess.move (board_from_result leg_gen_1) ('H', 1) ('F', 2) Chess.Black
let leg_gen_3 = 
  Chess.move (board_from_result leg_gen_2) ('C', 0) ('E', 1) Chess.White
let leg_gen_4 = 
  Chess.move (board_from_result leg_gen_3) ('F', 2) ('D', 3) Chess.Black
let leg_gen_5 = 
  Chess.move (board_from_result leg_gen_4) ('B', 1) ('D', 1) Chess.White
let leg_gen_6 = 
  Chess.move (board_from_result leg_gen_5) ('D', 3) ('C', 5) Chess.Black
let leg_gen_7 = 
  Chess.move (board_from_result leg_gen_6) ('B', 4) ('C', 5) Chess.White
let leg_gen_8 = 
  Chess.move (board_from_result leg_gen_7) ('G', 3) ('E', 3) Chess.Black
let leg_gen_9 = 
  Chess.move (board_from_result leg_gen_8) ('A', 4) ('B', 4) Chess.White
let leg_gen_10 = 
  Chess.move (board_from_result leg_gen_9) ('H', 4) ('E', 1) Chess.Black
let leg_gen_11 = 
  Chess.move (board_from_result leg_gen_10) ('B', 4) ('A', 4) Chess.White
let leg_gen_12 = 
  Chess.move (board_from_result leg_gen_11) ('H', 2) ('C', 7) Chess.Black
let leg_gen_13 = 
  Chess.move (board_from_result leg_gen_12) ('A', 0) ('A', 1) Chess.White

let check_illegal =
  Chess.move (board_from_result leg_gen_6) ('A', 2) ('C', 0) Chess.White

let board_tests = [


  (* make_init_board_test "basic" orig;
     make_move_test "move 1" b1_move_1 b1_move_2;
     make_move_test "move 2" b2_move ("Full spot, Cannot move here");
     make_move_test "m 3" b3_move "Cannot get index for this number, 
      board is 8x8";
     make_move_test "move 4 " b4_move "No piece to move"; *)


  make_get_piece_test "random 1" piece1 piece1_ans;
  make_get_piece_test "random 2" piece2 None;
  make_get_piece_col_test "color 1" piece1_col B.White;
  make_get_piece_col_test "color 3" piece3_col B.Black;
  make_get_piece_val_test "value 1" piece1_val B.Rook;
  make_get_piece_val_test "value 3" piece3_val B.Pawn;

  make_move_illegal_test "other" other_team;
  make_move_illegal_test "pass" pass;
  make_move_illegal_test "self" self_capture;
  make_move_illegal_test "bounds 1" out_of_bounds_1;
  make_move_illegal_test "bounds 2" out_of_bounds_2;
  make_move_illegal_test "rook" impossible_rook;
  make_move_illegal_test "bish" impossible_bish;
  make_move_illegal_test "qn1" impossible_qn_1;
  make_move_illegal_test "qn2" impossible_qn_2;
  make_move_illegal_test "king" impossible_king;
  make_move_illegal_test "pawn1" impossible_pawn_1;
  make_move_illegal_test "pawn2" impossible_pawn_2;
  make_move_illegal_test "check" check_illegal;

  make_move_legal_test "legal1" leg_gen_1;
  make_move_legal_test "legal2" leg_gen_2;
  make_move_legal_test "legal3" leg_gen_3;
  make_move_legal_test "legal4" leg_gen_4;
  make_move_legal_test "legal5" leg_gen_5;
  make_move_legal_test "legal6" leg_gen_6;
  make_move_legal_test "legal7" leg_gen_7;
  make_move_legal_test "legal8" leg_gen_8;
  make_move_legal_test "legal9" leg_gen_9;
  make_move_legal_test "legal10" leg_gen_10;
  make_move_legal_test "legal11" leg_gen_11;
  make_move_legal_test "legal12" leg_gen_12;
  make_move_legal_test "legal13" leg_gen_13;
]

let suite =
  "test suite for chess"  >::: List.flatten [
    board_tests
  ]

let _ = run_test_tt_main suite