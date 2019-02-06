
open Game

(**
   An engine module that runs chess.

   An Engine module that plays two different versions of chess: one
   is a Human vs AI and the other is a Human vs Human.

*)

module type Engine = sig

  (**[play_with_humans ()] runs a full human vs human game of chess. *)
  val play_with_humans : unit -> unit

  (**[play_with_AI ()] runs a full human vs computer game of chess. *)
  val play_with_AI : unit -> unit 

  (**[play_with_AI ()] runs a full human vs advanced computer game of chess. *)
  val play_with_advance_AI : unit -> unit

  (**[watch_AI ()] runs a easy computer vs advanced computer game of chess. *)
  val watch_AI : unit -> unit

end

module GameEngine : Engine