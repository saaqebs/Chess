
open Game

module type Engine = sig
  val play_with_humans : unit -> unit
  val play_with_AI : unit -> unit
  val play_with_advance_AI : unit -> unit
  val watch_AI : unit -> unit
end


module GameEngine : Engine = struct

  module H = HumanGame
  module EAI = AIGame 
  module AAI = AdvancedAIGame
  module AIvAI = AIvAIGame

  let play_with_humans = H.play 

  let play_with_AI = EAI.play

  let play_with_advance_AI = AAI.play

  let watch_AI = AIvAI.play

end 