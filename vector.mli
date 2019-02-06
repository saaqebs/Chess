
(**
   A vector module that maos integers to a value.

   A vector that maps key to values requiring int keys for indexing the 
   vector and values that are type 'a.
*)

module type Vector = sig
  (** A vector that maps key to values requiring int keys for indexing the 
      vector and values that are type 'a. *)
  type 'a t

  (**[empty i] returns an empty vector of length [i]. *)
  val empty : int -> 'a t

  (**[get i v] return the value as an option of a vector [v] at a 
     certain index [i]. *)
  val get : int -> 'a t -> 'a option

  (**[set i e v] sets the value of the corresponding index [i] to the 
     element [e] in a certain vector [v]. *)
  val set : int -> 'a -> 'a t -> 'a t

  (**[length v] returns the length of the vector [v] *)
  val length : 'a t -> int

  (**[del i v] deletes a value at a certain index [i] in vector [v]. *)
  val del : int -> 'a t -> 'a t

  (**[iter f v] prints all the values in vector [v] and using function [f]. *)
  val iter : ('a -> string) -> 'a t -> unit
end

module MapVector : Vector