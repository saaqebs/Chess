module type Vector = sig
  type 'a t

  val empty : int -> 'a t
  val get : int -> 'a t -> 'a option
  val set : int -> 'a -> 'a t -> 'a t
  val length : 'a t -> int
  val del : int -> 'a t -> 'a t
  val iter : ('a -> string) -> 'a t -> unit
end

module MapVector : Vector = struct
  module IntKey = struct
    type t = int 

    let compare = Pervasives.compare
  end

  module IntMap = Map.Make(IntKey)

  type 'a t = {size: int; data: 'a IntMap.t}

  let empty n = {
    size = n;
    data = IntMap.empty
  }

  let get i v =
    if (i >= v.size || i < 0) then raise (Invalid_argument "out of bounds")
    else IntMap.find_opt i v.data

  let set i e v =
    if (i >= v.size || i < 0) then raise (Invalid_argument "out of bounds")
    else {size = v.size; data = IntMap.add i e v.data}

  let length v = 
    v.size

  let del i v = 
    if (i >= v.size || i < 0) then raise (Invalid_argument "out of bounds")
    else {size = v.size; data = IntMap.remove i v.data}

  let iter f v =
    IntMap.iter (fun k e -> print_string (f e^" ")) v.data
end