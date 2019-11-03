open OrderBook


module type MatchingEngine = sig 

  (** OBook is a module representing the orderbook the matching engine 
      matches orders against*)
  module OBook : OrderBook

  (** Type [t] is the type of the orderbook we are matching against *)
  type t = OBook.t

  (** [matchorder obook order] is a pair with the first element representing 
      the list of orders that were successfully matched and 
      the second element representing the resultant orderbook after those 
      orders have been matched. *)
  val matchorder: t -> order -> order list * t

end

(** [MakeMatchingEngine] is a functor that takes in an orderbook and returns 
    the implementation of the MatchingEngine signature with that orderbook 
*)
module type MakeMatchingEngine = functor (OBook : OrderBook) -> MatchingEngine