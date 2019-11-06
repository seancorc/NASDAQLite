open OrderBook


type transaction = order * order 

module type MatchingEngine = sig 

  (** [matchorder obook order] is a pair with the first element representing 
      the list of orders that were successfully matched and 
      the second element representing the resultant orderbook after those 
      orders have been matched. *)
  val matchorder: OrderBook.t -> order -> transaction list * OrderBook.t

end


(** [MatchingEngine] is the implementation of the MatchingEngine signature*)
module MatchingEngine : MatchingEngine