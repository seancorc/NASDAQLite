(** [order_type] is the direction of a single order: Buy or Sell. *)
type order_direction = Buy | Sell

(** [order] is a single order element
    address, amount, price, time.  *)
type order = string * int * float * float

(** [transaction] represents a transaction element as
    price, amount, buyer_address, seller_address *)
type transaction = float * int * string * string 

(** [submitted_order] represents an order and its direction *)
type submitted_order = order_direction * order


module type OrderBook = sig
  (** [t] is the type of order books. *)
  type t
  (** [empty] is the empty order book. *)
  val empty : t
  (** [is_empty ob] is [true] iff [ob] is empty. *)
  val is_empty : t -> bool
  val no_buys : t -> bool
  val no_sells : t -> bool
  val buys : t -> order list
  val sells : t -> order list
  val size : t -> int
  val insert_order : t -> submitted_order -> t
  val best_bid : t -> order option
  val best_offer : t -> order option
  val pop_best_offer : t -> order option * t
  val pop_best_bid : t -> order option * t
  val construct_tx : t -> transaction option * t
end

module OrderBook : OrderBook
