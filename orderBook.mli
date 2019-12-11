(** [order_direction] is the direction of a single order: either 
    Buy or Sell. *)
type order_direction = Buy | Sell

(** [order] is a single order element
    address, amount, price, time.  *)
type order = string * int * float * float

(** [transaction] represents a transaction element as
    price, amount, buyer_address, seller_address *)
type transaction = float * int * string * string 

(** [submitted_order] represents an order and its direction *)
type submitted_order = order_direction * order

(** [OrderBook] is the signature for the module type that will store
    all buy and sell orders for a given ticker. *)
module type OrderBook = sig
  (** AF: Represents OrderBook as as a tuple of two lists of type [order] 
      elements. 
      RI: All of the orders in first list are  buy orders and all of the orders
      in the second list are sell orders. 
      Orders are stored in preference order with better prices always at the 
      front of the list. *)

  (** [t] is the type of order books. *)
  type t

  (** [to_json_string ob] is the orderbook [ob] in string json form. *)
  val to_json_string : t -> string

  (** [empty] is the empty order book. *)
  val empty : t

  (** [is_empty ob] is [true] iff [ob] is empty. *)
  val is_empty : t -> bool

  (** [no_buys ob] is true if there are no buy orders in orderbook [ob] and 
      false otherwise. *)
  val no_buys : t -> bool

  (** [no_sells ob] is true if there are no sell orders in orderbook [ob] and 
        false otherwise. *)
  val no_sells : t -> bool

  (** [buys ob] is the order list of buy orders in orderbook [ob.] *)
  val buys : t -> order list

  (** [sells ob] is the order list of sell orders in orderbook [ob.] *)
  val sells : t -> order list

  (** [size ob] is the total number of orders in the orderbook [ob]. *)
  val size : t -> int

  (** [insert_order ob s] is the orderbook [ob] with the submitted order [s]
      inserted. *)
  val insert_order : t -> submitted_order -> t

  (** [best_bid ob] is the best buy order in the orderbook [ob]. [best bid ob]
      is [None] if there are no buy orders in orderbook [ob]. *)
  val best_bid : t -> order option

  (** [best_offer ob] is the best sell order in the orderbook [ob]. 
      [best offer ob] is [None] if there are no sell orders in orderbook [ob].*)
  val best_offer : t -> order option

  (** [pop_best_offer ob] is a tuple of the best sell order in orderbook [ob] 
      and the orderbook with the best sell order removed. The best sell order
      is None if there are no sell orders in orderbook [ob]. *)
  val pop_best_offer : t -> order option * t

  (** [pop_best_bid ob] is a tuple of the best buy order in orderbook [ob] 
        and the orderbook with the best buy order removed. The best buy order
        is None if there are no buy orders in orderbook [ob]. *)
  val pop_best_bid : t -> order option * t

  (** [construct_tx ob] constructs a transaction from orderbook [ob]. 
      [construct_tx ob] is None if no transactions are necessary. *)
  val construct_tx : t -> transaction option * t

  (** [delete_user ob name] if orderbook [ob] with the user with username [name] 
      removed. *)
  val delete_user : t -> string -> t
end

(** [OrderBook] is the implementation of the OrderBook signature *)
module OrderBook : OrderBook
