(** The Order Book *)

(** [order_type] is the direction of a single order: Buy or Sell. *)
type order_type = Buy | Sell

(** [order] is a single order element of the order book that holds the asset,
    price, order type, and username of the order.  *)
type order = {
  asset: string;
  price: float;
  order_type: order_type;
  username: string
}

(** [OrderBook] is the signature for the module type that will hold all of the 
    orders in the market. *)
module type OrderBook = sig

  (** [t] is the type of order books. *)
  type t

  (** [empty] is the empty order book. *)
  val empty : t

  (** [is_empty ob] is [true] iff [ob] is empty. *)
  val is_empty : t -> bool

  (** [size ob] is the number of orders in [ob]. *
      [size empty] is [0]. *)
  val size : t -> int

  (** [insert o ob] is [ob] with order [o] inserted. *)
  val insert : order -> t -> t

  (** [member o ob] is [true] iff order [o] is in [ob]. *)
  val member : order -> t -> bool

  (** [get_complement_order o] is the order with all fields equal beside an
      opposite order_type *)
  val get_complement_order : order -> t -> order option

  (** [remove o ob] contains all the orders of [ob] except
      order [ob].  If [o] is not in [ob], then
      [remove] returns an order book with the same orders
      as [ob]. *)
  val remove : order -> t -> t
end

(** [OrderBook] is the implementation of the [OrderBook] signature *)
module OrderBook : OrderBook