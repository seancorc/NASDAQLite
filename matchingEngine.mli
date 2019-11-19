open OrderBook

module type MatchingEngine = sig 
  type t
  val create : unit -> t
  val member : t -> string -> bool
  val execute_regular_order : t -> order_direction -> order -> string -> unit
  val execute_market_order : t -> order_direction -> string -> int -> string -> unit
  val tickers : t -> string list
  val get_order_book : t -> string -> OrderBook.t
  val get_account_manager : t -> AccountManager.AccountManager.t
end

(** [MatchingEngine] is the implementation of the MatchingEngine signature *)
module MatchingEngine : MatchingEngine