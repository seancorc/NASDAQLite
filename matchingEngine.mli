open OrderBook


(** [UnboundTicker] is an exception indicating that the specified ticker was 
    invalid *)
exception UnboundTicker

module type MatchingEngine = sig 
  (** AF: Represents MatchingEngine by storing the Account Manager and a 
      hash map mapping strings to orders in a record. 
      RI: No additional constraints. *)

  (** [t] is the type of Matching Engine. *)
  type t

  (** [orderbooks_to_json_string me] is a json string of the Order Book in 
      the matching engine [me]. *)
  val orderbooks_to_json_string : t -> string

  (** [create ()] is a new matching engine. *)
  val create : unit -> t

  (** [member me tick] is true iff string [tick] is a ticker available in 
      matching engine [me]. Otherwise, it is false. *)
  val member : t -> string -> bool

  (** [execute_regular_order me d o tick] executes a regular order [o] in 
      direction [d] for string ticker [tick] in matching engine [me]. *)
  val execute_regular_order : t -> order_direction -> order -> string -> unit

  (** [executre_market_order me d tick amt acc] executes a market order for
      string account [acc] of integer amount [amt] in direction [d] for string
      ticker [tick] in matching engine [me]. *)
  val execute_market_order : t -> order_direction -> string -> int -> string 
    -> unit

  (** [tickers me] is a string list of the tickers availale in matching engine
      [me]. *)
  val tickers : t -> string list

  (** [get_order_book me tick] is the order book for string ticker [tick] from 
      matching engine [me]. *)
  val get_order_book : t -> string -> OrderBook.t

  (** [load_from_json j] is the matching engine loaded from the json [j]. *)
  val load_from_json : Yojson.Basic.t -> t  

  (** [get_account_manager me] is the account manager from matching engine 
      [me]. *)
  val get_account_manager : t -> AccountManager.AccountManager.t
  val set_account_manager : t -> AccountManager.AccountManager.t -> t
  val delete_user : t -> string -> string -> unit
  val add_asset : t -> string -> unit
end

(** [MatchingEngine] is the implementation of the MatchingEngine signature *)
module MatchingEngine : MatchingEngine