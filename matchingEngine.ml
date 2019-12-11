open OrderBook
open AccountManager
open Yojson.Basic.Util
open Dao

module type MatchingEngine = sig 
  type t
  val orderbooks_to_json_string : t -> string
  val create : unit -> t
  val member : t -> string -> bool
  val execute_regular_order : t -> order_direction -> order -> string -> unit
  val execute_market_order : t -> order_direction -> string -> int -> string -> unit
  val tickers : t -> string list
  val get_order_book : t -> string -> OrderBook.t
  val load_from_json : Yojson.Basic.t -> t
  val get_account_manager : t -> AccountManager.t
  val set_account_manager : t -> AccountManager.t -> t
  val delete_user : t -> string -> string -> unit
  val add_asset : t -> string -> unit
end

exception UnboundTicker

(** [StringHash] is a module representing a HashedType using String as the key*)
module StringHash = struct
  type t = string

  (** [equal i j] is true if i is structurally equal to j and false otherwise *)
  let equal i j = i = j

  (** [hash i] is i after running a hashing algorithm on it for use in 
      Hashtbl *)
  let hash i = Hashtbl.hash i
end


module D = Hashtbl.Make(StringHash)

module MatchingEngine : MatchingEngine = struct

  type t = {
    orderbooks: OrderBook.t D.t;
    account_manager: AccountManager.t;
  }

  let aapl = OrderBook.empty

  let no_tickers = D.create 20

  let create _ = 
    let obs = D.create 20 in 
    D.replace obs "AAPL" OrderBook.empty;
    D.replace obs "GOOG" OrderBook.empty;
    D.replace obs "MSFT" OrderBook.empty;
    D.replace obs "AMZN" OrderBook.empty;
    D.replace obs "ROKU" OrderBook.empty;
    let am = AccountManager.create () in 
    {
      orderbooks = obs;
      account_manager = am;
    }

  let add_asset me ticker =
    D.add me.orderbooks ticker OrderBook.empty 

  let get_account_manager (me: t) = me.account_manager

  let set_account_manager me am = {orderbooks=me.orderbooks;account_manager=am}

  let orderbooks_to_json_string (me : t) = 
    let obs = D.fold (fun ticker ob acc -> 
        (ticker, ob) :: acc) me.orderbooks [] in  
    let rec create base_string order_books = 
      match order_books with 
      | [] -> base_string
      | (ticker,ob) :: t -> 
        let ob_string = (OrderBook.to_json_string ob) in
        let ticker_string = "{\n\"ticker\": \"" ^ ticker ^ "\",\n" in 
        create (base_string ^ ticker_string ^ ob_string ^ "}" ^
                (if (List.length t) >= 1 then ",\n" else "\n")) t in
    (create "{\"tickers\": [" obs) ^ "]}"

  let member (me: t) (ticker: string) : bool = 
    let obs = me.orderbooks in D.mem obs ticker

  let tickers (me: t) : string list = 
    let obs = me.orderbooks in 
    D.fold (fun ticker ob acc -> ticker :: acc) obs []

  let get_order_book (me: t) (ticker: string) : OrderBook.t = 
    if member me ticker then 
      let obs = me.orderbooks in 
      let ob = D.find obs ticker in 
      ob
    else raise UnboundTicker

  let receive_order (me: t) (direction: order_direction) (order: order) 
      (ticker: string) : unit =  
    if member me ticker then 
      let obs = me.orderbooks in 
      let ob = D.find obs ticker in 
      let submit_order = (direction, order) in
      let ob' = OrderBook.insert_order ob submit_order in 
      D.replace obs ticker ob'
    else raise UnboundTicker

  let rec populate_orders_for_direction me orders ticker direction =
    match orders with 
    | [] -> ()
    | h :: t -> 
      let username = h |> to_assoc |> List.assoc ("username") |> to_string in 
      let amt = h |> to_assoc |> List.assoc ("amount") |> to_int in 
      let price = h |> to_assoc |> List.assoc ("price") |> to_float in 
      let time = h |> to_assoc |> List.assoc ("time") |> to_float in
      let order = (username,amt,price,time) in
      receive_order me direction order ticker;
      populate_orders_for_direction me t ticker direction;
      ()

  let rec populate_engine me tickers = 
    match tickers with 
    | [] -> ()
    | h :: t -> 
      let ticker = h |> to_assoc |> List.assoc "ticker" |> to_string in
      let _ = if member me ticker = false then add_asset me ticker else () in
      let buys = h |> to_assoc |> List.assoc "buys" |> to_list in
      populate_orders_for_direction me buys ticker Buy;
      let sells = h |> to_assoc |> List.assoc "sells" |> to_list in
      populate_orders_for_direction me sells ticker Sell;
      populate_engine me t;
      ()

  let load_from_json json = 
    let tickers = json |> to_assoc |> List.assoc "tickers" |> to_list in 
    let me = create () in
    populate_engine me tickers;
    me

  let rec repeat_parse (ob: OrderBook.t) (acc: transaction list) 
    : (transaction list) * OrderBook.t = 
    let tx_opt, ob' = OrderBook.construct_tx ob in 
    match tx_opt with 
    | Some tx -> 
      begin
        let acc' = tx :: acc in 
        repeat_parse ob' acc'
      end
    | None -> acc, ob'

  let parse_order_book (me: t) (ticker: string) 
    : (transaction list) * OrderBook.t = 
    let ob = get_order_book me ticker in 
    repeat_parse ob []

  let update_orderbook (me: t) (ticker: string) (ob: OrderBook.t) : unit = 
    D.replace me.orderbooks ticker ob 

  let process_transaction (am: AccountManager.t) (tx: float * int * string * string) (ticker: string) : unit = 
    match tx with 
    | p, amt, b, s -> 
      begin 
        let amt' = float_of_int amt in 
        let total_amount = p *. amt' in 
        let _ = AccountManager.dec_account_balance am b total_amount in 
        let _ = AccountManager.inc_account_balance am s total_amount in 
        let _ = AccountManager.inc_account_position am b ticker amt in 
        let _ = AccountManager.dec_account_position am s ticker amt in 
        ()
      end


  let rec process_transactions (am: AccountManager.t) (txs: transaction list) (ticker: string) : unit = 
    match txs with 
    | tx :: t -> 
      process_transaction am tx ticker;
      process_transactions am t ticker;
      ()
    | [] -> ()

  let clear_market_orders (ob: OrderBook.t) = 
    match OrderBook.best_bid ob, OrderBook.best_offer ob with 
    | Some (_, _, price, _), _ when price = max_float -> 
      let _, ob' = OrderBook.pop_best_bid ob in ob'
    | _, Some (_,_,price,_) when price = min_float -> 
      let _, ob' = OrderBook.pop_best_offer ob in ob'
    | _ -> ob

  let execute_regular_order (me: t) (direction: order_direction) (order: order) 
      (ticker: string) : unit = 
    let am = me.account_manager in 
    let _ = receive_order me direction order ticker in 
    let (txs, ob') = parse_order_book me ticker in 
    let _ = process_transactions am txs ticker in 
    let ob1 = clear_market_orders ob' in
    let _ = update_orderbook me ticker ob1 in
    ()

  let execute_market_order (me: t) (direction: order_direction) (ticker: string) (amount: int) (addr: string): unit = 
    let order = match direction with 
      | Buy -> (addr, amount, max_float, Unix.time ())
      | Sell -> (addr, amount, min_float, Unix.time ())in 
    let _ = execute_regular_order me direction order ticker in 
    ()

  let delete_user me username pass =
    AccountManager.delete_user me.account_manager username pass;
    D.filter_map_inplace (fun k v ->
        Some (OrderBook.delete_user v username)
      ) me.orderbooks;

end
