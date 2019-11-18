open OrderBook
open AccountManager

module type MatchingEngine = sig 
  val matchorder: OrderBook.t -> order -> transaction list * OrderBook.t
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

module MatchingEngine = struct

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

  let member (me: t) ticker = 
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

  let rec process_transactions (am: t) (txs: transaction list) (ticker: string) : unit = 
    match txs with 
    | tx :: t -> 
      let p, amount, addr_b, addr_s = tx in 
      ()
    | [] -> ()

  let execute_regular_order (me: t) (direction: order_direction) (order: order) 
      (ticker: string) : unit = 
    failwith ""



  let execute_market_order direction order ticker : unit = failwith ""


  (* 
  let matchorder obook order =
    match OrderBook.get_complement_order order obook with 
    | None -> 
      [], (OrderBook.insert order obook)
    | Some o ->
      let transaction = [(order, o)] in 
      let first_removed = (OrderBook.remove order obook) in
      let final_book = OrderBook.remove o first_removed in 
      transaction, final_book *)
end
