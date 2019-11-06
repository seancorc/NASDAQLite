open OrderBook


type transaction = order * order 

module type MatchingEngine = sig 
  val matchorder: OrderBook.t -> order -> transaction list * OrderBook.t
end

module MatchingEngine = struct

  let matchorder obook order =
    match OrderBook.get_complement_order order obook with 
    | None -> 
      let _ = print_int (OrderBook.num_buys (OrderBook.insert order obook)) in
      [], (OrderBook.insert order obook)
    | Some o ->
      let transaction = [(order, o)] in 
      let _ = print_int (List.length transaction) in 
      let first_removed = (OrderBook.remove order obook) in
      let final_book = OrderBook.remove o first_removed in 
      let _ = print_int (OrderBook.num_buys final_book) in 
      transaction, final_book
end
