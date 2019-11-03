open OrderBook


module type MatchingEngine = sig 
  module OBook : OrderBook
  type t
  val matchorder: t -> order -> transaction list * t
end

module MakeMatchingEngine = functor (OBook : OrderBook) -> struct

  module OBook = OBook

  type t = OBook.t

  let matchorder obook order =
    let complement_type = if order.order_type = Buy then Sell else Buy in 
    let complement_order = {asset=order.asset;price=order.price;order_type=complement_type} in
    if Obook.mem (complement_order) then [order; complement_order], (OBook.remove obook)
    else [], (Obook.insert order)
end
