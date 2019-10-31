open OrderBook


type t = OrderBook.t

let matchorder obook order =
  let complement_type = if order.order_type = Buy then Sell else Buy in 
  let complement_order = {asset=order.asset;price=order.price;order_type=complement_type} in
  if OrderBook.mem (complement_order) then [order; complement_order] * (OrderBook.remove obook)
  else (OrderBook.insert order)

