open OrderBook

type t = OrderBook.t


val matchorder: t -> order -> order list * t