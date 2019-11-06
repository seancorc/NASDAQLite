
type order_type = Buy | Sell

type order = {
  asset: string;
  price: float;
  order_type: order_type;
  username: string;
}

module type OrderBook = sig
  type t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : order -> t -> t
  val member : order -> t -> bool
  val remove : order -> t -> t
end

module OrderBook : OrderBook = struct 
  (**
       AF: Represents OrderBook as as a tuple of two lists of type [order] 
       elements. 
       RI: All of the orders in first list are  buy orders and all of the orders
       in the second list are sell orders. *)
  type t = order list * order list

  let empty = ([], [])

  let is_empty = function
    | ([], []) -> true 
    | _ -> false

  let size ((b, s) : t) = 
    List.fold_left (fun acc x -> acc + 1) 0 b + 
    List.fold_left (fun acc x -> acc + 1) 0 s

  (** [get_order_asset o] is the asset of the order [o] *)
  let get_order_asset = function
    | {asset; price; order_type; username} -> asset

  (** [get_order_price o] is the price of the order [o] *)
  let get_order_price = function
    | {asset; price; order_type; username} -> price

  (** [get_order_direction o] is the direction of the order [o] *)
  let get_order_direction = function 
    | {asset; price; order_type; username} -> order_type

  let insert (o : order) ((b, s) : t) = 
    match o with 
    | {asset; price; order_type; username} -> 
      if order_type = Buy then (o :: b, s)
      else (b, o :: s)

  (** [compare_orders o1 o2] is true iff [o1] is identical to [o2]. *)
  let compare_orders (o1 : order) (o2 : order) = 
    if (get_order_asset o1) = (get_order_asset o2) && 
       (get_order_price o1) = (get_order_price o2) && 
       (get_order_direction o1) = (get_order_direction o2) then true else false

  let member_helper (o : order) (lst : order list) = 
    List.fold_left (fun acc elt -> if compare_orders elt o 
                     then true || acc else false || acc) false lst

  let member (o : order) ((b, s) : t) = 
    match o with 
    | {asset; price; order_type = Buy; username} -> 
      List.fold_left (fun acc elt -> if compare_orders elt o 
                       then true || acc else false || acc) false b
    | {asset; price; order_type = Sell; username} ->
      List.fold_left (fun acc elt -> if compare_orders elt o 
                       then true || acc else false || acc) false s

  (** [remove_helper o lst] removes order [o] from order list [lst]. *)
  let remove_helper (o : order) (lst : order list) = 
    List.filter (fun elt -> not (compare_orders elt o)) lst

  let remove (o : order) ((b, s) : t) = 
    match o with 
    | {asset; price; order_type = Buy; username} -> (remove_helper o b, s)
    | {asset; price; order_type = Sell; username} -> (b, remove_helper o s)
end

