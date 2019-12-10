type order_direction = Buy | Sell

(** username, amount, price, time*)
type order = string * int * float * float

type submitted_order = order_direction * order

(* [(price, amount, address_buyer, address_seller)] *)
type transaction = float * int * string * string 


module type OrderBook = sig
  type t
  val to_json_string : t -> string
  val empty : t
  val is_empty : t -> bool
  val no_buys : t -> bool
  val no_sells : t -> bool
  val buys : t -> order list
  val sells : t -> order list
  val size : t -> int
  val insert_order : t -> submitted_order -> t
  val best_bid : t -> order option
  val best_offer : t -> order option
  val pop_best_offer : t -> order option * t
  val pop_best_bid : t -> order option * t
  val construct_tx : t -> transaction option * t

end

module OrderBook : OrderBook = struct 
  (**
       AF: Represents OrderBook as as a tuple of two lists of type [order] 
       elements. 
       RI: All of the orders in first list are  buy orders and all of the orders
       in the second list are sell orders. 
       Orders are stored in preference order with better prices always at the 
       front of the list. *)
  type t = order list * order list

  let empty = ([], [])

  let is_empty = function
    | ([], []) -> true 
    | _ -> false

  let no_buys = function 
    | ([], _) -> true
    | _ -> false

  let no_sells = function 
    | (_, []) -> true
    | _ -> false

  let buys ((b, _) : t) = 
    b

  let sells ((_, s) : t) = 
    s

  let rec json_string_of_order_list (olist : order list) base_string = 
    match olist with
    | [] ->  base_string
    | (username, amount, price, time) :: t -> 
      let js = "{
      \"username\":\"" ^ username ^"\",
      \"amount\": " ^ (string_of_int amount) ^ ",
      \"price\": " ^ (string_of_float price) ^ "0,
      \"time\": " ^ (string_of_float time) ^ "0}" ^ 
               (if List.length t >= 1 then ",\n" else "\n") in
      json_string_of_order_list t (base_string ^ js)


  let to_json_string ((b,s) : t) =
    let buy_base_string = "\"buys\": [" in
    let buy_json_string = (json_string_of_order_list b buy_base_string) ^ "],\n" in (*Note comma at end*)
    let sell_base_string = "\"sells\": [" in
    let sell_json_string = (json_string_of_order_list s sell_base_string) ^ "]\n" in
    (buy_json_string ^ sell_json_string)

  let size ((b, s) : t) = 
    List.fold_left (fun acc x -> acc + 1) 0 b + 
    List.fold_left (fun acc x -> acc + 1) 0 s

  let compare_buys (o1: order) (o2: order) : int = 
    match o1, o2 with 
    | (_, a1, p1, t1), (_, a2, p2, t2) -> 
      let pcomp = Stdlib.compare p2 p1 in 
      let tcomp = Stdlib.compare t1 t2 in 
      let acomp = Stdlib.compare a2 a1 in 
      if pcomp <> 0 then pcomp 
      else if tcomp <> 0 then tcomp 
      else acomp

  let compare_sells (o1: order) (o2: order) : int = 
    match o1, o2 with 
    | (_, a1, p1, t1), (_, a2, p2, t2) -> 
      let pcomp = Stdlib.compare p1 p2 in 
      let tcomp = Stdlib.compare t1 t2 in 
      let acomp = Stdlib.compare a2 a1 in 
      if pcomp <> 0 then pcomp 
      else if tcomp <> 0 then tcomp 
      else acomp

  let sort_orders (orders: order list) (compare: order -> order -> int) 
    : order list = 
    List.stable_sort compare orders 

  let sort_sells (orders: order list) : order list = 
    sort_orders orders compare_sells

  let sort_buys (orders: order list) : order list = 
    sort_orders orders compare_buys

  let sort_order_book ((b, s): t) : t = 
    let sorted_buys = sort_buys b in 
    let sorted_sells = sort_sells s in 
    (sorted_buys, sorted_sells)

  let insert_buy ((b, s) : t) (o : order) : t = 
    let updated_buys = o :: b in 
    let sorted_buys = sort_buys updated_buys in 
    (sorted_buys, s)

  let insert_sell ((b, s) : t) (o : order) : t = 
    let updated_sells = o :: s in 
    let sorted_sells = sort_sells updated_sells in 
    (b, sorted_sells)

  let insert_order (ob : t) ((dir, o): submitted_order) : t = 
    match dir with 
    | Buy -> insert_buy ob o
    | Sell -> insert_sell ob o

  let best_bid ((b,s) : t) : order option = 
    match b with 
    | h :: t -> Some h
    | _ -> None

  let best_offer ((b,s): t) : order option = 
    match s with 
    | h :: t -> Some h 
    | _ -> None

  let pop_best_bid ((b,s) : t) : order option * t = 
    match b with 
    | h :: t -> ((Some h), (t, s))
    | _ -> (None, (b,s))

  let pop_best_offer ((b,s): t) : order option * t = 
    match s with 
    | h :: t -> ((Some h), (b,t))
    | _ -> (None, (b,s))

  (** Require: both buys and sells are non-empty *)
  let remove_tops ((b,s): t) : t = 
    match b, s with 
    | bh :: bt, sh :: st -> (bt, st)
    | _ -> failwith "Cannot remove tops until at least May"

  let construct_tx (ob: t) : (transaction option * t) = 
    let bb = best_bid ob in 
    let bo = best_offer ob in 
    match bb, bo with 
    | None, _ -> None, ob
    | _, None -> None, ob
    | Some buy, Some sell -> 
      begin
        let (addr_b, amount_b, price_b, time_b) = buy in 
        let (addr_s, amount_s, price_s, time_s) = sell in 
        if price_b >= price_s then 
          begin
            let execution_price = if time_b >= time_s then price_s else price_b in 
            let tx_amount = min amount_s amount_b in 
            let tx = (execution_price, tx_amount, addr_b, addr_s) in 
            let ob' = remove_tops ob in 
            let final_ob = 
              if amount_s = amount_b then ob'
              else if tx_amount = amount_b then let sell' = (addr_s, amount_s - tx_amount, price_s, time_s) in 
                insert_sell ob' sell'
              else let buy' = (addr_b, amount_b - tx_amount, price_b, time_b) in 
                insert_buy ob' buy' in 
            (Some tx, final_ob)
          end
        else None, ob
      end
end

