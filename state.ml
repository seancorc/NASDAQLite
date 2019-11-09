open AccountManager
open Account 
open OrderBook

module type State = sig 
  type t 
  val create_state : AccountManager.t -> OrderBook.t -> t
  val get_user : t -> Account.t option
  val get_manager : t -> AccountManager.t 
  val get_book : t -> OrderBook.t
  val set_user : Account.t option -> t -> t 
  val set_book : OrderBook.t -> t -> t
end

module State : State = struct
  type t = {
    current_user : Account.t option;
    account_manager : AccountManager.t;
    order_book : OrderBook.t;
  }

  let create_state manager book = 
    {
      current_user = None;
      account_manager = manager;
      order_book = book;
    }

  let get_user = function 
    | {current_user; account_manager; order_book} -> current_user

  let get_manager = function 
    | {current_user; account_manager; order_book} -> account_manager

  let get_book = function 
    | {current_user; account_manager; order_book} -> order_book

  let set_user user t = 
    {t with current_user = user}

  let set_book book t = 
    {t with order_book = book}
end 