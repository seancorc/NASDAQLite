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

(** [State] is the implementation of the [State] signature *)
module State : State