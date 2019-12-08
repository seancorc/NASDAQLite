open AccountManager

module type Dao = sig 
  val get_account_data : unit -> AccountManager.t
  val signup_user : string -> string -> unit
end

module Dao : Dao