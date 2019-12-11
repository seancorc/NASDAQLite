open AccountManager

module type Dao = sig 
  val get_account_balance : string -> Yojson.Basic.t
  val get_account_positions : string -> Yojson.Basic.t
  val signup_user : string -> string -> unit
  val login_user : string -> string -> unit
  val delete_user : string -> string -> unit
  val execute_order : string -> string -> string -> string -> string -> unit
end

module Dao : Dao