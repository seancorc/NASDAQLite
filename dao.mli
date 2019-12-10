open AccountManager

module type Dao = sig 
  val get_account_data : unit -> Yojson.Basic.t
  val write_account_manager_data : string -> unit
  val get_account_balance : string -> Yojson.Basic.t
  val get_account_positions : string -> Yojson.Basic.t
  val signup_user : string -> string -> unit
  val get_engine_data : unit -> Yojson.Basic.t
  val write_engine_data : string -> unit
  val add_order : string -> string -> string -> string -> string -> unit
end

module Dao : Dao