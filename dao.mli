open AccountManager

module type Dao = sig 
  val get_account_data : unit -> Yojson.Basic.t
  val write_account_manager_data : string -> unit
  val signup_user : string -> string -> unit
  val get_engine_data : unit -> Yojson.Basic.t
  val add_order : string -> string -> string -> string -> string -> unit
end

module Dao : Dao