open AccountManager

module type Dao = sig 
  val get_account_data : unit -> Yojson.Basic.t
  val signup_user : string -> string -> unit
  val get_engine_data : unit -> Yojson.Basic.t
end

module Dao : Dao