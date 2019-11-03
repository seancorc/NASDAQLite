module type Account = sig 
  type t
  val create : string -> t
  val username : t -> string
  val balance : t -> string -> float
  val balances : t -> (string * float) list
  val set_balance : t -> string -> float -> unit
  val format : Format.formatter -> t -> unit
end

module StringHash : Hashtbl.HashedType


module Account : Account