module type Account = sig 
  type t
  val to_json_string : t -> string -> string
  val create_empty : string -> t
  val create : string -> float -> t
  val username : t -> string
  val balance : t -> float
  val positions : t -> (string * int) list
  val position : t -> string -> int
  val set_position : t -> string -> int -> unit
  val set_balance : t -> float -> unit
  val format : Format.formatter -> t -> unit
end

(** [Account] is the implementation of the Account signature *)
module Account : Account