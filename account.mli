module type Account = sig 
  (** [t] is the representation type for Account *)
  type t

  (** [create s] is an Account with username [s] *)
  val create : string -> t

  (** [username a] is the username of account [a] *)
  val username : t -> string

  (** [balance a t] is the balance of account [a] for the ticker [t] *)
  val balance : t -> string -> float

  (** [balances a] is an association list of all of the balances held by 
      account [a] in the format (ticker, balance) *)
  val balances : t -> (string * float) list

  (** [set_balance a t v] is unit after changing the balance of account [a]
      for ticker [t] to be set to the value [v] 
      Requires: [v] is greater than or equal to 0 *)
  val set_balance : t -> string -> float -> unit

  (** [format fmt a] is unit. Compatible with top-level's printing function
      using the [#install_printer] directive *)
  val format : Format.formatter -> t -> unit
end

(** [Account] is the implementation of the Account signature *)
module Account : Account