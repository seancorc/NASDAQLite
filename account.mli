(** [Account] is the signature for the module type that store a single 
    account's information. *)
module type Account = sig 
  (** AF: Represents Accounts by storing its username and balances in a record
      with balances stored in a Hashtbl mapping strings to floats. 
      RI: No additional constraints. *)

  (** [t] is the type of an Account. *)
  type t

  (** [to_json_string a pass] is the json string representation of account [a] 
      with hashed password [pass]. *)
  val to_json_string : t -> string -> string

  (** [create_empty name] is a new account with string username [name] and 0.0
      balance. *)
  val create_empty : string -> t

  (** [create name bal] is a new account with string username [name] and float 
      balance [bal]. *)
  val create : string -> float -> t

  (** [username a] is the string username of account [a]. *)
  val username : t -> string

  (** [balance a] is the current float balance of account [a]. *)
  val balance : t -> float

  (** [positions a] is a list of the current position of account [a] in the
      form [(ticker, size)]. *)
  val positions : t -> (string * int) list

  (** [position a ticker] is the current size of the position of account [a]
      for for ticker [t].
      Raises: UnboundTicker if the ticker is not valid. *)
  val position : t -> string -> int

  (** [set_position a ticker size] sets a position for account [a] in string 
      ticker [ticker] of integer size [size].
      Raises: UnboundTicker if the ticker is not valid. *)
  val set_position : t -> string -> int -> unit

  (** [set_balance a amt] sets the balance of account [a] to float amount 
      [amt]. *)
  val set_balance : t -> float -> unit

  (** [format f] is the orderbook formatted using formatter [f]. *)
  val format : Format.formatter -> t -> unit
end

(** [Account] is the implementation of the Account signature *)
module Account : Account