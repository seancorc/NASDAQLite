open Account

(** [InvalidUsername s] is an exception indicating that the username 
    used was not accepted and [s] is the reason why *)
exception InvalidUsername of string

(** [InvalidPassword] is an exception indicating that the password used was 
    invalid *)
exception InvalidPassword


(** [AccountManager] is the signature for the module type that will manage 
    the creation and manipulation of accounts *)
module type AccountManager = sig 
  type t
  val to_json_string : t -> string
  val create : unit -> t
  val register : t -> string -> string -> Account.t
  val load_from_json : Yojson.Basic.t -> t
  val set_account_balance : t -> string -> float -> unit
  val inc_account_balance : t -> string -> float -> unit
  val dec_account_balance : t -> string -> float -> unit
  val set_account_position : t -> string -> string -> int -> unit
  val inc_account_position : t -> string -> string -> int -> unit
  val dec_account_position : t -> string -> string -> int -> unit
  val login : t -> string -> string -> Account.t
  val delete_user : t -> string -> string -> unit
  val accounts : t -> string list
end

(** [AccountManager] is the implementation of the [AccountManager] signature *)
module AccountManager : AccountManager