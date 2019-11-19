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
  (* * [t] is the representation type of AccountManager
     type t

     (** [create ()] is a new Account Manager instance *)
     val create : unit -> t

     (** [register m u p] is an Account with username [u] which has been created 
      and is managed by [m], accessible with the password [p] *)
     val register : t -> string -> string -> Account.t

     (** [set_account_balance m u t a] is unit. Side effect: updates the account
      balance of account with username [u] for ticker [t] to amount [a].
      Requires: [u] is a username that is held by [m] and [a] is non-negative *)
     val set_account_balance : t -> string -> string -> float -> unit

     (** [inc_account_balance m u t a] is unit. Side effect: increases the
      balance of account with username [u] for ticker [t] by amount [a].
      Requires: [u] is a username that is held by [m] and [a] is non-negative *)
     val inc_account_balance : t -> string -> string -> float -> unit

     (** [dec_account_balance m u t a] is unit. Side effect: decreases the
      balance of account with username [u] for ticker [t] by amount [a].
      Requires: [u] is a username that is held by [m] and [a] is non-negative *)
     val dec_account_balance : t -> string -> string -> float -> unit

     (** [login m u p] is the Account held in [m] with username password combo 
      [u] and [p]
      Raises: [InvalidPassword] if the password is incorrect for this username
      Raises: [InvalidUsername "Username does not exist"] if this username is
      not held by AccountManager [m] *)
     val login : t -> string -> string -> Account.t

     (** [delete_user m u p] is unit. Deletes user [u] from [m] 
      Raises: [InvalidPassword] if the password is incorrect for this username
      Raises: [InvalidUsername "Username does not exist"] if this username is
      not held by AccountManager [m] *)
     val delete_user : t -> string -> string -> unit

     (** [accounts m] is the list of all usernames managed by [m] *)
     val accounts : t -> string list *)
  type t
  val create : unit -> t
  val register : t -> string -> string -> Account.t
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