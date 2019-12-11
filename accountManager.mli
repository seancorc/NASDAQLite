open Account

(** [InvalidUsername s] is an exception indicating that the username 
    used was not accepted and [s] is the reason why *)
exception Invalid_username of string

(** [InvalidPassword] is an exception indicating that the password used was 
    invalid *)
exception Invalid_password


(** [AccountManager] is the signature for the module type that will manage 
    the creation and manipulation of accounts *)
module type AccountManager = sig 

  (** [t] is the type of AccountManager. *)
  type t

  (** [to_json_string am] is the string representation of account manager [am] 
      and is compatible with the Yojson.Basic.from_string function. *)
  val to_json_string : t -> string

  (** [create ()] is an empty account manager *)
  val create : unit -> t

  (** [register am username password] is the account with username [username]
      which belongs to account manager am. 
      Raises: InvalidUsername if there already exists an account with username 
      [username] in am *)
  val register : t -> string -> string -> Account.t

  (** [load_from_json json] is the AccountManager prepopulated from a compatibly
      formatted json. Behavior is not defined if json is formatted incorrectly.
      Example JSON: "users": [
      {
      "username": "name",
      "hashed_pass": "pass",
      "balance": 100.0,
      "orders": [
        {
          "ticker": "GOOG",
          "amount": 1
        }
      ]
      }] *)
  val load_from_json : Yojson.Basic.t -> t

  (** [set_account_balance am username balance] changes the balance of the
      account that belongs to AccountManager [am] with username [username] 
      to a new value of [balance] 
      Raises: Not_found if there does not exist an account with username 
      [username] in AccountManager [am] *)
  val set_account_balance : t -> string -> float -> unit

  (** [inc_account_balance am username amount] adds [amount] to the balance of 
      the account that belongs to AccountManager [am] with username [username]  
      Raises: Not_found if there does not exist an account with username 
      [username] in AccountManager [am] *)
  val inc_account_balance : t -> string -> float -> unit

  (** [dec_account_balance am username amount] subtracts [amount] from the 
      balance of the account that belongs to AccountManager [am] with username
      [username]  
      Raises: Not_found if there does not exist an account with username 
      [username] in AccountManager [am] *)
  val dec_account_balance : t -> string -> float -> unit

  (** [set_account_position am username ticker amount] changes the number of 
      positions of the account that belongs to AccountManager [am] with 
      username [username] for ticker [ticker] to a new value of [amount] 
      Raises: Not_found if there does not exist an account with username 
      [username] in AccountManager [am] *)
  val set_account_position : t -> string -> string -> int -> unit

  (** [set_account_position am username ticker amount] adds [amount] to the 
      number of positions of the account that belongs to AccountManager [am] with 
        username [username] for ticker [ticker].
        Raises: Not_found if there does not exist an account with username 
        [username] in AccountManager [am] *)
  val inc_account_position : t -> string -> string -> int -> unit

  (** [set_account_position am username ticker amount] subtracts [amount] from 
      the number of positions of the account that belongs to AccountManager [am]
       with username [username] for ticker [ticker].
      Raises: Not_found if there does not exist an account with username 
          [username] in AccountManager [am] *)
  val dec_account_position : t -> string -> string -> int -> unit

  (** [login am username password] is the account with username [username] and
        password [password] that belongs to AccountManager [am]. 
        Raises: Invalid_username if there does not exist an account with 
        username [username] in AccountManager [am].
        Raises: Invalid_password if [password] is not the password associated
        with the account with username [username]. *)
  val login : t -> string -> string -> Account.t

  (** [delete_user am username password] deletes the account with username 
      [username] from account manager [am]. 
      Raises: Invalid_username if there does not exist an account with 
        username [username] in AccountManager [am].
      Raises: Invalid_password if [password] is not the password associated
        with the account with username [username].*)
  val delete_user : t -> string -> string -> unit

  (** [accounts am] is a string list containing the usernames of each account
      in [am]. 
      Example: ["sean", "jack", "aaron"] *)
  val accounts : t -> string list
end

(** [AccountManager] is the implementation of the [AccountManager] signature *)
module AccountManager : AccountManager