open AccountManager

module type Dao = sig 
  (** [get_account_balance username] is the json containing the balance of the
      account with username [username] or a json containing an error message
      from the server. *)
  val get_account_balance : string -> Yojson.Basic.t

  (** [get_account_positions username] is the json containing the positions of 
      the account with username [username] or a json containing an error message
      from the server. *)
  val get_account_positions : string -> Yojson.Basic.t

  (** [signup_user username password] makes a POST request to the server to 
      create an account with username [username] and password [password].
      Raises: Invalid_username if there is already an account with username 
      [username].
      Raises: Server_error if there is an internal issue with the server. *)
  val signup_user : string -> string -> unit

  (** [login_user username password] makes a POST request to the server to 
      login an account with username [username] and password [password]. This
      function's main purpose is authenticating credentials for the 
      login process.
      Raises: Invalid_username if there is already an account with username 
      [username].
      Raises: Invalid_password if the password does not match with the account
      associated with username [username].
      Raises: Server_error if there is an internal issue with the server. *)
  val login_user : string -> string -> unit

  (** [delete_user username password] makes a DELETE request to the server to 
      delete the account with username [username] and password [password].
      Raises: Invalid_username if there is already an account with username 
      [username].
      Raises: Invalid_password if the password does not match with the account
      associated with username [username].
      Raises: Server_error if there is an internal issue with the server. *)
  val delete_user : string -> string -> unit

  (** [execute_order username dir ticker amount price] makes a POST request to 
      the server to execute the order with direction [dir], username [username], 
      ticker [ticker], amount [amount], and price [price].
      Raises: Server_error if there is an internal issue with the server. *)
  val execute_order : string -> string -> string -> string -> string -> unit
end

module Dao : Dao