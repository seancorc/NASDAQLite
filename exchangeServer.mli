

exception Ticker_not_found
exception Invalid_direction



module type ExchangeServer = sig 
  (** [run ()] runs the server *)
  val run : unit -> unit

  (** [get_account_balance username _] is the json string which contains the 
      balance of the account with username [username] 
      Example: 
      \{
      "success": true,
      "data": 10.0
      \} *)
  val get_account_balance : string -> 'a -> string


  (** [get_account_positions username _] is the json string which contains the 
      positions of the account with username [username] 
      Example: 
       \{
      "success": true,
      "data": [
            \{
                  "ticker": "GOOG",
                  "amount": 1
            \}
      ]
      \}
  *)
  val get_account_positions : string -> 'a -> string

  (** [signup request_body] is the json string which represents a successful
      registration of the accout data provided in [request_body] or an error 
      message
      Example1: \{"success": true\} 
      Example2: \{"success": false, "error": "Invalid request body"\}
  *)
  val signup : string -> string

  (** [execute_order request_body] is the json string which represents a 
      successful execution of the order data provided in [request_body] or an 
      error message
      Example1: \{"success": true\} 
      Example2: \{"success": false, "error": "Invalid request body"\}
  *)
  val execute_order : string -> string

  (** [login request_body] is the json string which represents a successful
      login of the accout data provided in [request_body] or an error 
      message
      Example1: \{"success": true\} 
      Example2: \{"success": false, "error": "Invalid request body"\}
  *)
  val login : string -> string

  (** [delete request_body] is the json string which represents a successful
      deletion of the accout data provided in [request_body] or an error 
      message
      Example1: \{"success": true\} 
      Example2: \{"success": false, "error": "Invalid request body"\}
  *)
  val delete : string -> string

  (** [create_asset request_body] is the json string which represents a 
      successful creation of the asset data provided in [request_body] or an 
      error message
      Example1: \{"success": true\} 
      Example2: \{"success": false, "error": "Invalid request body"\}
  *)
  val create_asset : string -> string
end

(** ExchangeServer is the implmentation of the ExchangeServer signature *)
module ExchangeServer : ExchangeServer