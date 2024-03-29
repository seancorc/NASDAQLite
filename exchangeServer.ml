(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open MatchingEngine
open AccountManager
open OrderBook

let dirname = "data"
let successful_response = "{\"success\": true}"
let invalid_request_body_error = "{\"success\": false, 
                                  \"error\": \"Invalid request body\"}"
let error_response err msg _ = "{\"success\": false, 
                            \"error\": \"" ^ err ^ "\",
                            \"message\": \"" ^ msg ^ "\"}" 
let _ = Cohttp_lwt_unix__.Debug.activate_debug () 
let base_uri = "//localhost:8000"

exception Ticker_not_found
exception Invalid_direction


module type ExchangeServer = sig 
  val run : unit -> unit
  val get_account_balance : string -> 'a -> string
  val get_account_positions : string -> 'a -> string
  val signup : string -> string
  val execute_order : string -> string
  val login : string -> string
  val delete : string -> string
  val create_asset : string -> string
end

module ExchangeServer : ExchangeServer = struct

  (** [find_user username user] is the bool representing whether [username]
      is equal to the username represented by the json object [user] *)
  let find_user username user = 
    let other_username = user |> to_assoc |> List.assoc "username" |> to_string 
    in username = other_username

  let get_account_balance username _ =
    let json = Yojson.Basic.from_file 
        (dirname ^ Filename.dir_sep ^ "accounts.json") in
    let users = json |> to_assoc |> List.assoc "users" |> to_list in
    let user = List.find (find_user username) users in
    let balance = (user |> to_assoc |> List.assoc "balance" |> to_float) in
    "{\"success\": true, \"data\":" ^ (string_of_float balance) ^ "0}"

  (** [create_orders_json orders acc] is the json_string representing the orders
      given in the json object list [orders].*)
  let rec create_orders_json orders acc =
    match orders with 
    | [] -> acc 
    | h :: t -> 
      let assoc = to_assoc h in
      let ticker = assoc |> List.assoc "ticker" |> to_string in
      let amount = assoc |> List.assoc "amount" |> to_int in
      let order_json = "{\"ticker\": \"" ^ ticker ^ "\",
      \"amount\": " ^ (string_of_int amount) ^ "}" ^ if List.length t >= 1
                       then ",\n" else "\n" in 
      create_orders_json t (acc ^ order_json)

  let get_account_positions username _ =
    let json = Yojson.Basic.from_file 
        (dirname ^ Filename.dir_sep ^ "accounts.json") in
    let users = json |> to_assoc |> List.assoc "users" |> to_list in
    let user = List.find (find_user username) users in
    let orders = (user |> to_assoc |> List.assoc "orders" |> to_list) in
    "{\"success\": true, \"data\":[" ^ (create_orders_json orders "") ^ "]}"


  let signup body =
    match Yojson.Basic.from_string body with 
    | credentials ->
      let json_am = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "accounts.json") in
      let am = AccountManager.load_from_json json_am in 
      let assoc = to_assoc credentials in
      let username = assoc |> List.assoc "username" |> to_string in
      let pass = assoc |> List.assoc "pass" |> to_string in
      begin try 
          let _ = AccountManager.register am username pass in
          let json_string = AccountManager.to_json_string am in
          let json_am = Yojson.Basic.from_string json_string in
          Yojson.Basic.to_file 
            (dirname ^ Filename.dir_sep ^ "accounts.json") json_am;
          successful_response
        with 
        | Invalid_username a -> error_response "username" a ()
        | Invalid_password -> error_response "password" "" ()
      end
    | exception e ->
      invalid_request_body_error

  (** [to_direction s] is the OrderBook.order_direction represented by the 
      string [s]. 
      Raises: Invalid_direction if [s] is not a valid direction. *)
  let to_direction = function
    | "buy" -> Buy
    | "sell" -> Sell 
    | _ -> raise Invalid_direction

  let execute_order body = 
    match Yojson.Basic.from_string body with 
    | json_order -> 
      let json_me = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "engine.json") in
      let me = MatchingEngine.load_from_json json_me in
      let json_am = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "accounts.json") in
      let am = AccountManager.load_from_json json_am in
      let new_me = MatchingEngine.set_account_manager me am in
      let assoc = to_assoc json_order in 
      let dir = assoc |> List.assoc "direction" |> to_string |> to_direction in
      let username = assoc |> List.assoc "username" |> to_string in
      let ticker = assoc |> List.assoc "ticker" |> to_string in
      let amount = assoc |> List.assoc "amount" |> to_int in
      let price = assoc |> List.assoc "price" |> to_float in
      let tickers = MatchingEngine.tickers me in 
      if not (List.mem ticker tickers) then 
        (error_response "Invalid Ticker" "" ()) 
      else
        let order = (username, amount, price, Unix.time ()) in
        let _ = MatchingEngine.execute_regular_order new_me dir order ticker in 
        let json_string = MatchingEngine.orderbooks_to_json_string new_me in
        let json_me = Yojson.Basic.from_string json_string in
        Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "engine.json") 
          json_me;
        let updated_am = MatchingEngine.get_account_manager new_me in
        let json_string = AccountManager.to_json_string updated_am in
        let json_am = Yojson.Basic.from_string json_string in
        Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "accounts.json") 
          json_am;
        successful_response
    | exception e ->
      invalid_request_body_error

  let login body = 
    match Yojson.Basic.from_string body with 
    | credentials -> 
      let json_am = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "accounts.json") in
      let am = AccountManager.load_from_json json_am in 
      let assoc = to_assoc credentials in
      let username = assoc |> List.assoc "username" |> to_string in
      let pass = assoc |> List.assoc "pass" |> to_string in
      begin try 
          let _ = AccountManager.login am username pass in
          successful_response
        with 
        | Invalid_username a -> error_response "username" a ()
        | Invalid_password -> error_response "password" ""()
        | _ -> error_response "login unsuccessful" "" ()
      end
    | exception e -> 
      invalid_request_body_error

  let delete body = 
    match Yojson.Basic.from_string body with 
    | credentials -> 
      let json_me = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "engine.json") in
      let me = MatchingEngine.load_from_json json_me in
      let json_am = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "accounts.json") in
      let am = AccountManager.load_from_json json_am in
      let new_me = MatchingEngine.set_account_manager me am in
      let assoc = to_assoc credentials in
      let username = assoc |> List.assoc "username" |> to_string in
      let pass = assoc |> List.assoc "pass" |> to_string in
      begin try 
          let _ = MatchingEngine.delete_user new_me username pass in
          let json_string = MatchingEngine.orderbooks_to_json_string new_me in
          let json_me = Yojson.Basic.from_string json_string in
          Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "engine.json") 
            json_me;
          let updated_am = MatchingEngine.get_account_manager new_me in
          let json_string = AccountManager.to_json_string updated_am in
          let json_am = Yojson.Basic.from_string json_string in
          Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "accounts.json") 
            json_am;
          successful_response
        with 
        | Invalid_username a -> error_response "username" a ()
        | Invalid_password -> error_response "password" "" ()
        | _ -> error_response "login unsuccessful" "" ()
      end
    | exception e -> 
      invalid_request_body_error

  let create_asset body =
    match Yojson.Basic.from_string body with 
    | ticker_json ->
      let assoc = to_assoc ticker_json in
      let ticker = assoc |> List.assoc "ticker" |> to_string in
      let json_me = Yojson.Basic.from_file 
          (dirname ^ Filename.dir_sep ^ "engine.json") in
      let me = MatchingEngine.load_from_json json_me in
      if MatchingEngine.member me ticker then 
        error_response "ticker" "" () 
      else
        let _ = MatchingEngine.add_asset me ticker in 
        let json_string = MatchingEngine.orderbooks_to_json_string me in
        let json_me = Yojson.Basic.from_string json_string in
        Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "engine.json") 
          json_me;
        successful_response
    | exception e -> 
      invalid_request_body_error

  (** [appropriate_method uri meth] is the function specified by URI 
      [uri] and request method [meth]. *)
  let appropriate_method uri meth =
    let account_balance_regex = 
      Str.regexp "\\/\\/localhost:8000\\/account\\/balance\\/" in
    let account_positions_regex = 
      Str.regexp "\\/\\/localhost:8000\\/account\\/positions\\/" in
    if uri = (base_uri ^ "/account/login/") then 
      begin match meth with 
        | "POST" -> login
        | _ -> error_response "Method Not Supported" ""
      end
    else if uri = (base_uri ^ "/account/delete/") then
      begin match meth with
        | "DELETE" -> delete
        | _ -> error_response "Method Not Supported" ""
      end
    else if uri = (base_uri ^ "/account/signup/") then 
      begin match meth with 
        | "POST" -> signup
        | _ -> error_response "Method Not Supported" ""
      end
    else if uri = (base_uri ^ "/engine/asset/") then
      begin match meth with 
        | "POST" -> create_asset
        | _ -> error_response "Method Not Supported" ""
      end
    else if Str.string_match account_balance_regex 
        (uri ^ "/account/balance/") 0 then 
      let slash_index = String.rindex_from uri ((String.length uri) - 2) '/' in
      let username = String.sub uri (slash_index + 1) 
          (String.length uri - (slash_index + 2)) in
      begin match meth with 
        | "GET" -> get_account_balance username
        | _ -> error_response "Method Not Supported" ""
      end
    else if Str.string_match account_positions_regex 
        (uri ^ "/account/positions/") 0 then 
      let slash_index = String.rindex_from uri ((String.length uri) - 2) '/' in
      let username = String.sub uri (slash_index + 1) 
          (String.length uri - (slash_index + 2)) in
      begin match meth with 
        | "GET" -> get_account_positions username
        | _ -> error_response "Method Not Supported" ""
      end
    else if uri = (base_uri ^ "/engine/") then
      begin match meth with 
        | "POST" -> execute_order
        | _ -> error_response "Method Not Supported" ""
      end
    else error_response "404 Route Not Found" ""

  (** [server] is a promise that when ran using 'Lwt_main.run server', 
      activates the server on port 8000 *)
  let server =
    let callback _conn req body =
      let uri = req |> Request.uri |> Uri.to_string in
      let _ = print_string "Received request from "; print_string uri; in 
      let meth = req |> Request.meth |> Code.string_of_method in
      let body_string_promise = body |> Cohttp_lwt.Body.to_string in 
      body_string_promise >|= (appropriate_method uri meth)
      >>= (fun body -> 
          Server.respond_string ~status:`OK ~body ())
    in
    Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

  (** [create_default_tickers dt] is the json object that represents the 
      the list of tickers [dt] all with no buys or sells. *)
  let rec create_default_tickers dt =
    match dt with 
    | [] -> []
    | h :: t -> `Assoc[("ticker", `String h);("buys",`List[]);("sells",`List[])] 
                :: (create_default_tickers t)

  (** [setup_data_directory ()] attempts to make a 'data' directory with 
      appropriate files but if an exception is thrown (typically because the 
      data directory already exists) it does nothing. *)
  let setup_data_directory _ =
    try 
      let accounts_file_name = "accounts.json" in
      let engine_file_name = "engine.json" in
      Unix.mkdir dirname 0o775;
      let _ = Stdlib.open_out (dirname ^ Filename.dir_sep ^ accounts_file_name) 
      in let _ = Stdlib.open_out (dirname ^ Filename.dir_sep ^ engine_file_name) 
      in let starting_accounts_json = `Assoc["users", `List []] in 
      let default_tickers = create_default_tickers 
          ["GOOG"; "MSFT"; "AAPL"; "ROKU"; "AMZN"] in
      let starting_engine_json = `Assoc["tickers", `List default_tickers] in 
      Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ accounts_file_name) 
        starting_accounts_json;
      Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ engine_file_name) 
        starting_engine_json;
      ()
    with _ ->
      ()

  let run () =  setup_data_directory (); ignore (Lwt_main.run server);

end

let _ = ExchangeServer.run ()
