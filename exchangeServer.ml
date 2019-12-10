(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

let dirname = "data"
let successful_response = "{\"success\": true}"
let invalid_request_body_error = "{\"error\": \"Invalid request body\"}"
exception Ticker_Not_Found
exception Invalid_Direction
exception Parse_Error

let get_accounts _ =
  match Yojson.Basic.from_file 
          ("data" ^ Filename.dir_sep ^ "accounts.json") with 
  | v -> Yojson.Basic.pretty_to_string v
  | exception e -> "Error Parsing File"

let find_user username user = 
  let other_username = user |> to_assoc |> List.assoc "username" |> to_string in 
  username = other_username

let get_account_balance username _ =
  let json = Yojson.Basic.from_file (dirname ^ Filename.dir_sep ^ "accounts.json") in
  let users = json |> to_assoc |> List.assoc "users" |> to_list in
  let user = List.find (find_user username) users in
  let balance = (user |> to_assoc |> List.assoc "balance" |> to_float) in
  "{\"success\": true, \"data\":" ^ (string_of_float balance) ^ "0}"

let write_accounts body = 
  match Yojson.Basic.from_string body with 
  | am -> 
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "accounts.json") am;
    successful_response
  | exception e ->
    invalid_request_body_error

let add_account body =
  match Yojson.Basic.from_string body with 
  | new_user ->
    let json = Yojson.Basic.from_file (dirname ^ Filename.dir_sep ^ "accounts.json") in
    let users = json |> to_assoc |> List.assoc "users" |> to_list in
    let new_users = `Assoc["users", `List (new_user :: users)] in
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "accounts.json") new_users;
    successful_response
  | exception e -> 
    invalid_request_body_error

let get_engine _ = 
  match Yojson.Basic.from_file 
          ("data" ^ Filename.dir_sep ^ "engine.json") with 
  | v -> Yojson.Basic.pretty_to_string v
  | exception e -> "Error Parsing File"

let write_engine body = 
  match Yojson.Basic.from_string body with 
  | me -> 
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "engine.json") me;
    successful_response
  | exception e ->
    invalid_request_body_error

let get_specific_ticker ticker tickers = 
  let rec find_ticker_and_accumulate_others acc = 
    match tickers with 
    | [] -> raise Ticker_Not_Found
    | h :: t -> 
      let current_ticker = h |> to_assoc |> List.assoc "ticker" |> to_string in
      if current_ticker = ticker then 
        h, (acc @ t)
      else 
        let ots = (h :: acc) in
        find_ticker_and_accumulate_others ots in 
  find_ticker_and_accumulate_others []

let update_ticker_json tj direction order : Yojson.Basic.t =
  let ticker = tj |> to_assoc |> List.assoc "ticker" |> to_string in
  let buys = tj |> to_assoc |> List.assoc "buys" |> to_list in
  let sells = tj |> to_assoc |> List.assoc "sells" |> to_list in
  if direction = "buy" then 
    `Assoc ["ticker", `String ticker;"buys", `List (order :: buys);"sells", 
                                                                   `List sells] 
  else if direction = "sell" then
    `Assoc ["ticker", `String ticker;"buys", `List buys;"sells", 
                                                        `List (order :: sells)] 
  else 
    raise Invalid_Direction 


let add_order body = 
  match Yojson.Basic.from_string body with 
  | req_body ->
    let ticker = req_body |> to_assoc |> List.assoc "ticker" |> to_string in
    let order = req_body |> to_assoc |> List.assoc "order" in
    let direction = req_body |> to_assoc |> List.assoc "direction" |> to_string 
                    |> String.lowercase_ascii in 
    let json = Yojson.Basic.from_file (dirname ^ Filename.dir_sep ^ "engine.json") in
    let all_tickers = json |> to_assoc |> List.assoc "tickers" |> to_list in
    let ticker_json, other_tickers = get_specific_ticker ticker all_tickers in
    let updated_tj = update_ticker_json ticker_json direction order in
    let (new_json : Yojson.Basic.t) = `Assoc ["tickers", `List (updated_tj :: 
                                                                other_tickers)] in
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ "engine.json") new_json;
    successful_response
  | exception e -> 
    invalid_request_body_error


let error_response err _ = "{\"error\": \"" ^ err ^ "\"}" 

let _ = Cohttp_lwt_unix__.Debug.activate_debug () 
let base_uri = "//localhost:8000"

let appropriate_method uri meth =
  let account_balance_regex = Str.regexp "\\/\\/localhost:8000\\/account\\/balance\\/*" in
  if uri = (base_uri ^ "/accounts/") then 
    begin match meth with 
      | "GET" -> get_accounts
      | "POST" -> write_accounts
      | _ -> error_response "Method Not Supported"
    end
  else if uri = (base_uri ^ "/account/") then 
    begin match meth with 
      | "POST" -> add_account
      | _ -> error_response "Method Not Supported"
    end
  else if Str.string_match account_balance_regex 
      (base_uri ^ "/account/balance/") 0 then 
    let slash_index = String.rindex_from uri ((String.length uri) - 2) '/' in
    let username = String.sub uri (slash_index + 1) 
        (String.length uri - (slash_index + 2)) in
    begin match meth with 
      | "GET" -> get_account_balance username
      | _ -> error_response "Method Not Supported"
    end
  else if uri = (base_uri ^ "/engine/") then
    begin match meth with 
      | "GET" -> get_engine
      | "POST" -> write_engine
      | _ -> error_response "Method Not Supported"
    end
  else if uri = (base_uri ^ "/orders/") then
    begin match meth with 
      | "POST" -> add_order
      | _ -> error_response "Method Not Supported"
    end
  else error_response "404 Route Not Found"


let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let _ = print_string "Received request from "; print_string uri; in 
    let meth = req |> Request.meth |> Code.string_of_method in
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let body_string_promise = body |> Cohttp_lwt.Body.to_string in 
    body_string_promise >|= (appropriate_method uri meth)
    >>= (fun body -> 
        Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)