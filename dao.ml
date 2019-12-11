open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bcrypt
open AccountManager

module type Dao = sig 
  val get_account_balance : string -> Yojson.Basic.t
  val get_account_positions : string -> Yojson.Basic.t
  val signup_user : string -> string -> unit
  val login_user : string -> string -> unit
  val delete_user : string -> string -> unit
  val execute_order : string -> string -> string -> string -> string -> unit
  val create_asset : string -> unit
end

exception Server_Error

module Dao : Dao = struct 

  let get_account_balance username = 
    let body =
      Client.get 
        (Uri.of_string ("http://localhost:8000/account/balance/" ^ username ^ "/")) >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    json

  let get_account_positions username = 
    let body =
      Client.get 
        (Uri.of_string ("http://localhost:8000/account/positions/" ^ username ^ "/")) >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    json


  let handle_credentials_error json = 
    let assoc = json |> to_assoc in
    let error_type =  assoc |> List.assoc "error" |> to_string in
    let msg = assoc |> List.assoc "message" |> to_string in
    if error_type = "password" then 
      raise Invalid_password
    else if error_type = "username" then
      raise (Invalid_username msg)
    else
      raise Server_Error 

  let signup_user username pass = 
    let json_string = "{\n\"username\":\"" ^ username ^ "\",\n 
    \"pass\":\"" ^ pass ^ "\"}" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/account/signup/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | false -> handle_credentials_error json

  let login_user username pass = 
    let json_string = "{\n\"username\":\"" ^ username ^ "\",\n 
    \"pass\":\"" ^ pass ^ "\"}" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/account/login/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | false -> handle_credentials_error json

  let execute_order username dir ticker amount price = 
    let json_string = "{\n\"username\":\""^ username ^"\",
    \"amount\":"^ amount ^",
    \"direction\":\""^ dir ^"\",
    \"price\":"^ price ^"0,
    \"time\":"^ string_of_float (Unix.time ()) ^ "0," ^ "
    \"ticker\":\""^ ticker ^"\"
    }" in 
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/engine/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | _ -> raise Server_Error 

  let create_asset ticker = 
    let json_string = "{\"ticker\": \"" ^ ticker  ^ "\" }" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/engine/asset/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | _ -> raise Server_Error 

  let delete_user username pass = 
    let json_string = "{\n\"username\":\"" ^ username ^ "\",\n 
    \"pass\":\"" ^ pass ^ "\"}" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.delete ~body:post_body 
        (Uri.of_string "http://localhost:8000/account/delete/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | false -> handle_credentials_error json

end