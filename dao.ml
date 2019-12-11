open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bcrypt
open AccountManager

module type Dao = sig 
  val get_account_data : unit -> Yojson.Basic.t
  val get_account_balance : string -> Yojson.Basic.t
  val get_account_positions : string -> Yojson.Basic.t
  val signup_user : string -> string -> unit
  val login_user : string -> string -> unit
  val execute_order : string -> string -> string -> string -> string -> unit
end

exception Server_Error

module Dao : Dao = struct 

  let get_account_data () = 
    let body =
      Client.get 
        (Uri.of_string "http://localhost:8000/accounts/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    json

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
    | false -> 
      let error_type = json |> to_assoc |> List.assoc "error" |> to_string in
      if error_type = "password" then 
        raise InvalidPassword
      else if error_type = "username" then
        raise (InvalidUsername "Username is taken")
      else
        raise Server_Error 

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
    | false -> 
      let error_type = json |> to_assoc |> List.assoc "error" |> to_string in
      if error_type = "password" then 
        raise InvalidPassword
      else if error_type = "username" then
        raise (InvalidUsername "Username does not exist")
      else
        raise Server_Error 

  let execute_order username dir ticker amount price = 
    let json_string = "{\n\"username\":\""^ username ^"\",
    \"amount\":"^ amount ^",
    \"direction\":\""^ dir ^"\",
    \"price\":"^ price ^",
    \"time\":"^ string_of_float (Unix.time ()) ^ "0," ^ "
    \"ticker\":"^ ticker ^",
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


  let add_order ticker direction username amt price =
    let order = "{\n\"username\":\""^ username ^"\",
    \n\"amount\":"^ amt ^",
    \n\"price\":"^ price ^",
    \n\"time\":"^ string_of_float (Unix.time ()) ^ "0" ^ "}" in
    let json_string = "{\n\"ticker\":\"" ^ ticker ^ "\",\n 
    \"direction\":\"" ^ direction ^ "\",\n\"order\": "^ order ^ "\n}" in
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

end