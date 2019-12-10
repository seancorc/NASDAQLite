open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Bcrypt

module type Dao = sig 
  val get_account_data : unit -> Yojson.Basic.t
  val write_account_manager_data : string -> unit
  val signup_user : string -> string -> unit
  val get_engine_data : unit -> Yojson.Basic.t
  val add_order : string -> string -> string -> string -> string -> unit
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

  let write_account_manager_data am_json_string = 
    let post_body = Cohttp_lwt.Body.of_string am_json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/accounts/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | _ -> raise Server_Error

  let signup_user username pass = 
    let hashed_pass = Bcrypt.string_of_hash (Bcrypt.hash pass) in
    let json_string = "{\n\"username\":\"" ^ username ^ "\",\n 
    \"hashed_pass\":\"" ^ hashed_pass ^ "\",\n\"balance\": 0.0,\n  
    \"orders\": []\n}" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/account/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | _ -> raise Server_Error

  let get_engine_data () =
    let body =
      Client.get 
        (Uri.of_string "http://localhost:8000/engine/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    json

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