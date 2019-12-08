open Yojson.Basic.Util
open Lwt
open Cohttp
open Cohttp_lwt_unix
open AccountManager

module type Dao = sig 
  val get_account_data : unit -> AccountManager.t
  val signup_user : string -> string -> unit
end


module Dao : Dao = struct 

  let get_account_data () = 
    let body =
      Client.get 
        (Uri.of_string "http://localhost:8000/accounts/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    AccountManager.load_from_json json

  let signup_user username pass = 
    let json_string = "{\n\
                       \"username\":\"" ^ username ^ "\",\n\ 
    \"hashed_pass\":\"" ^ pass ^ "\",\n\
                                  \"balance\": 0.0,\n\  
    \"orders\": []\n\
                                  }" in
    let post_body = Cohttp_lwt.Body.of_string json_string in
    let body =
      Client.post ~body:post_body 
        (Uri.of_string "http://localhost:8000/accounts/") >>= fun (resp, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun body ->
      Yojson.Basic.from_string body 
    in         
    let json = Lwt_main.run body in
    match json |> to_assoc |> List.assoc "success" |> to_bool with
    | true -> ()
    | false -> failwith "Request could not be made"


end