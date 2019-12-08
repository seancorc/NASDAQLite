(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Helpers
open Yojson.Basic.Util


let get_accounts (body_str: string) : string =
  match Yojson.Basic.from_file 
          ("data" ^ Filename.dir_sep ^ "accounts.json") with 
  | v -> Yojson.Basic.pretty_to_string v
  | exception e -> "Error Parsing File"

let error_response mesg _ = mesg (* Needs to take extra parameter 
                                    to account for body*)

let _ = Cohttp_lwt_unix__.Debug.activate_debug () 
let base_uri = "//localhost:8000"

let appropriate_method uri meth = 
  if uri = (base_uri ^ "/accounts/") then 
    begin match meth with 
      | "GET" -> get_accounts
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
