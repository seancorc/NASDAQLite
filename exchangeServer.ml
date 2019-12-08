(* server_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Helpers


let process_request_body (body_str: string) : string = 
  body_str


let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let _ = print_string "Received request from "; print_string uri; in 
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let body_string_promise = body |> Cohttp_lwt.Body.to_string in 
    body_string_promise >|= process_request_body
    >>= (fun body -> 
        Server.respond_string ~status:`OK ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
