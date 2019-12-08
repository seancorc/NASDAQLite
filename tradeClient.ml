(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix


(* let callback_example (status_code: int) (headers: string) (body: string) : string = 
   let _ = print_string "Received status code: "; print_int status_code in 
   body

   let post_request uri callback =
   Client.post (Uri.of_string uri) >>= fun (resp, body) ->
   let status_code = resp |> Response.status |> Code.code_of_status in
   let headers = (resp |> Response.headers |> Header.to_string) in 
   let body_promise = body |> Cohttp_lwt.Body.to_string in 
   body_promise >|= (callback status_code headers)

   let example_request = post_request "localhost:8000" callback_example

   let () =
   let body = Lwt_main.run example_request in
   print_endline ("Received body\n" ^ body) *)
(* client_example.ml *)
open Lwt
open Cohttp
open Cohttp_lwt_unix

let ctx = Cohttp_lwt_unix.Net.default_ctx


let body =
  Client.get ?ctx:(Some ctx) (Uri.of_string "https://www.reddit.com/") >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n" code;
  Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  Printf.printf "Body of length: %d\n" (String.length body);
  body

let () =
  let body = Lwt_main.run body in
  print_endline ("Received body\n" ^ body)
