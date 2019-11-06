
open AccountManager
open Account

type action = Login | Signup 

(** [start_loop] is the eithier the Login or Signup action depending on the 
    user's input*)
let rec start_loop () = 
  print_endline ("Type 'login' to login or 'signup' to signup");
  match String.trim(read_line ()) with
  | "login" -> Login
  | "signup" -> Signup
  | _ -> print_endline "I couldn't understand that command, please try again."; 
    start_loop ()


let startup_action () = 
  print_endline ("Welcome to NASDAQLite!"); 
  start_loop ()

let rec run a m = 
  print_endline ("Account Username: " ^ Account.username a);
  print_endline "To log out of this account, type 'logout'";
  let i = (read_line ()) in 
  match String.trim i with 
  | "logout" -> restart m


and login m = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  try run (AccountManager.login m username password) m with 
  | InvalidPassword -> print_endline "Incorrect Password"; login m
  | (InvalidUsername s) -> print_endline s; login m


and register m = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  let new_account = AccountManager.register m username password in 
  run new_account m

and restart m = 
  match start_loop () with 
  | Login -> login m
  | Signup -> register m

and main () =
  let m = AccountManager.create () in
  match startup_action () with 
  | Login -> login m
  | Signup -> register m


let () = main ()

