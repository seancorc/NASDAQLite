
type action = Login | Signup 

(** [start_loop] is the eithier the Login or Signup action depending on the 
    user's input*)
let rec start_loop () = 
  print_endline ("Type 'login' to login or 'signup' to signup");
  match read_line () with
  | "login" -> Login
  | "signup" -> Signup
  | _ -> print_endline "I couldn't understand that command, please try again."; 
    start_loop ()


let startup_action () = 
  print_endline ("Welcome to NASDAQLite!"); 
  start_loop ()

let login () = 
  print_string "Username: ";
  let input = (read_line ()) in 
  ()


let main () =
  match startup_action () with 
  | Login -> login ()
  | Signup -> failwith "unimplemented"

let () = main ()

