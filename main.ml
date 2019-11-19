open AccountManager
open Account
open OrderBook
open MatchingEngine

type action = Login | Signup 

(** [start_loop] is the eithier the Login or Signup action depending on the 
    user's input. *)
let rec start_loop () = 
  print_endline ("Type 'login' to login or 'signup' to signup");
  match String.trim(read_line ()) with
  | "login" -> Login
  | "signup" -> Signup
  | _ -> print_endline "I couldn't understand that command, please try again."; 
    start_loop ()

(** [startup_action] prints the welcome message and prompts [start_loop]. *)
let startup_action () = 
  print_endline ("Welcome to NASDAQLite!"); 
  start_loop ()

(** [non_option_user s] is the user in State [s] in non-option format.
    Requires: The user in State [s] is not [None]. *)
(* let non_option_user (s : State.t) = match State.get_user s with
   | None -> failwith "This is impossible"
   | Some u -> u *)

(** [update_account am] updates the AccountManager [am]. *)
(* let update_account am = function
   | {asset; price; order_type; username} -> () *)

(* (** [execute_order am o] executes the order [o] and updates the user's account
    in AccountManager [am] based on the order. *)
   let execute_order am o = 
   match o with 
   | {asset; price; order_type = Buy; username} -> 
    AccountManager.inc_account_balance am username asset price
   | {asset; price; order_type = Sell; username} -> 
    AccountManager.dec_account_balance am username asset price

   (** [parse_txns txns] parses the transaction list [txns] and executes each
    order. *)
   let parse_txns (txns: transaction list) am = 
   match txns with 
   | [] -> ()
   | (o1,o2)::t -> 
    let _ = execute_order am o1 in 
    let _ = execute_order am o2 in 
    () *)

(** [print_balances b] prints each balance in the balance list [b]. *)
let print_balances b = 
  let _ = print_string "Balances: [" in 
  let _ = List.iter (fun a -> 
      let (s, f) = a in 
      let _ = print_string s in 
      let _ = print_string ":" in 
      let _ = print_float f in 
      print_string "; ") b in 
  let _ = print_string "]" in 
  let _ = print_newline () in 
  ()

(** [parse_order s o] parses the input order [o] and updates State [s] based
    on this input. *)
let rec parse_order (s : State.t) = function 
  | ["Buy"; t; a] -> {asset = t; price = (float_of_string a); order_type = Buy; 
                      username = Account.username (non_option_user s)}
                       s
  | ["Sell"; t; a] -> {asset = t; price = (float_of_string a); order_type = Sell; 
                       username = Account.username (non_option_user s)}
let _ = print_endline "Invalid order" in 
s

(** [login s] prompts the user to log into their account using their username
    and password. *)
let login (s : State.t) : State.t = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in
  try 
    let account = AccountManager.login (State.get_manager s) username password in 
    (State.set_user (Some account) s)
  with 
  | InvalidPassword ->
    let _ =  print_endline "Incorrect Password" in 
    s
  | (InvalidUsername a) -> 
    let _ = print_endline a in 
    s

(** [register s] prompts the user to register a newaccount with a new username
    and password. *)
let register (s : State.t) : State.t = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  let new_account = AccountManager.register (State.get_manager s) username 
      password in 
  let s = State.set_user (Some new_account) s in 
  s

(** [restart s] prompts the user to register a newaccount with a new username
    and password. *)
let restart (s : State.t) : State.t  = 
  match start_loop () with 
  | Login -> login s
  | Signup -> register s

(** [repl s] is the main terminal of the system. It prints the account name,
    balances, and prompts the user to either log out or input an order. *)
let rec repl (s: State.t) : unit = 
  let st = match (State.get_user s) with 
    | None -> restart s
    | Some user -> begin
        print_newline (); print_endline ("Account: " ^ Account.username user); 
        let balances = Account.balances user in 
        let _ = print_balances balances in 
        print_endline "To log out of this account, type 'logout'";
        print_endline "To place an order input: order type,ticker,amount";
        let i = (read_line ()) in 
        match String.trim i with 
        | "logout" -> 
          let updated_state = State.set_user None s in 
          updated_state
        | a -> 
          begin 
            let lst = String.split_on_char ',' a in 
            if List.length lst <> 3 then 
              let _ = print_endline "Invalid order" in 
              s
            else
              let updated_state = parse_order s lst in 
              updated_state
          end
      end in 
  repl st

(** [main] creates a new AccountManager instance, State, and prompts the user
    to log into an account or register a new one. *)
let main () : unit =
  let m = AccountManager.create () in
  let s = State.create_state m OrderBook.empty in 
  let state = match startup_action () with 
    | Login -> login s
    | Signup -> register s in 
  repl state

let () = main ()

