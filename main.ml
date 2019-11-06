open AccountManager
open Account
open State
open OrderBook
open MatchingEngine

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

let non_option_user (s : State.t) = match State.get_user s with
  | None -> failwith "This is impossible"
  | Some u -> u

let update_account am = function
  | {asset; price; order_type; username} -> ()

let execute_order am o = 
  match o with 
  | {asset; price; order_type = Buy; username} -> 
    AccountManager.inc_account_balance am username asset price
  | {asset; price; order_type = Sell; username} -> 
    AccountManager.dec_account_balance am username asset price

let parse_txns (txns: transaction list) am = 
  match txns with 
  | [] -> ()
  | (o1,o2)::t -> 
    let _ = execute_order am o1 in 
    let _ = execute_order am o2 in 
    ()

let print_balances b = 
  let _ = print_string "[" in 
  let _ = List.iter (fun a -> 
      let (s, f) = a in 
      let _ = print_string s in 
      let _ = print_string ":" in 
      let _ = print_float f in 
      print_newline ()) b in 
  let _ = print_string "]" in 
  ()

let rec parse_order (s : State.t) = function 
  | ["Buy"; t; a] -> 
    let order = {asset = t; price = (float_of_string a); order_type = Buy; 
                 username = Account.username (non_option_user s)} in 
    let (txs, ob) = MatchingEngine.matchorder (State.get_book s) order in 
    let s = State.set_book ob s in 
    parse_txns txs (State.get_manager s)
  | ["Sell"; t; a] -> 
    let order = {asset = t; price = (float_of_string a); order_type = Sell; 
                 username = Account.username (non_option_user s)} in 
    let (txs, ob) = MatchingEngine.matchorder (State.get_book s) order in 
    let s = State.set_book ob s in 
    parse_txns txs (State.get_manager s)
  | _ -> print_endline "Invalid order"; run s

and run (s : State.t) : unit = 
  print_newline (); print_endline ("Account: " ^ Account.username (non_option_user s)); 
  (* let _ = print_int (OrderBook.num_buys (State.get_book s)) in *)
  let balances = Account.balances (non_option_user s) in 
  let _ = print_balances balances in 
  print_endline "To log out of this account, type 'logout'";
  print_endline "To place an order input: order type,ticker,amount";
  let i = (read_line ()) in 
  match String.trim i with 
  | "logout" -> restart s
  | a -> 
    begin 
      let lst = String.split_on_char ',' a in 
      if List.length lst <> 3 then 
        let _ = print_endline "Invalid order" in 
        run s
      else
        parse_order s lst; run s
    end


and login (s : State.t) = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in
  try 
    let account = AccountManager.login (State.get_manager s) username 
        password in 
    run (State.set_user (Some account) s)
  with 
  | InvalidPassword -> print_endline "Incorrect Password"; restart s
  | (InvalidUsername a) -> print_endline a; restart s


and register (s : State.t) = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  let new_account = AccountManager.register (State.get_manager s) username 
      password in 
  let s = State.set_user (Some new_account) s in 
  run s

and restart (s : State.t) : unit  = 
  match start_loop () with 
  | Login -> login s
  | Signup -> register s

and main () =
  let m = AccountManager.create () in
  let s = State.create_state m (OrderBook.empty) in 
  match startup_action () with 
  | Login -> login s
  | Signup -> register s


let () = main ()

