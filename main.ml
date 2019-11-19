open AccountManager
open Account
open OrderBook
open MatchingEngine

type state = {current_account: Account.t option; account_manager : AccountManager.t; matching_engine: MatchingEngine.t}

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
      let _ = print_int f in 
      print_string "; ") b in 
  let _ = print_string "]" in 
  let _ = print_newline () in 
  ()

(** [parse_order s o] parses the input order [o] and updates State [s] based
    on this input. *)
(* let rec parse_order username = function 
   | ["Buy"; t; a] -> (username, a)
   | ["Sell"; t; a] -> {asset = t; price = (float_of_string a); order_type = Sell; username = username} *)

(** [login s] prompts the user to log into their account using their username
    and password. *)
let login (s : state) : state = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in
  try 
    let account = AccountManager.login (s.account_manager) username password in 
    {s with current_account =  Some account}
  with 
  | InvalidPassword ->
    let _ =  print_endline "Incorrect Password" in 
    s
  | (InvalidUsername a) -> 
    let _ = print_endline a in 
    s

(** [register s] prompts the user to register a newaccount with a new username
    and password. *)
let register (s : state) : state = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  let new_account = AccountManager.register (s.account_manager) username 
      password in 
  let s = {s with current_account = (Some new_account)} in 
  s

(** [restart s] prompts the user to register a newaccount with a new username
    and password. *)
let restart (s : state) : state  = 
  match start_loop () with 
  | Login -> login s
  | Signup -> register s
(* 
let parse_details usr amount price = 
let amount' = int_of_string amount in 
let price' = float_of_string in 
(usr, ) *)

let parse_order (usr: string) (lst: string list) : submitted_order option = 
  try
    begin
      match lst with 
      | ["Buy"; ticker; amount; price] -> failwith ""
      | ["Sell"; ticker; amount; price] -> failwith ""
      | _ -> None
    end
  with exn -> None

(** [repl s] is the main terminal of the system. It prints the account name,
    balances, and prompts the user to either log out or input an order. *)
let rec repl (s: state) : unit = 
  let st = match s.current_account with 
    | None -> restart s
    | Some user -> begin
        print_newline (); print_endline ("Account: " ^ Account.username user); 
        let balances = Account.positions user in 
        let usd_balance = Account.balance user in 
        let balances = ("USD", (int_of_float usd_balance)) :: balances in 
        let _ = print_balances balances in 
        print_endline "To log out of this account, type 'logout'";
        print_endline "To place an order input: order type,ticker,amount";
        let i = (read_line ()) in 
        match String.trim i with 
        | "logout" -> 
          {s with current_account = None}
        | a -> 
          begin 
            (* Buy/Sell ticker amount price *)
            let lst = String.split_on_char ',' a in 
            if List.length lst <> 4 then 
              let _ = print_endline "Invalid order" in 
              s
            else
              let order = parse_order Account.username lst in 
              s
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

