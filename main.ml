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

let parse_order (usr: string) (lst: string list) : (string * submitted_order) option = 
  try
    begin
      let ticker, submitted_order = match lst with 
        | ["Buy"; ticker; amount; price] -> ticker, (Buy, (usr, (int_of_string amount), (float_of_string price), (Unix.time ())))
        | ["Sell"; ticker; amount; price] -> ticker, (Sell, (usr, (int_of_string amount), (float_of_string price), (Unix.time ())))
        | _ -> raise Not_found in 
      Some ((ticker, submitted_order))
    end
  with exn -> None

let prompt_user_input (user: Account.t): string = 
  print_newline (); print_endline ("Account: " ^ Account.username user); 
  let balances = Account.positions user in 
  let usd_balance = Account.balance user in 
  let balances = ("USD", (int_of_float usd_balance)) :: balances in 
  let _ = print_balances balances in 
  print_endline "To log out of this account, type 'logout' and to save&exit type 'quit'";
  print_endline "To place an order input: order type,ticker,amount";
  read_line ()

let read_input s user input am me = 
  match String.trim input with 
  | "logout" -> 
    {s with current_account = None}
  | "quit" ->
    AccountManager.write_accounts_to_dir "data" am;
    MatchingEngine.write_to_dir "data" me;
    Stdlib.exit 0;
  | a -> 
    begin 
      (* Buy/Sell ticker amount price *)
      let lst = String.split_on_char ',' a in 
      if List.length lst <> 4 then 
        let _ = print_endline "Invalid order" in 
        s
      else
        let parsed_order = parse_order (Account.username user) lst in 
        match parsed_order with 
        | None -> print_endline "Invalid order"; s
        | Some (ticker, submitted_order) ->
          begin
            let dir, order = submitted_order in 
            let _ = MatchingEngine.execute_regular_order s.matching_engine dir order ticker in 
            s
          end
    end

(** [repl s] is the main terminal of the system. It prints the account name,
    balances, and prompts the user to either log out or input an order. *)
let rec repl (s: state) : unit = 
  let st = match s.current_account with 
    | None -> restart s
    | Some user -> 
      begin
        let input = prompt_user_input user in 
        let updated_state = read_input s user input s.account_manager s.matching_engine in 
        updated_state
      end in 
  repl st

let me = MatchingEngine.load_from_dir "data"
let am = MatchingEngine.get_account_manager me

let empty_state = {current_account = None ; account_manager = am; matching_engine = me}

(** [main] creates a new AccountManager instance, State, and prompts the user
    to log into an account or register a new one. *)
let main () : unit =
  let s = empty_state in 
  let state = match startup_action () with 
    | Login -> login s
    | Signup -> register s in 
  repl state

let () = main ()

