open AccountManager
open Account
open OrderBook
open MatchingEngine
open Dao


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
  let _ = Dao.signup_user username password in
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
        | ["Buy Market"; ticker; amount] -> ticker, (Buy, (usr, (int_of_string amount), max_float, (Unix.time ())))
        | ["Sell Market"; ticker; amount] -> ticker, (Sell, (usr, (int_of_string amount), min_float, (Unix.time ())))
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
  print_endline "To place an order input: order type (Buy, Sell, Buy Market, Sell Market), ticker, order size, price (only for Buy or Sell)";
  read_line ()

let read_input s user input am me = 
  match String.trim input with 
  | "logout" -> 
    {s with current_account = None}
  | "quit" ->
    let am_json_string = AccountManager.to_json_string s.account_manager in 
    Dao.write_account_manager_data am_json_string;
    (** TODO, do same thing with matching engine data*)
    Stdlib.exit 0;
  | a -> 
    begin 
      (* Buy/Sell ticker amount price *)
      let lst = String.split_on_char ',' a in 
      let lst' = List.map (String.trim) lst in
      let parsed_order = parse_order (Account.username user) lst' in 
      match parsed_order with 
      | None -> print_endline "Invalid order"; s
      | Some (ticker, submitted_order) ->
        begin
          let tickers = MatchingEngine.tickers s.matching_engine in 
          if not (List.mem ticker tickers) then (print_endline "Invalid order"; s) else
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

let rec create_default_tickers dt =
  match dt with 
  | [] -> []
  | h :: t -> `Assoc[("ticker", `String h);("buys",`List[]);("sells",`List[])] ::
              (create_default_tickers t)



let inital_state () =
  try 
    let engine_json = Dao.get_engine_data () in
    let me = MatchingEngine.load_from_json engine_json in  
    let am = MatchingEngine.get_account_manager me in 
    {current_account = None ; account_manager = am; matching_engine = me}
  with e ->
    let dirname = "data" in
    let accounts_file_name = "accounts.json" in
    let engine_file_name = "engine.json" in
    Unix.mkdir dirname 0o775;
    let _ = Stdlib.open_out (dirname ^ Filename.dir_sep ^ accounts_file_name) in
    let _ = Stdlib.open_out (dirname ^ Filename.dir_sep ^ engine_file_name) in
    let starting_accounts_json = `Assoc["users", `List []] in 
    let default_tickers = create_default_tickers 
        ["GOOG"; "MSFT"; "AAPL"; "ROKU"; "AMZN"] in
    let starting_engine_json = `Assoc["tickers", `List default_tickers] in 
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ accounts_file_name) 
      starting_accounts_json;
    Yojson.Basic.to_file (dirname ^ Filename.dir_sep ^ engine_file_name) 
      starting_engine_json;
    let me = MatchingEngine.create () in 
    let am = MatchingEngine.get_account_manager me in 
    {current_account = None ; account_manager = am; matching_engine = me}
