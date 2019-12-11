open AccountManager
open Account
open OrderBook
open MatchingEngine
open Dao
open Yojson.Basic.Util


type state = {username: string option}

type action = Login | Signup | Delete

(** [start_loop] prompts the user to sign in and returns  either 
    [Login] or [Signup] depending on the user's input. *)
let rec start_loop () = 
  print_endline ("Type 'login' to login, 'signup' to signup, or 'delete' to \
                  delete an account (type 'quit' to exit)");
  match String.lowercase_ascii (String.trim(read_line ())) with
  | "login" -> Login
  | "signup" -> Signup
  | "delete" -> Delete
  | "quit" -> Stdlib.exit 0;
  | _ -> print_endline "\n**I couldn't understand that command, please try again.**"; 
    start_loop ()

(** [startup_action] prints the welcome message and prompts [start_loop]
    so it goes in a loop until returning either [Login] or [Signup] once the 
    user submits a valid desired action. *)
let startup_action () = 
  print_endline ("Welcome to NASDAQLite!"); 
  start_loop ()

(** [print_balances b] Returns unit after printing each balance in the 
    list of balances [b]. *)
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
    let _ = Dao.login_user username password in
    {username=Some username}
  with 
  | Invalid_password ->
    print_endline "\n**Incorrect password**";
    s
  | (Invalid_username a) -> 
    print_endline a;
    s
  | _ -> 
    print_endline "\n**There was a server error, please try again.**";
    s

(** [register s] prompts the user to register a new account with a new username
    and password and returns the updated state [s] *)
let register (s : state) : state = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  try 
    let _ = Dao.signup_user username password in
    let new_s = {username=Some username} in 
    new_s  
  with 
  | Invalid_password ->
    print_endline "\n**Incorrect password**";
    s
  | (Invalid_username a) -> 
    print_endline a;
    s
  | _ -> 
    print_endline "\n**There was a server error, please try again.**";
    s

(** [delete s] prompts the user to delete an account with a username
    and password. *)
let delete (s : state) : state = 
  print_string "Username: ";
  let username = String.trim(read_line ()) in 
  print_string "Password: ";
  let password = (read_line ()) in 
  try 
    let _ = Dao.delete_user username password in
    print_endline "\n**Account deleted**";
    s
  with 
  | Invalid_password ->
    print_endline "\n**Incorrect password**";
    s
  | (Invalid_username a) -> 
    print_endline a;
    s
  | _ -> 
    print_endline "\n**There was a server error, please try again.**";
    s

(** [restart s] Returns updated state [s] after restarting current state with a 
    new login by running the start loop and then running either log or register 
    on the newer state and returning the result *)
let restart (s : state) : state  = 
  match start_loop () with 
  | Login -> login s
  | Signup -> register s
  | Delete -> delete s

(** [parse_order usr lst] Returns None if [lst] represents an invalid order or
    [Some (t, so)] where [t] is the ticker of the order and [so] is the 
    submitted order. *)
let parse_order (usr: string) (lst: string list) : (string * submitted_order) option = 
  try
    begin
      let ticker, submitted_order = match lst with 
        | ["buy"; ticker; amount; price] -> ticker, (Buy, (usr, (int_of_string amount), (float_of_string price), (Unix.time ())))
        | ["sell"; ticker; amount; price] -> ticker, (Sell, (usr, (int_of_string amount), (float_of_string price), (Unix.time ())))
        | ["buy market"; ticker; amount] -> ticker, (Buy, (usr, (int_of_string amount), max_float, (Unix.time ())))
        | ["sell market"; ticker; amount] -> ticker, (Sell, (usr, (int_of_string amount), min_float, (Unix.time ())))
        | _ -> raise Not_found in 
      Some ((ticker, submitted_order))
    end
  with exn -> None

(** [prompt_user_input username] Returns [s] where s is the user input
    submitted in the text UI after prompting the user to input a valid 
    action. *)
let prompt_user_input username = 
  let usd_balance_json = Dao.get_account_balance username in 
  let assoc_list = usd_balance_json |> to_assoc in 
  let usd_balance = assoc_list |> List.assoc "data" |> to_float in 
  let positions_json = Dao.get_account_positions username in
  let assoc_list = positions_json |> to_assoc in
  let json_positions = assoc_list |> List.assoc "data" |> to_list in 
  let positions = List.fold_left (fun acc pos -> 
      let assoc = pos |> to_assoc in
      let ticker = assoc |> List.assoc "ticker" |> to_string in 
      let amount = assoc |> List.assoc "amount" |> to_int in
      (ticker,amount) :: acc) [] json_positions  in
  let balances = ("USD", (int_of_float usd_balance)) :: positions in 
  let _ = print_balances balances in 
  print_endline "To log out of this account, type 'logout' and to create\
                 an asset, type 'create' (to exit type 'quit')";
  print_endline "To place an order input: order type (Buy, Sell, Buy Market,\
                 Sell Market), ticker, order size, price (only for Buy or Sell)";
  String.lowercase_ascii (read_line ())


(** [string_of_dir d] is the string representing direction [d], which is 
    either ['buy'] or ['sell'] *)
let string_of_dir = function
  | Buy -> "buy"
  | Sell -> "sell"

(** [read_input s username input] returns [s] after updating it by parsing
    the [input] from the user given by username: [username] and executing
    the commanded action *)
let read_input s username input = 
  match String.trim input with 
  | "logout" -> 
    {username = None}
  | "quit" ->
    Stdlib.exit 0;
  | "create" ->
    print_string "Ticker: ";
    let ticker = String.uppercase_ascii (String.trim(read_line ())) in 
    begin try 
        Dao.create_asset ticker;
        s
      with _ -> 
        print_endline "\n**There was a server error, please try again.**";
        s
    end
  | a -> 
    begin 
      (* Buy/Sell ticker amount price *)
      let lst = String.split_on_char ',' a in 
      let lst' = List.map (String.trim) lst in
      let parsed_order = parse_order username lst' in 
      match parsed_order with 
      | None -> print_endline "\n**Invalid order**"; s
      | Some (ticker, (dir, (username, amount, price, time))) ->
        begin
          try 
            Dao.execute_order username (string_of_dir dir) 
              (String.uppercase_ascii ticker)
              (string_of_int amount) (string_of_float price);
            print_endline "\n**Order placed**";
            s
          with e -> 
            print_endline "\n**There was a server error, please try again.**";
            s
        end
    end

(** [repl s] Returns unit and executes an infinite read-eval-print-loop
    to interact with the user and . It prints the account name,
    balances, and prompts the user to either log out or input an order. *)
let rec repl (s: state) : unit = 
  let st = match s.username with 
    | None -> restart s
    | Some username -> 
      begin
        let input = prompt_user_input username in 
        let updated_state = read_input s username input in 
        updated_state
      end in 
  repl st


(** [initial_state] Returns a new start state *)
let inital_state () =
  {username = None}
