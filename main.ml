open AccountManager
open Account
open OrderBook
open MatchingEngine
open ReplHelpers

(** [main] creates a new AccountManager instance, State, and prompts the user
    to log into an account or register a new one. *)
let main () : unit =
  let s = inital_state () in 
  let state = match startup_action () with 
    | Login -> login s
    | Signup -> register s 
    | Delete -> delete s in
  repl state

let () = main ()

