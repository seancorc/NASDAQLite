open Account
open Yojson.Basic.Util

module type AccountManager = sig 
  type t
  val to_json_string : t -> string
  val create : unit -> t
  val register : t -> string -> string -> Account.t
  val load_from_json : Yojson.Basic.t -> t
  val set_account_balance : t -> string -> float -> unit
  val inc_account_balance : t -> string -> float -> unit
  val dec_account_balance : t -> string -> float -> unit
  val set_account_position : t -> string -> string -> int -> unit
  val inc_account_position : t -> string -> string -> int -> unit
  val dec_account_position : t -> string -> string -> int -> unit
  val login : t -> string -> string -> Account.t
  val delete_user : t -> string -> string -> unit
  val accounts : t -> string list
end

(** [StringHash] is a module representing a HashedType using String as the key*)
module StringHash = struct
  type t = string

  (** [equal i j] is true if i is structurally equal to j and false otherwise *)
  let equal i j = i = j

  (** [hash i] is i after running a hashing algorithm on it for use in 
      Hashtbl *)
  let hash i = Hashtbl.hash i
end

(** [D] is a Hashtbl using StringHash as a key *)
module D = Hashtbl.Make(StringHash)

exception Invalid_username of string
exception Invalid_password


(** [hash_pw p] is the hash of p after running the bcrypt algorithm on it *)
let hash_pw p = Bcrypt.hash p


(** [verify_pw p h] is true iff [p] matches correctly the password that was 
    hashed to create the hash [h]. False otherwise *)
let verify_pw pw hash = Bcrypt.verify pw hash


module AccountManager : AccountManager = struct 

  (**
     AF: Represents AccountManager as a Hashtbl mapping usernames to the tuple 
     containing the corresponding account and the hash of their password
     RI: The AccountManager never adds more than one account with the same 
     username. 
     Additionally, the Hashtbl does not use add unless it verifies that there 
     is no other account that it will be hiding, so as to avoid seemingly 
     random behaviors. *)
  type t = (Account.t * Bcrypt.hash) D.t

  let create () = D.create 10

  let register (m: t) (username: string) (password: string) : Account.t = 
    let exists = D.mem m username in 
    if exists then raise (Invalid_username "Username is taken")
    else 
      let account = Account.create_empty username in 
      let _ = D.add m username (account, (hash_pw password)) in 
      account

  let to_json_string (am : t) = 
    let accounts = D.fold (fun _ (acct, hash) acc -> 
        (acct,Bcrypt.string_of_hash hash) :: acc) am [] in 
    let rec create base_string accts = 
      match accts with 
      | [] -> base_string
      | (acct,hash) :: t -> 
        let acct_string = (Account.to_json_string acct hash) in
        create (base_string ^ acct_string ^ (if (List.length t) >= 1 then 
                                               "," else "")) t in
    create "{\"users\": [" accounts ^ "]}"

  (** [populate_orders orders acct] parses an indivdual json object from 
      [orders] into a position and adds that position to [acct]'s positions *)
  let rec populate_orders orders acct =
    match orders with 
    | [] -> ()
    | h :: t -> 
      let ticker = h |> to_assoc |> List.assoc "ticker" |> to_string in
      let amt = h |> to_assoc |> List.assoc "amount" |> to_int in 
      Account.set_position acct ticker amt;
      populate_orders t acct;
      ()

  (** [populate_manager am users] parses an indivdual json object from [users]
      into a user and adds that user to AccountManager [am] *)
  let rec populate_manager am users = 
    match users with 
    | [] -> ()
    | h :: t -> 
      let username = h |> to_assoc |> List.assoc "username" |> to_string in
      let hashed_pass = h |> to_assoc |> List.assoc "hashed_pass" |> to_string 
      in let balance = h |> to_assoc |> List.assoc "balance" |> to_float in
      let account = Account.create username balance in 
      let _ = D.add am username (account, (Bcrypt.hash_of_string hashed_pass)) 
      in let list_of_orders = (h |> to_assoc |> List.assoc "orders" |> to_list) 
      in populate_orders list_of_orders account;
      populate_manager am t;
      ()

  let load_from_json json = 
    let users = json |> to_assoc |> List.assoc "users" |> to_list in 
    let am = create () in
    populate_manager am users;
    am

  let set_account_balance (m: t) (username: string) (a: float) = 
    let (account, _) = D.find m username in 
    Account.set_balance account a

  let inc_account_balance (m: t) (username: string) (a: float) = 
    let (account, _) = D.find m username in 
    let cur_balance = Account.balance account in 
    let new_balance = cur_balance +. a in 
    Account.set_balance account new_balance

  let dec_account_balance (m: t) (username: string) (a: float) = 
    let (account, _) = D.find m username in 
    let cur_balance = Account.balance account in 
    let new_balance = cur_balance -. a in 
    Account.set_balance account new_balance

  let set_account_position (m: t) (username: string) (t: string) (a: int) = 
    let (account, _) = D.find m username in 
    Account.set_position account t a

  let inc_account_position (m: t) (username: string) (t: string) (a: int) = 
    let (account, _) = D.find m username in 
    let cur_balance = Account.position account t in 
    let new_balance = cur_balance + a in 
    Account.set_position account t new_balance

  let dec_account_position (m: t) (username: string) (t: string) (a: int) = 
    let (account, _) = D.find m username in 
    let cur_balance = Account.position account t in 
    let new_balance = cur_balance - a in 
    Account.set_position account t new_balance

  let login (m: t) (username: string) (password: string) : Account.t = 
    match D.find_opt m username with 
    | Some (account, password_hash) ->
      begin
        if (verify_pw password password_hash) then account 
        else raise Invalid_password
      end
    | None -> raise (Invalid_username "Username does not exist")

  let delete_user (m: t) (username: string) (password: string) : unit = 
    match D.find_opt m username with 
    | Some (account, password_hash) ->
      begin
        if (verify_pw password password_hash) then 
          let _ = D.remove m username in 
          ()
        else raise Invalid_password
      end
    | None -> raise (Invalid_username "Username does not exist")

  let accounts (m: t) : string list = 
    D.fold (fun k v l -> k :: l) m []

end