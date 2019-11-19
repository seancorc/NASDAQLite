open Account

module type AccountManager = sig 
  type t
  val create : unit -> t
  val register : t -> string -> string -> Account.t
                                          <<<<<<< HEAD
  val set_account_balance : t -> string -> float -> unit
  val inc_account_balance : t -> string -> float -> unit
  val dec_account_balance : t -> string -> float -> unit
  val set_account_position : t -> string -> string -> int -> unit
  val inc_account_position : t -> string -> string -> int -> unit
  val dec_account_position : t -> string -> string -> int -> unit
                                                             =======
  val set_account_balance : t -> string -> string -> float -> unit
  val load_from_dir : string -> t
  val write_accounts_to_dir : string -> t -> unit
  val inc_account_balance : t -> string -> string -> float -> unit
  val dec_account_balance : t -> string -> string -> float -> unit
    >>>>>>> Implement write accounts to file
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

exception InvalidUsername of string
exception InvalidPassword


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
    if exists then raise (InvalidUsername "Username is taken")
    else 
      let account = Account.create_empty username in 
      let _ = D.add m username (account, (hash_pw password)) in 
      account

  let set_account_balance (m: t) (username: string) (a: float) = 
    let (account, _) = D.find m username in 
    Account.set_balance account t a

  let rec populate_manager am ic = 
    try 
      let line = input_line ic in
      let comma_index = String.index line ',' in
      let username = String.sub line 0 comma_index in
      let pass_lenth = (String.length line - (String.length username) - 1) in
      let hashed_pass = String.sub line (comma_index + 1) pass_lenth in
      let account = Account.create username in 
      let _ = D.add am username (account, (Bcrypt.hash_of_string hashed_pass)) in 
      populate_manager am ic;
    with e ->
      if e = End_of_file then ()
      else raise e

  (** Searches for file 'accounts.csv' and returns input_channel associated 
      with that file *)
  let rec get_input_channel handle dirname = 
    let filename = Unix.readdir handle in
    match filename with 
    | "accounts.csv" -> 
      open_in (dirname ^ Filename.dir_sep ^ filename)
    | s -> get_input_channel handle dirname

  let load_from_dir dirname = 
    let ic = get_input_channel (Unix.opendir dirname) dirname in
    let am = create () in
    populate_manager am ic;
    am

  let rec get_output_channel handle dirname = 
    let filename = Unix.readdir handle in
    match filename with 
    | "accounts.csv" -> 
      open_out (dirname ^ Filename.dir_sep ^ filename)
    | s -> get_output_channel handle dirname

  let to_list (m: t) : (Account.t * Bcrypt.hash) list = 
    D.fold (fun k v l -> v :: l) m []

  let rec write_accounts_to_dir dirname am = 
    let full_am_list = to_list am in
    let oc = get_output_channel (Unix.opendir dirname) dirname in
    let rec populate_file am_list = 
      match am_list with
      | [] -> close_out oc;
      | h :: t -> 
        let username = Account.username (fst h) in 

        let hashed_password = Bcrypt.string_of_hash (snd h) in
        Printf.fprintf oc "%s,%s\n" username hashed_password;
        populate_file t
    in populate_file full_am_list



  let inc_account_balance (m: t) (username: string) (t: string) (a: float) = 
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
        else raise InvalidPassword
      end
    | None -> raise (InvalidUsername "Username does not exist")

  let delete_user (m: t) (username: string) (password: string) : unit = 
    match D.find_opt m username with 
    | Some (account, password_hash) ->
      begin
        if (verify_pw password password_hash) then 
          let _ = D.remove m username in 
          ()
        else raise InvalidPassword
      end
    | None -> raise (InvalidUsername "Username does not exist")

  let accounts (m: t) : string list = 
    D.fold (fun k v l -> k :: l) m []

end