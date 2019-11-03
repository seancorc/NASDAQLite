open Account

module type AccountManager = sig 
  type t
  val create : unit -> t
  val register : t -> string -> string -> Account.t
  val login : t -> string -> string -> Account.t
  val delete_user : t -> string -> string -> unit
  val accounts : t -> string list
end

module StringHash = struct
  type t = string
  let equal i j = i = j
  let hash i = Hashtbl.hash i
end
module D = Hashtbl.Make(StringHash)

exception InvalidUsername of string
exception InvalidPassword


let hash_pw p = Bcrypt.hash p

let verify_pw pw hash = Bcrypt.verify pw hash


module AccountManager : AccountManager = struct 
  type t = (Account.t * Bcrypt.hash) D.t
  let create () = D.create 10
  let register (m: t) (username: string) (password: string) : Account.t = 
    let exists = D.mem m username in 
    if exists then raise (InvalidUsername "Username is taken")
    else 
      let account = Account.create username in 
      let _ = D.add m username (account, (hash_pw password)) in 
      account

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