
module type Account = sig 
  type t
  val create : string -> t
  val username : t -> string
  val balance : t -> string -> float
  val balances : t -> (string * float) list
  val set_balance : t -> string -> float -> unit
  val format : Format.formatter -> t -> unit
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

module Account : Account = struct 

  (** [D] is a Hashtbl using StringHash as a key *)
  module D = Hashtbl.Make(StringHash)

  (**
     AF: Represents Accounts by storing its username and balances in a record
     with balances stored in a Hashtbl mapping strings to floats. 
     RI: No additional constraints. *)
  type t = {
    username: string;
    balances: float D.t;
  }
  let create usr = 
    let balances = D.create 10 in 
    {username = usr; balances = balances}

  let username a = a.username

  let balance a t = 
    try D.find a.balances t
    with Not_found -> 0.0

  let set_balance a t v = D.replace a.balances t v

  let balances a = D.fold (fun k v c -> (k, v) :: c) a.balances []

  let format fmt a = Format.pp_print_string fmt 
      (Format.sprintf "Account: %s" a.username)
end
