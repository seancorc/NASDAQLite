module type Account = sig 
  type t
  val create : string -> t
  val username : t -> string
  val balance : t -> string -> float
  val balances : t -> (string * float) list
  val set_balance : t -> string -> float -> unit
  val format : Format.formatter -> t -> unit
end



module Account : Account = struct 
  module StringHash = struct
    type t = string
    let equal i j = i = j
    let hash i = Hashtbl.hash i
  end

  module D = Hashtbl.Make(StringHash)
  type t = {
    username: string;
    balances: float D.t;
  }
  let create usr = 
    let balances = D.create 10 in 
    {username = usr; balances = balances}

  let username a = a.username
  let balance a t = D.find a.balances t
  let set_balance a t v = D.replace a.balances t v

  let balances a = D.fold (fun k v c -> (k, v) :: c) a.balances []

  let format fmt a = Format.pp_print_string fmt 
      (Format.sprintf "Account: %s" a.username)
end