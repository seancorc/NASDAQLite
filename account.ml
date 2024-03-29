module type Account = sig 
  type t
  val to_json_string : t -> string -> string
  val create_empty : string -> t
  val create : string -> float -> t
  val username : t -> string
  val balance : t -> float
  val positions : t -> (string * int) list
  val position : t -> string -> int
  val set_position : t -> string -> int -> unit
  val set_balance : t -> float -> unit
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

  type t = {
    username: string;
    balance: float ref;
    positions: int D.t;
  }

  (** [create_order_from_json string o] creates a json string of the orders in 
      [orders]. *)
  let create_orders_json_string orders =
    let rec create base_string ords =   
      match ords with 
      | [] -> base_string
      | (ticker, amt) :: t -> 
        base_string ^ "{" ^ "\"ticker\": " ^ ticker ^ ",\n\"amount\": "
        ^ amt ^ "}" in 
    (create "[" orders) ^ "]"

  let to_json_string a hashed_pass =
    let orders = D.fold (fun k v acc -> 
        ("\"" ^ k ^ "\"",(string_of_int v)) :: acc) a.positions [] in 
    let json_orders = create_orders_json_string orders in 
    "{
      \"username\":\"" ^ a.username ^"\",
      \"hashed_pass\": \"" ^ hashed_pass ^ "\",
      \"balance\": " ^ (string_of_float !(a.balance)) ^ "0,
      \"orders\": " ^ json_orders ^ "}\n"

  let create (usr: string) (start_balance: float) = 
    let positions = D.create 10 in 
    {username = usr; balance = ref start_balance; positions = positions}

  let create_empty usr = 
    let positions = D.create 10 in 
    {username =usr ; balance = ref 0.0; positions = positions}

  let username a = a.username

  let balance (a: t) : float = 
    let f = a.balance in 
    !f

  let set_balance (a: t) (balance: float) : unit = 
    a.balance := balance

  let position (a: t) (t: string) : int = 
    let p = D.find_opt a.positions t in 
    match p with 
    | Some amount -> amount
    | None -> 0

  let set_position (a: t) (t: string) (v: int) = D.replace a.positions t v

  let positions a = D.fold (fun k v c -> (k, v) :: c) a.positions []

  let format fmt a = Format.pp_print_string fmt 
      (Format.sprintf "Account: %s" a.username)
end
