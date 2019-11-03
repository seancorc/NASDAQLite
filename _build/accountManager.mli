open Account

exception InvalidUsername of string
exception InvalidPassword

module type AccountManager = sig 
  type t
  val create : unit -> t
  val register : t -> string -> string -> Account.t
  val login : t -> string -> string -> Account.t
  val delete_user : t -> string -> string -> unit
  val accounts : t -> string list
end

module AccountManager : AccountManager