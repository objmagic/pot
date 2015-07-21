module type TMAP = sig
  type _ key
  type _ value
  type iter_f = { f : 'a. 'a key -> 'a value -> unit }
  type t
  val fresh_key : 'a -> string -> 'a key
  val key_id : 'a key -> string
  val empty : t
  val add : t -> 'a key -> 'a value -> t
  val find : t -> 'a key -> 'a value option
  val iter : iter_f -> t -> unit
end

module Tmap (V: sig type _ value end) : TMAP with type 'a value := 'a V.value = struct

  module Nonce = struct
    let i = ref 0L

    let nonce () = i := Int64.succ !i; Int64.to_string !i
  end

  type _ ttype = ..
  type (_, _) eql = Refl: ('a, 'a) eql

  type 'a key = {
    id: string;
    k : 'a ttype;
    eq : 'b. 'b ttype -> ('a, 'b) eql option
  }

  type 'a value = 'a V.value

  type iter_f = { f : 'a. 'a key -> 'a value -> unit }

  let fresh_key (type a) (v: a) id =
    let module M = struct type _ ttype += T : a ttype end in
    let eq : type b. b ttype -> (a, b) eql option =
      function M.T -> Some Refl | _ -> None in
    { id ; k = M.T; eq }

  let key_id k = k.id

  type t =
    | Nil : t
    | Cons : 'a key * 'a value * t -> t

  let empty = Nil

  let add t k v =
    Cons (k, v, t)

  let rec find : type a. t -> a key -> a value option =
    fun t k ->
      match t with
      | Nil -> None
      | Cons ({k = k'}, v, rest) ->
        match k.eq k' with
        | Some Refl -> Some v
        | None -> find rest k

  let rec iter : iter_f -> t -> unit = fun f t ->
    match t with
    | Nil -> ()
    | Cons (k, v, res) ->
      f.f k v; iter f res
end
