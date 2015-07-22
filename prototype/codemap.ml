open Nparser.BasicCharParser

module type CMAP = sig
  type _ key
  type _ value
  type iterf = { f : 'a. 'a key -> 'a value -> unit }
  type t
  val fresh_key : 'a cgrammar -> 'a key
  val gen : 'a cgrammar -> ('a key * 'a cgrammar)
  val empty : t
  val add : t -> 'a key -> 'a value -> t
  val find : t -> 'a key -> 'a value option
  val iter : t -> iterf -> unit
  val size : t -> int
end

module MakeCodeMap (V : sig type _ v end) : CMAP with type 'a value := 'a V.v = struct

  type _ ntt = ..
  type (_, _) eql = Refl: ('a, 'a) eql

  type 'a key = {
    k : 'a ntt;
    eq : 'b. 'b ntt -> ('a, 'b) eql option
  }

  type 'a value = 'a V.v

  type iterf = { f : 'a. 'a key -> 'a value -> unit }

  let fresh_key (type a) (v: a cgrammar) =
    let module M = struct type _ ntt += T : a ntt end in
    let eq : type b. b ntt -> (a, b) eql option =
      function M.T -> Some Refl | _ -> None in
    { k = M.T; eq }

  let gen v =
    (fresh_key v), v

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

  let rec iter : t -> iterf -> unit = fun t f ->
    match t with
    | Nil -> ()
    | Cons (k, v, res) ->
      f.f k v; iter res f

  let size t =
    let rec loop t acc =
      match t with
      | Nil -> acc
      | Cons (_, _, res) -> loop res (acc + 1) in
    loop t 0

end
