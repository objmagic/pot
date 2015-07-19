(* GADT associative list *)
module Eq = struct
  type (_, _) t = Refl : ('a, 'a) t
end

module type TypeEqWitness = sig
  type 'a key
  val equal : 'a key -> 'b key -> ('a, 'b) Eq.t option
end

module type CodeMapS = sig
  include TypeEqWitness
  type t
  val empty : t
  val add : t -> 'a key -> 'a code -> t
  val find : t -> 'a key -> 'a code option
end

module CodeMap (TyEq: TypeEqWitness) : CodeMapS with type 'a key = 'a TyEq.key = struct
  include TyEq

  type t =
    | Nil : t
    | Cons : 'a TyEq.key * 'a code * t -> t

  let cast : type a b. (a, b) Eq.t -> a code -> b code =
    fun Eq.Refl x -> x

  let empty = Nil

  let add : type a. t -> a TyEq.key -> a code -> t =
    fun t k v -> Cons (k, v, t)

  let rec find : type a. t -> a TyEq.key -> a code option =
    fun t key ->
      match t with
      | Nil -> None
      | Cons (k, v, res) ->
        match TyEq.equal k key with
        | None -> find res key
        | Some eq -> Some (cast eq v)

end
