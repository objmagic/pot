module type Reader = sig
  type t

  type pos

  type elem

  val read : t -> pos -> elem

  val length : t -> int

  type state = {
    input : t;
    length: int;
    index: pos;
    row: int;
    col: int;
  }

  val init_state_from_t : t -> state

end

module type Char_Reader_S =
  Reader with type elem = char
          and type pos = int
          and type t = string

module Char_Reader : Char_Reader_S = struct
  type t = string

  type pos = int

  type elem = char

  type state = {
    input : t;
    length: int;
    index: pos;
    row: int;
    col: int
  }

  let read s i = String.get s i

  let length s = String.length s

  let init_state_from_t s = {
    input = s;
    length = String.length s;
    index = 0;
    row = 0;
    col = 0;
  }

end

module type Parser = sig
  include Reader

  type 'a parse_result =
    | Success of 'a * state
    | Failure of state

  type 'a parser = state -> 'a parse_result

  type _ cgrammar =
    | Lit    : string * elem -> elem cgrammar
    | Seq    : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> ('a * 'b) cgrammar
    | Left   : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> 'a cgrammar
    | Right  : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> 'b cgrammar
    | Either : string * 'a cgrammar lazy_t list  -> 'a cgrammar
    | Rep    : string * 'a cgrammar lazy_t -> ('a list) cgrammar
    | Repsep : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> ('a list) cgrammar
    | TakeWhile : string * ('a -> bool) -> ('a list) cgrammar
    | Trans : string * ('a -> 'b) * 'a cgrammar lazy_t -> 'b cgrammar

  val lit : elem -> elem cgrammar

  val (<~>) : 'a cgrammar lazy_t -> 'b cgrammar lazy_t -> ('a * 'b) cgrammar

  val (>>)  : _ cgrammar lazy_t -> 'a cgrammar lazy_t -> 'a cgrammar

  val (<<)  : 'a cgrammar lazy_t -> _ cgrammar lazy_t -> 'a cgrammar

  val either : 'a cgrammar lazy_t list -> 'a cgrammar

  val rep : 'a cgrammar lazy_t -> ('a list) cgrammar

  val repsep : 'a cgrammar lazy_t -> 'b cgrammar lazy_t -> ('a list) cgrammar

  val takewhile : ('a -> bool) -> ('a list) cgrammar

  val (<*>) : ('a -> 'b) -> 'a cgrammar lazy_t -> 'b cgrammar
end

module Parser (Reader: Reader) : Parser
  with type t    = Reader.t
   and type pos  = Reader.pos
   and type elem = Reader.elem = struct

  include Reader

  type 'a parse_result =
    | Success of 'a * state
    | Failure of state

  type 'a parser = state -> 'a parse_result

  type _ cgrammar =
    | Lit    : string * elem -> elem cgrammar
    | Seq    : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> ('a * 'b) cgrammar
    | Left   : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> 'a cgrammar
    | Right  : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> 'b cgrammar
    | Either : string * 'a cgrammar lazy_t list  -> 'a cgrammar
    | Rep    : string * 'a cgrammar lazy_t -> ('a list) cgrammar
    | Repsep : string * 'a cgrammar lazy_t * 'b cgrammar lazy_t -> ('a list) cgrammar
    | TakeWhile : string * ('a -> bool) -> ('a list) cgrammar
    | Trans : string * ('a -> 'b) * 'a cgrammar lazy_t -> 'b cgrammar

  module Nonce = struct
    let i = ref 0L

    let nonce () = i := Int64.succ !i; Int64.to_string !i
  end

  include Nonce

  let lit elem = Lit (nonce (), elem)

  let (<~>) a b = Seq (nonce (), a, b)

  let (>>) a b = Right (nonce (), a, b)

  let (<<) a b = Left (nonce (), a, b)

  let either al = Either (nonce (), al)

  let rep a = Rep (nonce (), a)

  let repsep a b = Repsep (nonce (), a, b)

  let takewhile f = TakeWhile (nonce (), f)

  let (<*>) f a = Trans (nonce (), f, a)

end

module Char_parser = struct
  include Parser(Char_Reader)
  let init_state_from_string str = {
    input = str;
    length = String.length str;
    index = 0;
    row = 0;
    col = 0;
  }
end

module type XS = sig
  include module type of Char_parser

  val takeWhile: (char -> bool) -> string cgrammar

  val (<***>) : ('a cgrammar) -> ('a -> 'b) -> ('b cgrammar)

  val repsep: ('a cgrammar) -> elem cgrammar -> ('a list cgrammar)
end


module M (X: XS) =
struct

  open X
  open Char_parser

  type json = Obj of obj | Arr of arr | StringLit of string | Int of int | Float of float
  and obj = member list
  and member = (string * json) list
  and arr = json list

  type 'a rule = {
    name: string;
    grammar: 'a cgrammar;
  }

end
