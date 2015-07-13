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
    | Lit    : elem -> elem cgrammar
    | Seq    : 'a cgrammar * 'b cgrammar -> ('a * 'b) cgrammar
    | Left   : 'a cgrammar * 'b cgrammar -> 'a cgrammar
    | Right  : 'a cgrammar * 'b cgrammar -> 'b cgrammar
    | Either : 'a cgrammar list  -> 'a cgrammar
    | Rep    : 'a cgrammar -> ('a list) cgrammar
    | Repsep : 'a cgrammar * 'b cgrammar -> ('a list) cgrammar
    | TakeWhile : ('a -> bool) -> ('a list) cgrammar
    | Trans : ('a -> 'b) * 'a cgrammar -> 'b cgrammar
    | NT: string * 'a cgrammar Lazy.t -> 'a cgrammar


  val lit : elem -> elem cgrammar

  val (<~>) : 'a cgrammar -> 'b cgrammar -> ('a * 'b) cgrammar

  val (>>)  : _ cgrammar -> 'a cgrammar -> 'a cgrammar

  val (<<)  : 'a cgrammar -> _ cgrammar -> 'a cgrammar

  val either : 'a cgrammar list -> 'a cgrammar

  val rep : 'a cgrammar -> ('a list) cgrammar

  val repsep : 'a cgrammar -> 'b cgrammar -> ('a list) cgrammar

  val takewhile : ('a -> bool) -> ('a list) cgrammar

  val (<*>) : ('a -> 'b) -> 'a cgrammar -> 'b cgrammar
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
    | Lit    : elem -> elem cgrammar
    | Seq    : 'a cgrammar * 'b cgrammar -> ('a * 'b) cgrammar
    | Left   : 'a cgrammar * 'b cgrammar -> 'a cgrammar
    | Right  : 'a cgrammar * 'b cgrammar -> 'b cgrammar
    | Either : 'a cgrammar list  -> 'a cgrammar
    | Rep    : 'a cgrammar -> ('a list) cgrammar
    | Repsep : 'a cgrammar * 'b cgrammar -> ('a list) cgrammar
    | TakeWhile : ('a -> bool) -> ('a list) cgrammar
    | Trans : ('a -> 'b) * 'a cgrammar -> 'b cgrammar
    | NT: string * 'a cgrammar Lazy.t -> 'a cgrammar

  module Nonce = struct
    let i = ref 0L

    let nonce () = i := Int64.succ !i; Int64.to_string !i
  end

  include Nonce

  let lit elem = Lit elem

  let (<~>) a b = Seq (a, b)

  let (>>) a b = Right (a, b)

  let (<<) a b = Left (a, b)

  let either al = Either al

  let rep a = Rep a

  let repsep a b = Repsep (a, b)

  let takewhile f = TakeWhile f

  let (<*>) f a = Trans (f, a)

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

  type json = Obj of obj | Arr of arr | StringLit of string
  and  obj = member list
  and  member = string * json
  and  arr = json list

  type t2 = A of t3 | S of string
  and t3 = t2

end
