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

  type _ cgrammar =
    | Lit    : elem -> elem cgrammar
    | Seq    : 'a cgrammar * 'b cgrammar -> ('a * 'b) cgrammar
    | Left   : 'a cgrammar * 'b cgrammar -> 'a cgrammar
    | Right  : 'a cgrammar * 'b cgrammar -> 'b cgrammar
    | Either : 'a cgrammar * 'b cgrammar -> [`Left of 'a | `Right of 'b] cgrammar
    | Rep    : 'a cgrammar -> ('a list) cgrammar

  val lit : elem -> elem cgrammar

  val (<~>) : 'a cgrammar -> 'b cgrammar -> ('a * 'b) cgrammar
       
  val (>>)  : _ cgrammar -> 'a cgrammar -> 'a cgrammar
      
  val (<<)  : 'a cgrammar -> _ cgrammar -> 'a cgrammar
   
  val (<|>) : 'a cgrammar -> 'b cgrammar -> [`Left of 'a | `Right of 'b] cgrammar

  val rep : 'a cgrammar -> ('a list) cgrammar

end

module Parser (Reader: Reader) : Parser
  with type t    = Reader.t
   and type pos  = Reader.pos
   and type elem = Reader.elem = struct

  include Reader

  type 'a parse_result =
    | Success of 'a * state
    | Failure of state

  type _ cgrammar =
    | Lit    : elem -> elem cgrammar
    | Seq    : 'a cgrammar * 'b cgrammar -> ('a * 'b) cgrammar
    | Left   : 'a cgrammar * 'b cgrammar -> 'a cgrammar
    | Right  : 'a cgrammar * 'b cgrammar -> 'b cgrammar
    | Either : 'a cgrammar * 'b cgrammar -> [`Left of 'a | `Right of 'b] cgrammar
    | Rep    : 'a cgrammar -> ('a list) cgrammar

  let lit elem = Lit elem

  let (<~>) a b = Seq (a, b)

  let (>>) a b = Right (a, b)

  let (<<) a b = Left (a, b)

  let (<|>) a b = Either (a, b)

  let rep a = Rep a
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
