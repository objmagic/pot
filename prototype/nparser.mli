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

module type CharReaderS =
  Reader with type elem = char
          and type pos = int
          and type t = string

module CharReader : CharReaderS

module type Parser = sig
  
  include Reader

  type 'a parse_result =
    | Success of 'a * state
    | Failure of state

  type 'a parser = state -> 'a parse_result

  type _ cgrammar = ..
  type _ cgrammar +=
    | Exact    : elem -> elem cgrammar
    | Seq    : 'a cgrammar * 'b cgrammar -> ('a * 'b) cgrammar
    | Left   : 'a cgrammar * 'b cgrammar -> 'a cgrammar
    | Right  : 'a cgrammar * 'b cgrammar -> 'b cgrammar
    | Either : 'a cgrammar list  -> 'a cgrammar
    | Rep    : 'a cgrammar -> ('a list) cgrammar
    | Repsep : 'a cgrammar * 'b cgrammar -> ('a list) cgrammar
    | Trans : ('a -> 'b) * 'a cgrammar -> 'b cgrammar
    | NT:  string * 'a cgrammar Lazy.t -> 'a cgrammar

  val exact : elem -> elem cgrammar

  val (<~>) : 'a cgrammar -> 'b cgrammar -> ('a * 'b) cgrammar

  val (>>)  : _ cgrammar -> 'a cgrammar -> 'a cgrammar

  val (<<)  : 'a cgrammar -> _ cgrammar -> 'a cgrammar

  val either : 'a cgrammar list -> 'a cgrammar

  val rep : 'a cgrammar -> ('a list) cgrammar

  val repsep : 'a cgrammar -> 'b cgrammar -> ('a list) cgrammar

  val (<*>) : ('a -> 'b) -> 'a cgrammar -> 'b cgrammar
end

module BasicParser(Reader: Reader) : Parser
  with type t = Reader.t
   and type pos = Reader.pos
   and type elem = Reader.elem

module type BCP = sig
  include Parser with type t = CharReader.t
                  and type pos = CharReader.pos
                  and type elem = CharReader.elem

  type _ cgrammar +=
    | TakeWhile : (char code -> bool code) -> string cgrammar

  val takewhile : (char code -> bool code) -> string cgrammar

  val lit : elem -> elem cgrammar

  val init_state_from_string : string -> state

end

module BasicCharParser : BCP
