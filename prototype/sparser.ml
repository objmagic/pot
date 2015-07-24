(* Some basic parsers. They are slow. These parser modules should be
   put into functor (fparser.ml) and then we'll have an optimized parser *)

module JsonParser = struct
  open Nparser.BasicCharParser

  type json = Obj of obj | Arr of arr | StringLit of string
  and  obj = member list
  and  member = string * json
  and  arr = json list

  let str_parser =
    ((lit '"') >> (TakeWhile (fun c -> .<.~c <> '"'>.)) << (lit '"'))

  let rec json_parser = NT ("json_parser", lazy (either ([
      ((fun o -> Obj o) <*> obj_parser);
      ((fun arr -> Arr arr) <*> arr_parser);
      ((fun s -> StringLit s) <*> str_parser)])))
  and obj_parser = NT ("obj_parser", lazy (
    (lit '{') >> (repsep member_parser (lit ',')) << (lit '}')))
  and arr_parser = NT ("arr_parser", lazy (
    (lit '[') >> (repsep json_parser (lit ',')) << (lit ']')))
  and member_parser = NT ("member_parser", lazy (
      str_parser <~> ((lit ':') >> json_parser)))
end


open Codemap

module BasicFParser = struct

  include Nparser.BasicCharParser

  type 'a t_parser_code = state code -> 'a parse_result code

  type 'a nt_parser_code = (state -> 'a parse_result) code

  type 'a parser_code = T of 'a t_parser_code | NT of 'a nt_parser_code

  module CodeMap = MakeCodeMap(struct type 'a v = 'a nt_parser_code end)

  type _ cgrammar +=
    | Trans : ('a code -> 'b code) * 'a cgrammar -> 'b cgrammar
    | FNT : ('a CodeMap.key * 'a cgrammar) Lazy.t -> 'a cgrammar

  let (<*>) f g = Trans (f, g)

end

module FJsonParser = struct
  open BasicFParser

  type json = Obj of obj | Arr of arr | StringLit of string
  and  obj = member list
  and  member = string * json
  and  arr = json list

end

module FT2Parser = struct
  open BasicFParser

  type t2 = A of t3 | C of char and t3 = t2

  (* Failed because we cannot handle left recursion *)

  (* Successful *)
  let test_nt = FNT (lazy (CodeMap.gen (lit 'c')))

  (* Successful *)
  let rec test_nt_2 = FNT (lazy (CodeMap.gen (lit 'c' <~> sub_parser)))
  and sub_parser = FNT (lazy (CodeMap.gen (lit 'd')))
end

let () = Runcode.(add_search_path "./_build")
