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

  type 'a parser_code = state code -> 'a parse_result code

  type 'a parser_generator =
    'a cgrammar -> 'a parser_code

  module CodeMap = MakeCodeMap(struct type 'a v = 'a parser_code ref end)

  type _ cgrammar +=
    | FNT : ('a CodeMap.key * 'a cgrammar) Lazy.t -> 'a cgrammar

end

module FJsonParser = struct
  open BasicFParser
  
  type json = Obj of obj | Arr of arr | StringLit of string
  and  obj = member list
  and  member = string * json
  and  arr = json list

  let str_parser =
    ((lit '"') >> (TakeWhile (fun c -> .<.~c <> '"'>.)) << (lit '"'))

  let rec json_parser = FNT (lazy (CodeMap.gen (either ([
      ((fun o -> Obj o) <*> obj_parser);
      ((fun arr -> Arr arr) <*> arr_parser);
      ((fun s -> StringLit s) <*> str_parser)]))))

  and obj_parser = FNT (lazy (CodeMap.gen (
      (lit '{') >> (repsep member_parser (lit ',')) << (lit '}'))))

  and arr_parser = FNT (lazy (CodeMap.gen (
      (lit '[') >> (repsep json_parser (lit ',')) << (lit ']'))))

  and member_parser = FNT (lazy (CodeMap.gen (
      str_parser <~> ((lit ':') >> json_parser))))
end

module FTIParser = struct
  open BasicFParser

  type t2 = A of t3 | C of char and t3 = t2

  (* Failed because we cannot handle left recursion *)
  let rec t2_parser = FNT (lazy (CodeMap.gen (either ([
      (fun arr -> A arr) <*> arr_parser;
      (fun c -> C c) <*> lit 'c']))))
  and arr_parser = FNT (lazy (CodeMap.gen (
      (lit '[') >> t2_parser << (lit ']'))))

  (* Successful *)
  let test = FNT (lazy (CodeMap.gen (lit 'c')))

  (* Successful *)
  let rec test2 = FNT (lazy (CodeMap.gen (lit 'c' <~> sub_parser)))
  and sub_parser = FNT (lazy (CodeMap.gen (lit 'd')))
end


let () = Runcode.(add_search_path "./_build")
