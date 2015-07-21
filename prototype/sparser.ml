(* Some basic parsers. They are slow. These parser modules should be
   put into functor (fparser.ml) and then we'll have an optimized parser *)

module Nonce = struct
  
  let i = ref 0L

  let nonce () = i := Int64.succ !i; Int64.to_string !i
end


module Json_parser = struct
  open Nparser.BasicCharParser

  open Nonce

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


module Test_I_parser = struct

  type t2 = A of t3 | C of char and t3 = t2

end

let () = Runcode.(add_search_path "./_build")
