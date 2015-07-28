(* This file contains modules that should be put in a separate file as
   required by MetaOCaml. CSP issues... *)
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

module BasicFParser = struct

  open Codemap
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

  type json = Obj of obj | Arr of arr | StringLit of string
  and  obj = member list
  and  member = string * json
  and  arr = json list

  let pp_json json =
    let rec bfs t =
      match t with
      | Obj o ->
        let s = String.concat ", " (List.map (fun (s, j) -> s ^ " : " ^ (bfs j) ^ "\n") o) in
        Printf.sprintf "{ %s}" s
      | Arr arr ->
        let s = String.concat "\n" (List.map bfs arr) in
        Printf.sprintf "[ %s ]" s
      | StringLit s -> Printf.sprintf "\"%s\"" s in
    print_endline (bfs json)

end

module FT2Parser = struct

  type t2 = A of t3 | C of char and t3 = t2

  let pp_t2 t =
    let rec bfs t =
      match t with
      | A next -> "A (" ^ (bfs next) ^ ")"
      | C c -> "C (" ^ (Char.escaped c) in
    print_endline (bfs t)

end

let () = Runcode.(add_search_path "./_build")
