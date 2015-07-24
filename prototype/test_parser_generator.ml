open Sparser
open Fparser

module PP = struct
  open Print_code
  open Format
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

module Test_expansion = struct
  
  open BasicFParser

  let pp_state s =
    let s = String.sub s.input s.index (s.length - s.index) in
    let s = if String.length s = 0 then "Empty" else "Left: " ^ s in
    Printf.printf "State: %s\n" s

  let pp_code pc =
    match pc with
    | T tpc -> PP.pp_code .<fun s -> .~(tpc .<s>.)>.
    | NT ntpc -> PP.pp_code ntpc

  module TestJsonParser = struct
    open FJsonParser

    let str_parser =
      ((lit '"') >> (TakeWhile (fun c -> .<.~c <> '"'>.)) << (lit '"'))

    let rec json_parser = FNT (lazy (CodeMap.gen (either ([
        ((fun o -> .<Obj .~o>.) <*> obj_parser);
        ((fun arr -> .<Arr .~arr>.) <*> arr_parser);
        ((fun s -> .<StringLit .~s>.) <*> str_parser)]))))

    and obj_parser = FNT (lazy (CodeMap.gen (
        (lit '{') >> (repsep member_parser (lit ',')) << (lit '}'))))

    and arr_parser = FNT (lazy (CodeMap.gen (
        (lit '[') >> (repsep json_parser (lit ',')) << (lit ']'))))

    and member_parser = FNT (lazy (CodeMap.gen (
        str_parser <~> ((lit ':') >> json_parser))))

    let parser_code = GenParser.gen_parser json_parser

    let pp_json_parser_code () = PP.pp_code parser_code

    let run_json () =
      let s = "{\"1\":[\"2\",\"3\"],\"4123\":\"1\"}" in
      let state = init_state_from_string s in
      match (Runcode.(!. parser_code)) state with
      | Success (r, s) -> pp_json r; pp_state s
      | Failure s -> print_endline "failed"; pp_state s
  end

  module TestT2Parser = struct
    open FT2Parser

    let rec t2_parser = FNT (lazy (CodeMap.gen (either ([
      (fun arr -> .<A .~arr>.) <*> arr_parser;
      (fun c -> .<C .~c>.) <*> lit 'c']))))
    and arr_parser = FNT (lazy (CodeMap.gen (
        (lit '[') >> t2_parser << (lit ']'))))

    let pp_t2 t =
      let rec bfs t =
        match t with
        | A next -> "A (" ^ (bfs next) ^ ")"
        | C c -> "C (" ^ (Char.escaped c) in
      print_endline (bfs t)

    let parser_code = GenParser.gen_parser t2_parser
    
    let pp_t2_parser_code () = PP.pp_code parser_code

    let run_t2 () =
      match Runcode.(!. parser_code) (init_state_from_string "[[[c]]]") with
      | Success (r, s) -> pp_t2 r; pp_state s
      | Failure s -> pp_state s

  end

end

(*
module Test = struct

  open Sparser.BasicFParser

  let state s = init_state_from_string s
 
  let dump_code (g, _, _, _) () =
    let parser_code = GenParser.gen_parser g in
    match parser_code with
    | T pc -> PP.pp_code .<fun s -> .~(pc .<s>.)>.
    | NT npc -> PP.pp_code npc

  let run_code (g, ss, p1, p2) () =
    let show_res res =
      match res with
      | Success (r, s) -> p1 r; p2 s
      | Failure s -> p2 s in
    let parser_code = GenParser.gen_parser g in
    let f s =
      match parser_code with
      | T tpc -> Runcode.(!. (tpc .<s>.)) |> show_res
      | NT ntpc -> Runcode.(!. ntpc) s |> show_res in
    List.iter f ss

  let pp_char_list cl =
    let buf = Buffer.create 10 in
    List.iter (Buffer.add_char buf) cl;
    Printf.printf "Success: %s\n" (Buffer.to_bytes buf)

  let pp_char_pair (c1, c2) = Printf.printf "%c %c\n" c1 c2

  let pp_state s =
    let s = String.sub s.input s.index (s.length - s.index) in
    let s = if String.length s = 0 then "Empty" else "Left: " ^ s in
    Printf.printf "State: %s\n" s

  let rec g1 = FNT (lazy (CodeMap.gen (g2 <~> g3)))
      and g2 = FNT (lazy (CodeMap.gen (lit 'd')))
      and g3 = (lit 'e')

  let repg = g1, [
      state "de";
      state "dec";
      state "cde"], pp_char_pair, pp_state
  
  let repsepg = Repsep ((lit 'c'), (lit ',')), [
      state "c,c";
      state "c";
      state "c,c,c,c,c,";
      state "";
      state ","], pp_char_list, pp_state

  let strg = ((lit '"') >> (TakeWhile (fun c -> .<.~c <> '"'>.)) << (lit '"')),
             [state "\"abc\"";state "\"a\"";state "\"\""; state "\""; state "\"a"],
             print_endline, pp_state
end
*)

let () = Runcode.(add_search_path "./_build")

open Test_expansion

let () = TestJsonParser.run_json ()
