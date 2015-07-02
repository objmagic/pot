open Print_code
open Format

module PP = struct
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

module CFParser = struct

  open Nparser.Char_parser

  (* state is dynamic, grammar is static *)

  type 'a parser_code = state code -> 'a parse_result code

  type 'a parser_generator =
    'a cgrammar -> 'a parser_code

  let gen_lit_parser : char -> char parser_code =
    fun c state -> .<
      let ix = (.~state).index in
      let f = Failure .~state in
      if ix < (.~state).length then
        let e1 = (.~state).input.[ix] in
        if e1 = c then .~(
          if c = '\n' then
            .<Success (e1, {.~state with row = (.~state).row + 1; col = 0; index = ix + 1})>.
          else
            .<Success (e1, {.~state with col = (.~state).col + 1; index = ix + 1})>.)
        else f
      else f
    >.
    
  let gen_seq_parser :
    ('a parser_code) -> ('b parser_code) -> ('a * 'b) parser_code =
    fun pcx pcy state -> .<
      let res1 = .~(pcx state) in
        match res1 with
        | Success (r1, state1) -> begin
          let res2 = .~(pcy .<state1>.) in
          match res2 with
          | Success (r2, state2) -> Success ((r1, r2), state2)
          | Failure _ as f -> f end
        | Failure _ as f-> f >.

  let gen_left_parser :
    ('a parser_code) -> ('b parser_code) -> ('a parser_code) =
    fun pcx pcy state -> .<
      let res1 = .~(pcx state) in
      match res1 with
      | Success (r1, state1) -> begin
        let res2 = .~(pcy .<state1>.) in
        match res2 with
        | Success (_, state2) -> Success (r1, state2)
        | Failure _ as f -> f end
      | Failure _ as f -> f >.

  let gen_right_parser :
    ('a parser_code) -> ('b parser_code) -> ('b parser_code) =
    fun pcx pcy state -> .<
      let res1 = .~(pcx state) in
        match res1 with
        | Success (_, state1) -> .~(pcy .<state1>.)
        | Failure state1 -> Failure state1 >.

  let gen_either_parser :
    ('a parser_code) -> ('b parser_code) -> ([`Left of 'a | `Right of 'b] parser_code) =
    fun pcx pcy state -> .<
      let res1 = .~(pcx state) in
      match res1 with
      | Success (v1, state1) -> Success (`Left v1, state1)
      | Failure _ ->
        let res2 = .~(pcy state) in
        match res2 with
        | Success (v2, state2) -> Success (`Right v2, state2)
        | Failure _ as f -> f >.

  (** GADT, weeeeeeeeeee! *)
  let rec gen_parser : type a. a parser_generator = fun c state ->
    match c with
    | Lit e -> gen_lit_parser e state
    | Seq (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_seq_parser c1 c2 state
    | Left (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_left_parser c1 c2 state
    | Right (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_right_parser c1 c2 state
    | Either (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_either_parser c1 c2 state
    | _ -> failwith "TODO"

  let grammar_z = lit 'a'

  let grammar_lit = lit 'a' <~> lit 'b'

  let grammar_a = ((lit 'a') <~> (lit 'b')) <|> (lit 'c')

  let grammar_b = ((lit 'a') <|> (lit 'b')) <~> (lit 'c')

  let grammar_e = (lit 'a') >> (lit 'b') << (lit 'c')

end

let c1 () =
  let open Nparser.Char_parser in
  let parser_code = CFParser.gen_parser CFParser.grammar_e in
  let s = init_state_from_string "ab" in
  parser_code .<s>.

(*
  let state = init_state_from_string "abc" in
  match (Runcode.run parser_code) state with
  | Success (c, s) -> Printf.sprintf "Success %c" c
  | Failure s -> Printf.sprintf "Fail at row %d, col %d, index %d" s.row s.col s.index
*)

let () =
  (* add runtime search path because we are using ``ocamlbuild'' *)
  Runcode.(add_search_path "./_build");
  PP.pp_code (c1 ())
