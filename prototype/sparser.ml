open Print_code
open Format

module PP = struct
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

module CFParser = struct

  open Nparser.Char_parser

  type 'a parser = state -> 'a parse_result

  type 'a parser_generator =
    'a cgrammar -> ('a parser) code

  let gen_lit_parser : char -> (char parser) code =
    fun e -> .<
      fun state ->
        let ix = state.index in
        let f = Failure state in
        if ix < state.length then
          let e1 = state.input.[ix] in
          if e1 = e then
            if e = '\n' then
              let row1 = state.row + 1 and col1 = 0 in
              Success (e1, {state with row = row1; col = col1; index = ix + 1})
            else Success (e1, {state with col = (state).col + 1; index = ix + 1})
          else f
        else f
    >.

  let gen_seq_parser :
    ('a parser) code -> ('b parser) code -> (('a * 'b) parser) code =
    fun a b -> .<
      fun state ->
        let res1 = .~a state in
        match res1 with
        | Success (r1, state1) -> begin
            let res2 = .~b state1 in
            match res2 with
            | Success (r2, state2) -> Success ((r1, r2), state2)
            | Failure s -> Failure s end
        | Failure state2 -> Failure state2 >.

  let gen_left_parser :
    ('a parser) code -> ('b parser) code -> ('a parser) code =
    fun a b -> .<
      fun state ->
        let res1 = .~a state in
        match res1 with
        | Success (r1, state1) -> begin
            let res2 = .~b state1 in
            match res2 with
            | Success (_, state2) -> Success (r1, state2)
            | Failure s -> Failure s end
        | Failure state2 -> Failure state2 >.

  let gen_right_parser :
    ('a parser) code -> ('b parser) code -> ('b parser) code =
    fun a b -> .<
      fun state ->
        let res1 = .~a state in
        match res1 with
        | Success (_, state1) -> .~b state1
        | Failure state1 -> Failure state1 >.

  (*
  let gen_either_parser :
    ('a parser) code -> ('a parser) code -> ('a parser) code =
    fun a b -> .<
      fun state ->
        let res1 = .~a state in
        match res1 with
        | Success (v1, state1) -> Success (`Left v1, state1)
        | Failure _ -> .~b state >.
  *)

  (** GADT, weeeeeeeeeee! *)
  let rec gen_parser : type a.a parser_generator = fun c ->
    match c with
    | Lit e -> gen_lit_parser e
    | Seq (g1, g2) ->
        let c1 = gen_parser g1 and c2 = gen_parser g2 in
        gen_seq_parser c1 c2
    | Left (g1, g2) ->
        let c1 = gen_parser g1 and c2 = gen_parser g2 in
        gen_left_parser c1 c2
    | Right (g1, g2) ->
        let c1 = gen_parser g1 and c2 = gen_parser g2 in
        gen_right_parser c1 c2
    | Either (g1, g2) -> failwith "TODO"
    | _ -> failwith "TODO"

  let grammar_lit = lit 'a' <~> lit 'b'

  let grammar_a = ((lit 'a') <~> (lit 'b')) <|> (lit 'c')

  let grammar_b = ((lit 'a') <|> (lit 'b')) <~> (lit 'c')

  let grammar_e = (lit 'a') >> (lit 'b') << (lit 'c')

end

let c1 () =
  let open Nparser.Char_parser in
  let parser_code = CFParser.gen_parser CFParser.grammar_e in
  PP.pp_code parser_code

(*
  let state = init_state_from_string "abc" in
  match (Runcode.run parser_code) state with
  | Success (c, s) -> Printf.sprintf "Success %c" c
  | Failure s -> Printf.sprintf "Fail at row %d, col %d, index %d" s.row s.col s.index
*)

let () =
  (* add runtime search path because we are using ``ocamlbuild'' *)
  Runcode.(add_search_path "./_build");
  c1 ()
