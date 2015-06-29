open Print_code
open Format
open Normal_parser

module PP = struct
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

module CFParser = struct

  open Normal_parser

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
              Success (e1, {state with row = row1; col = col1})
            else Success (e1, {state with col = (state).col + 1})
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

  (** GADT, weeeeeeeeeee! *)
  let rec gen_parser : type a. a parser_generator = fun c ->
    match c with
    | Lit e -> gen_lit_parser e
    | Seq (g1, g2) ->
        let c1 = gen_parser g1
        and c2 = gen_parser g2 in
        gen_seq_parser c1 c2
    | _-> failwith "TODO"

  let grammar_lit = lit 'a'

  let grammar_a = (lit 'a') <~> (lit 'b')

  let grammar_b = (lit 'a') <|> (lit 'b') <~> (lit 'c')

  let grammar_c = (lit 'a') <~> (lit 'b') <|> (lit 'c')

  let grammar_d = grammar_a <~> (grammar_b <|> grammar_c) <~> grammar_a

end

let c1 =
  let open Normal_parser in
  let parser_code = CFParser.gen_parser CFParser.grammar_lit in
  let state = init_state_from_string "a" in
  .< .~parser_code state >.

let () =
  PP.pp_code c1
