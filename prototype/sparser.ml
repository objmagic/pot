open Print_code
open Format
open Gengenlet

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

  (* let-rec insertion helper by Jeremy *)
  let letrec : 'a 'b 'c.(('a -> 'b) code -> (('a -> 'b) code -> unit code) -> 'c) -> 'c =
    fun k ->
      let r = genlet (.< ref (fun _ -> assert false) >.) in
      k .< ! .~r >. (fun e -> genlet (.<.~r := .~e >.))

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
    ('a parser_code) list -> ('a parser_code) =
    fun l s ->
      let combine : ('a parser_code) -> ('a parser_code) -> ('a parser_code) =
        fun pcx pcy state -> .<
          let res1 = .~(pcx state) in
          match res1 with
          | Success (_, _) as s -> s
          | Failure _ -> .~(pcy state)>. in
      match l with
      | x :: res ->
        (List.fold_left combine x res) s
      | _ -> failwith "Invalid grammar"

  (*
      let rec rep_parser state res =
        let res = .~(parser state) in
        match res with
        | Success of s1, r1 as s -> rep_parser .<s1>. res
        | Failure s -> .<Success s, res>.

     *)
  let gen_rep_parser : ('a parser_code) -> ('a list parser_code) = fun p s ->
    .<let rec rep_parser state acc =
        let res = .~(p .<state>.) in
        match res with
        | Success (r1, s1) -> rep_parser s1 (r1 :: acc)
        | Failure s1 -> Success (acc, s1) in rep_parser .~s []>.
  
  (** GADT, weeeeeeeeeee! *)
  let rec gen_parser : type a. a parser_generator = fun c state ->
    match c with
    | Lit e -> gen_lit_parser e state
    | Either gl -> gen_either_parser (List.map gen_parser gl) state
    (*
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
    | Rep g -> gen_rep_parser (gen_parser g) state *)
    | _ -> failwith "TODO"

  let str_parser =
    Trans (
      (fun cl -> List.map (fun c -> String.make 1 c) cl |> String.concat ""),
      Left ((Right ((lit '"'), (TakeWhile (fun c -> c <> '"')))), (Lit '"')))

  let rec json_parser = Either ([
      Trans ((fun o -> Obj o), obj_parser);
      Trans ((fun a -> Arr a), arr_parser);
      Trans ((fun s -> StringLit s), str_parser)])
  and obj_parser =
    Left ((Right ((Lit '{'), (Repsep (member_parser, (Lit ','))))), (Lit '}'))
  and arr_parser =
    Left ((Right ((Lit '['), (Repsep (json_parser, (Lit ','))))), (Lit ']'))
  and member_parser =
    Seq (str_parser, Right ((Lit ':'), json_parser))

  let rec t2_parser = Either ([
      Trans ((fun arr -> A arr), arr_parser);
      Trans ((fun str -> S str), str_parser)])
  and arr_parser =
    Left ((Right ((Lit '['), t2_parser)), (Lit ']'))

  let either_test = Either ([
      lit 'a'; lit 'b'; lit 'c'; lit 'd'; lit 'e'; lit 'f'])

end

let c1 () =
  let open Nparser.Char_parser in
  let parser_code = CFParser.gen_parser CFParser.either_test in
  let s = init_state_from_string "c" in
  parser_code .<s>.

let see_code () =
  (* add runtime search path because we are using ``ocamlbuild'' *)
  Runcode.(add_search_path "./_build");
  PP.pp_code (c1 ())

let run () =
  Runcode.(add_search_path "./_build");
  match Runcode.(!. (c1 ())) with
  | Nparser.Char_parser.Failure _ -> failwith "Parsing failed"
  | Nparser.Char_parser.Success (c, s) ->
    Printf.printf "%c\n" c

let () = see_code ()
