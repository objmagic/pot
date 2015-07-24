(* Take basic parser combinator and do optimization *)

open Print_code
open Format
open Gengenlet

open Sparser

module PP = struct
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

(* state is dynamic, grammar is static *)

module CFParser = struct

  open BasicFParser

  let gen_lit_parser : char -> char parser_code =
    fun c -> T (fun state -> .<
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
    >.)

  let gen_apply pc state =
    match pc with
    | T tpc -> tpc state
    | NT ntpc -> .<.~ntpc .~state>.

  let gen_seq_parser :
    ('a parser_code) -> ('b parser_code) -> ('a * 'b) parser_code =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(gen_apply pcx state) in
      match res1 with
      | Success (r1, state1) -> begin
        let res2 = .~(gen_apply pcy .<state1>.) in
        match res2 with
        | Success (r2, state2) -> Success ((r1, r2), state2)
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)

  let gen_left_parser :
    ('a parser_code) -> ('b parser_code) -> ('a parser_code) =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(gen_apply pcx state) in
      match res1 with
      | Success (r1, state1) -> begin
        let res2 = .~(gen_apply pcy .<state1>.) in
        match res2 with
        | Success (_, state2) -> Success (r1, state2)
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)
  
  let gen_right_parser :
    ('a parser_code) -> ('b parser_code) -> ('b parser_code) =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(gen_apply pcx state) in
      match res1 with
      | Success (_, state1) -> begin
        let res2 = .~(gen_apply pcy .<state1>.) in
        match res2 with
        | Success (_, _) as s -> s
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)

  let gen_either_parser : ('a parser_code) list -> ('a parser_code) =
    fun l ->
      let combine : ('a parser_code) -> ('a parser_code) -> ('a parser_code) =
        fun pcx pcy -> T (fun state -> .<
          let res1 = .~(gen_apply pcx state) in
          match res1 with
          | Success (_, _) as s -> s
          | Failure _ -> .~(gen_apply pcy state)>.) in
      match l with
      | x :: res ->
        (List.fold_left combine x res)
      | _ -> failwith "Invalid Either grammar"
  
  let gen_rep_parser : ('a parser_code) -> ('a list parser_code) = fun p ->
    T (
      fun state -> .<
        let rec rep_parser state acc =
          let res = .~(gen_apply p .<state>.) in
          match res with
          | Success (r1, s1) -> rep_parser s1 (r1 :: acc)
          | Failure s1 -> Success (List.rev acc, s1) in
        rep_parser .~state []>.)

  let gen_repsep_parser : ('a parser_code) -> ('b parser_code) -> ('a list parser_code) =
    fun ap bp -> T (
      fun state -> .<
        let rec repsep_parser state acc =
          let res = .~(gen_apply ap .<state>.) in
          match res with
          | Success (r1, s1) -> begin
            let res2 = .~(gen_apply bp .<s1>.) in
            match res2 with
            | Success (_, s2) -> repsep_parser s2 (r1 :: acc)
            | Failure s3 -> Success (List.rev (r1 :: acc), s1) end
          | Failure s1 -> Success (List.rev acc, s1)
        in repsep_parser .~state []>.)

  let gen_trans_parser : ('a -> 'b) -> ('a parser_code) -> ('b parser_code) =
    fun trans ap -> T (fun state -> .<
      let res = .~(gen_apply ap state) in
      match res with
      | Success (r, s) -> Success ((trans r), s)
      | Failure _ as f -> f>.)

  let gen_tw_parser : (char code -> bool code) -> string parser_code = fun pred ->
    T (fun state -> .<
      let buf = Buffer.create 10 in
      let len = (.~state).length in
      let rec tw_parser s =
        let i = s.index in
        if i < len then
          let c = s.input.[i] in
          if .~(pred .<c>.) then begin
            Buffer.add_char buf c;
            if c = '\n' then
              tw_parser {s with row = s.row + 1; col = 0; index = i + 1}
            else
              tw_parser {s with col = s.col + 1; index = i + 1} end
          else Buffer.contents buf, s
        else Buffer.contents buf, s in
      let str, state_final = tw_parser .~state in
      Success (str, state_final)>.)

  let rec gen_parser : type a. a cgrammar -> a parser_code = fun c ->
    match c with
    | Lit e -> gen_lit_parser e
    | Either gl -> gen_either_parser (List.map gen_parser gl)
    | Seq (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_seq_parser c1 c2
    | Left (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_left_parser c1 c2
    | Right (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_right_parser c1 c2
    | Rep g -> gen_rep_parser (gen_parser g)
    | Repsep (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      gen_repsep_parser c1 c2
    | Trans (f, g) ->
      let c1 = gen_parser g in
      gen_trans_parser f c1
    | TakeWhile pred -> gen_tw_parser pred
    | FNT lp -> assert false
    | _ -> assert false

end


(*
module Test_expansion = struct
  open BasicFParser

  let pp_state s =
    let s = String.sub s.input s.index (s.length - s.index) in
    let s = if String.length s = 0 then "Empty" else "Left: " ^ s in
    Printf.printf "State: %s\n" s

  let test_I () =
    let parser_code = CFParser.gen_parser (FTIParser.t2_parser) in
    PP.pp_code .<fun s -> .~(parser_code .<s>.)>.


  open FTIParser
  
  let pp_t2 t =
    let rec bfs t =
      match t with
      | A next -> "A (" ^ (bfs next) ^ ")"
      | C c -> "C (" ^ (Char.escaped c) in
    print_endline (bfs t)


  let test () =
    let p2, _ =
      let t2pc = ref (fun _ -> assert false) in
      let t3pc = ref (fun _ -> assert false) in
      let _ = t2pc := fun s ->
        let res1 = !t3pc s in
        match res1 with
        | Success (arr, s) -> Success ((A arr), s)
        | Failure _ ->
            let res2 =
              let ix = s.index in
              let f = Failure s in
              if ix < s.length then
                if 'c' = s.input.[ix] then Success ('c', {s with row = s.row + 1; index = ix + 1}) else f
              else f in
            match res2 with
            | Success (c, s) -> Success ((C c), s)
            | Failure _ as f -> f in
      let _ = t3pc := fun s ->
        let ix = s.index in
        let f = Failure s in
        if ix < s.length then
          if '[' = s.input.[ix] then
            let s' = {s with row = s.row + 1; index = ix + 1} in
            let res2 = !t2pc s' in
            match res2 with
            | Success (res3, s'') ->
              let ix' = s''.index in
              let f' = Failure s'' in
              if ix' < s''.length then
                if ']' = (s'').input.[ix'] then
                  Success (res3, {s'' with row = s.row + 1; index = ix' + 1})
                else f'
              else f'
            | Failure _ as f -> f
          else f
        else f in
      (!t2pc, !t3pc) in
    let res = p2 (init_state_from_string "[c]") in
    match res with
    | Success (v, state) -> pp_t2 v; pp_state state
    | _ -> print_endline "fail"


    (*
    let map = CFParser.preprocess_grammar FJsonParser.json_parser in
    let iterf : type a. a CodeMap.key -> a parser_code ref -> unit =
      fun _ cr -> PP.pp_code .<fun s -> !cr .<s>.>. in
    CodeMap.iter map {CodeMap.f=iterf};
    Printf.printf "size : %d\n" (CodeMap.size map)
       *)
end
*)

module Test = struct

  open Sparser.BasicFParser

  let state s = init_state_from_string s
 
  let dump_code (g, _, _, _) () =
    let parser_code = CFParser.gen_parser g in
    match parser_code with
    | T pc -> PP.pp_code .<fun s -> .~(pc .<s>.)>.
    | NT npc -> PP.pp_code npc

  let run_code (g, ss, p1, p2) () =
    let show_res res =
      match res with
      | Success (r, s) -> p1 r; p2 s
      | Failure s -> p2 s in
    let parser_code = CFParser.gen_parser g in
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
  (*
  let repsepg = Repsep ((lit 'c'), (lit ',')), [
      state "c,c";
      state "c";
      state "c,c,c,c,c,";
      state "";
      state ","], pp_char_list, pp_state

  let strg = ((lit '"') >> (TakeWhile (fun c -> .<.~c <> '"'>.)) << (lit '"')),
             [state "\"abc\"";state "\"a\"";state "\"\""; state "\""; state "\"a"],
             print_endline, pp_state
  *)

end

let () = Runcode.(add_search_path "./_build")

open Test

let () =
  run_code repg ()
