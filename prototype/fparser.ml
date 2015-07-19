(* Take basic parser combinator and do optimization *)

open Print_code
open Format
open Gengenlet

module PP = struct
  let pp_code code = print_code Format.std_formatter code
  let pp_closed_code code = print_closed_code Format.std_formatter code
  let format_code closed_code = format_code Format.std_formatter closed_code
end

(* Ideally, we want to have something like ... *)

(** A parser module user writes, e.g Sparser.Test_I_parser *)
module type UserCustomParser = sig end

module type SP = sig

  include Nparser.Parser (** basic parser definition  *)
  include UserCustomParser

end
   
module Fparser (Slowparser: SP) = struct
end


(*
open Nparser.Char_parser

(* state is dynamic, grammar is static *)
type 'a parser_code = state code -> 'a parse_result code

type 'a parser_generator =
  'a cgrammar -> 'a parser_code
*)

(*
module CFParser = struct

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

  let gen_rep_parser : ('a parser_code) -> ('a list parser_code) = fun p s ->
    .<let rec rep_parser state acc =
        let res = .~(p .<state>.) in
        match res with
        | Success (r1, s1) -> rep_parser s1 (r1 :: acc)
        | Failure s1 -> Success (acc, s1) in rep_parser .~s []>.
  
  let rec gen_parser : type a. a parser_generator = fun c state ->
    match c with
    | Lit e -> gen_lit_parser e state
    | Either gl -> gen_either_parser (List.map gen_parser gl) state
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
    | Rep g -> gen_rep_parser (gen_parser g) state 
    | _ -> failwith "TODO"
*)

(*
  include Nonce

  let str_parser =
    (fun cl -> List.map (fun c -> String.make 1 c) cl |> String.concat "") <*>
    ((lit '"') >> (TakeWhile (fun c -> c <> '"')) << (lit '"'))

  let rec t2_parser = NT (nonce (), lazy (
      either [(fun arr -> A arr) <*> arr_parser;
              ((fun c -> C c) <*> lit 'c')]))
  and arr_parser = NT (nonce (), lazy (
      (lit '[') >> t2_parser << (lit ']')))

  let either_test = either ([
      lit 'a'; lit 'b'; lit 'c'; lit 'd'; lit 'e'; lit 'f'])

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
*)

(*
  let gen () =
    let t2pc = ref (fun _ -> assert false) in
    let t3pc = ref (fun _ -> assert false) in
    let anonpc = ref (fun _ -> assert false) in
    let _ = t2pc := fun s ->
      let res1 = !t3pc s in
      match res1 with
      | Success (arr, s) -> Success ((A arr), s)
      | Failure _ ->
          let res2 = !anonpc s in
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
    let _ = anonpc := fun s ->
      let ix = s.index in
      let f = Failure s in
      if ix < s.length then
        if 'c' = s.input.[ix] then Success ('c', {s with row = s.row + 1; index = ix + 1}) else f
      else f in
    (!t2pc, !t3pc, !anonpc) *)

(* generated parser for json_parser should be

   let json_parser = ref (fun _ -> assert false) in
   let obj_parser = ref (fun _ -> assert false) in
   let arr_parser = ref (fun _ -> assert false) in
   let memeber_paresr = ref (fun _ -> assert false) in
   let str_parser = ref (fun _ -> assert false) in
   let _ = json_parser := fun s ->
     let r1 =
       match !obj_parser s with
       | Success (res, state) -> Obj res
       | Failure _ ->
           match !arr_parser s with
           | Success (res, state ) -> Arr res
           | Failure _ ->
               match !str_parser s with
               | Success (str, state) -> StringLit str
               | Failure _ as f -> f
     in r1 in
   let _ = obj_paresr := fun s ->
     let ix = .. char
     ... 
     
*)

  (* Avoid let rec

     let rec json_parser = fun s ->
       match obj_parser s with
      | Success ...
      | Failure ... ->

*)

let () = Runcode.(add_search_path "./_build")
