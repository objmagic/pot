(* Take basic parser combinator and do optimization *)
open Gengenlet
open Sparser

(* state is dynamic, grammar is static *)


module GenParser = struct

  open BasicFParser

  let map = ref (CodeMap.empty)

  let reset_map () = map := CodeMap.empty

  let apply pc state =
    match pc with
    | T tpc -> tpc state
    | NT ntpc -> .<.~ntpc .~state>.

  let lit_parser : char -> char parser_code =
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

  let seq_parser :
    ('a parser_code) -> ('b parser_code) -> ('a * 'b) parser_code =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(apply pcx state) in
      match res1 with
      | Success (r1, state1) -> begin
        let res2 = .~(apply pcy .<state1>.) in
        match res2 with
        | Success (r2, state2) -> Success ((r1, r2), state2)
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)

  let left_parser :
    ('a parser_code) -> ('b parser_code) -> ('a parser_code) =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(apply pcx state) in
      match res1 with
      | Success (r1, state1) -> begin
        let res2 = .~(apply pcy .<state1>.) in
        match res2 with
        | Success (_, state2) -> Success (r1, state2)
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)
  
  let right_parser :
    ('a parser_code) -> ('b parser_code) -> ('b parser_code) =
    fun pcx pcy -> T (fun state -> .<
      let res1 = .~(apply pcx state) in
      match res1 with
      | Success (_, state1) -> begin
        let res2 = .~(apply pcy .<state1>.) in
        match res2 with
        | Success (_, _) as s -> s
        | Failure _ -> Failure (.~state) end
      | Failure _ -> Failure (.~state) >.)

  let either_parser : ('a parser_code) list -> ('a parser_code) =
    fun l ->
      let combine : ('a parser_code) -> ('a parser_code) -> ('a parser_code) =
        fun pcx pcy -> T (fun state -> .<
          let res1 = .~(apply pcx state) in
          match res1 with
          | Success (_, _) as s -> s
          | Failure _ -> .~(apply pcy state)>.) in
      match l with
      | x :: res -> List.fold_left combine x res
      | _ -> failwith "Invalid Either grammar"
  
  let rep_parser : ('a parser_code) -> ('a list parser_code) = fun p ->
    T (
      fun state -> .<
        let rec rep_parser state acc =
          let res = .~(apply p .<state>.) in
          match res with
          | Success (r1, s1) -> rep_parser s1 (r1 :: acc)
          | Failure s1 -> Success (List.rev acc, s1) in
        rep_parser .~state []>.)

  let repsep_parser : ('a parser_code) -> ('b parser_code) -> ('a list parser_code) =
    fun ap bp -> T (
      fun state -> .<
        let rec repsep_parser state acc =
          let res = .~(apply ap .<state>.) in
          match res with
          | Success (r1, s1) -> begin
            let res2 = .~(apply bp .<s1>.) in
            match res2 with
            | Success (_, s2) -> repsep_parser s2 (r1 :: acc)
            | Failure s3 -> Success (List.rev (r1 :: acc), s1) end
          | Failure s1 -> Success (List.rev acc, s1)
        in repsep_parser .~state []>.)

  let trans_parser : ('a code -> 'b code) -> ('a parser_code) -> ('b parser_code) =
    fun trans ap -> T (fun state -> .<
      let res = .~(apply ap state) in
      match res with
      | Success (r, s) -> Success (.~(trans .<r>.), s)
      | Failure _ as f -> f>.)

  let tw_parser : (char code -> bool code) -> string parser_code = fun pred ->
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

  let letrec : 'a 'b 'c.(('a -> 'b) code -> (('a -> 'b) code -> unit code) -> 'c) -> 'c =
    fun k ->
    let r = genlet (.< ref (fun _ -> assert false) >.) in
    k .< ! .~r >. (fun e -> genlet (.<.~r := .~e >.))

  let rec gen_parser : type a. a cgrammar -> a parser_code = fun c ->
    match c with
    | Exact e -> lit_parser e
    | Either gl -> either_parser (List.map gen_parser gl)
    | Seq (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      seq_parser c1 c2
    | Left (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      left_parser c1 c2
    | Right (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      right_parser c1 c2
    | Rep g -> rep_parser (gen_parser g)
    | Repsep (g1, g2) ->
      let c1 = gen_parser g1 and c2 = gen_parser g2 in
      repsep_parser c1 c2
    | Trans (f, g) ->
      let c1 = gen_parser g in
      trans_parser f c1
    | TakeWhile pred -> tw_parser pred
    | FNT lp -> begin
      let k, g = Lazy.force lp in
      match CodeMap.find !map k with
      | Some c -> NT c
      | None ->
        letrec (fun f def ->
          map := CodeMap.add !map k f;
          let _ = def .<fun s -> .~(apply (gen_parser g) .<s>.)>. in
          NT f
          ) end
    | _ -> assert false

  let gen_parser : type a. a cgrammar -> a nt_parser_code = fun c ->
    let_locus @@ fun () -> .<.~(
      match gen_parser c with
      | T tpc -> .<fun s -> .~(tpc .<s>.)>.
      | NT ntpc -> ntpc)>.

end

