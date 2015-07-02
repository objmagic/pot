(*  ((lit 'a') <~> (lit 'b')) <|> (lit 'c') *)

.<
let res1_15 =
  let res1_4 =
    let ix_1 = (* CSP s *).Nparser.Char_parser.index in
    let f_2 = Nparser.Char_parser.Failure (* CSP s *) in
    if ix_1 < (* CSP s *).Nparser.Char_parser.length
    then
      let e1_3 = ((* CSP s *).Nparser.Char_parser.input).[ix_1] in
      (if e1_3 = 'a'
       then
         Nparser.Char_parser.Success
           (e1_3,
             {
               (* CSP s *) with
               Nparser.Char_parser.index = (ix_1 + 1);
               Nparser.Char_parser.col =
                 ((* CSP s *).Nparser.Char_parser.col + 1)
             })
       else f_2)
    else f_2 in
  match res1_4 with
  | Nparser.Char_parser.Success (r1_5,state1_6) ->
      let res2_11 =
        let ix_8 = state1_6.Nparser.Char_parser.index in
        let f_9 = Nparser.Char_parser.Failure state1_6 in
        if ix_8 < state1_6.Nparser.Char_parser.length
        then
          let e1_10 = (state1_6.Nparser.Char_parser.input).[ix_8] in
          (if e1_10 = 'b'
           then
             Nparser.Char_parser.Success
               (e1_10,
                 {
                   state1_6 with
                   Nparser.Char_parser.index = (ix_8 + 1);
                   Nparser.Char_parser.col =
                     (state1_6.Nparser.Char_parser.col + 1)
                 })
           else f_9)
        else f_9 in
      (match res2_11 with
       | Nparser.Char_parser.Success (r2_12,state2_13) ->
           Nparser.Char_parser.Success ((r1_5, r2_12), state2_13)
       | Nparser.Char_parser.Failure _ as f_14 -> f_14)
  | Nparser.Char_parser.Failure _ as f_7 -> f_7 in
match res1_15 with
| Nparser.Char_parser.Success (v1_16,state1_17) ->
    Nparser.Char_parser.Success ((`Left v1_16), state1_17)
| Nparser.Char_parser.Failure _ ->
    let res2_21 =
      let ix_18 = (* CSP s *).Nparser.Char_parser.index in
      let f_19 = Nparser.Char_parser.Failure (* CSP s *) in
      if ix_18 < (* CSP s *).Nparser.Char_parser.length
      then
        let e1_20 = ((* CSP s *).Nparser.Char_parser.input).[ix_18] in
        (if e1_20 = 'c'
         then
           Nparser.Char_parser.Success
             (e1_20,
               {
                 (* CSP s *) with
                 Nparser.Char_parser.index = (ix_18 + 1);
                 Nparser.Char_parser.col =
                   ((* CSP s *).Nparser.Char_parser.col + 1)
               })
         else f_19)
      else f_19 in
    (match res2_21 with
     | Nparser.Char_parser.Success (v2_22,state2_23) ->
         Nparser.Char_parser.Success ((`Right v2_22), state2_23)
     | Nparser.Char_parser.Failure _ as f_24 -> f_24)>.
