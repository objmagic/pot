.<
let res1_4 =
  let ix_1 = (* CSP s *).Nparser.Char_parser.index in
  let f_2 = Nparser.Char_parser.Failure (* CSP s *) in
  if ix_1 < (* CSP s *).Nparser.Char_parser.length
  then
    let e1_3 = ((* CSP s *).Nparser.Char_parser.input).[ix_1] in
    (if e1_3 = 'w'
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
    let res2_18 =
      let rec rep_parser_8 state_9 acc_10 =
        let res_14 =
          let ix_11 = state_9.Nparser.Char_parser.index in
          let f_12 = Nparser.Char_parser.Failure state_9 in
          if ix_11 < state_9.Nparser.Char_parser.length
          then
            let e1_13 = (state_9.Nparser.Char_parser.input).[ix_11] in
            (if e1_13 = 'e'
             then
               Nparser.Char_parser.Success
                 (e1_13,
                   {
                     state_9 with
                     Nparser.Char_parser.index = (ix_11 + 1);
                     Nparser.Char_parser.col =
                       (state_9.Nparser.Char_parser.col + 1)
                   })
             else f_12)
          else f_12 in
        match res_14 with
        | Nparser.Char_parser.Success (r1_15,s1_16) ->
            rep_parser_8 s1_16 (r1_15 :: acc_10)
        | Nparser.Char_parser.Failure s1_17 ->
            Nparser.Char_parser.Success (acc_10, s1_17) in
      rep_parser_8 state1_6 [] in
    (match res2_18 with
     | Nparser.Char_parser.Success (r2_19,state2_20) ->
         Nparser.Char_parser.Success ((r1_5, r2_19), state2_20)
     | Nparser.Char_parser.Failure _ as f_21 -> f_21)
| Nparser.Char_parser.Failure _ as f_7 -> f_7>.
