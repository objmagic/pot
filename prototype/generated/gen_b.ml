.<
let res1_9 =
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
  | Nparser.Char_parser.Success (_,_) as s_5 -> s_5
  | Nparser.Char_parser.Failure _ ->
      let ix_6 = (* CSP s *).Nparser.Char_parser.index in
      let f_7 = Nparser.Char_parser.Failure (* CSP s *) in
      if ix_6 < (* CSP s *).Nparser.Char_parser.length
      then
        let e1_8 = ((* CSP s *).Nparser.Char_parser.input).[ix_6] in
        (if e1_8 = 'b'
         then
           Nparser.Char_parser.Success
             (e1_8,
               {
                 (* CSP s *) with
                 Nparser.Char_parser.index = (ix_6 + 1);
                 Nparser.Char_parser.col =
                   ((* CSP s *).Nparser.Char_parser.col + 1)
               })
         else f_7)
      else f_7 in
match res1_9 with
| Nparser.Char_parser.Success (r1_10,state1_11) ->
    let res2_16 =
      let ix_13 = state1_11.Nparser.Char_parser.index in
      let f_14 = Nparser.Char_parser.Failure state1_11 in
      if ix_13 < state1_11.Nparser.Char_parser.length
      then
        let e1_15 = (state1_11.Nparser.Char_parser.input).[ix_13] in
        (if e1_15 = 'c'
         then
           Nparser.Char_parser.Success
             (e1_15,
               {
                 state1_11 with
                 Nparser.Char_parser.index = (ix_13 + 1);
                 Nparser.Char_parser.col =
                   (state1_11.Nparser.Char_parser.col + 1)
               })
         else f_14)
      else f_14 in
    (match res2_16 with
     | Nparser.Char_parser.Success (r2_17,state2_18) ->
         Nparser.Char_parser.Success ((r1_10, r2_17), state2_18)
     | Nparser.Char_parser.Failure _ as f_19 -> f_19)
| Nparser.Char_parser.Failure _ as f_12 -> f_12>.
