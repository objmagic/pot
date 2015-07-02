(* (lit 'a') >> (lit 'b') << (lit 'c') *)

.<
let res1_10 =
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
  | Nparser.Char_parser.Success (_,state1_5) ->
      let ix_7 = state1_5.Nparser.Char_parser.index in
      let f_8 = Nparser.Char_parser.Failure state1_5 in
      if ix_7 < state1_5.Nparser.Char_parser.length
      then
        let e1_9 = (state1_5.Nparser.Char_parser.input).[ix_7] in
        (if e1_9 = 'b'
         then
           Nparser.Char_parser.Success
             (e1_9,
               {
                 state1_5 with
                 Nparser.Char_parser.index = (ix_7 + 1);
                 Nparser.Char_parser.col =
                   (state1_5.Nparser.Char_parser.col + 1)
               })
         else f_8)
      else f_8
  | Nparser.Char_parser.Failure state1_6 ->
      Nparser.Char_parser.Failure state1_6 in
match res1_10 with
| Nparser.Char_parser.Success (r1_11,state1_12) ->
    let res2_17 =
      let ix_14 = state1_12.Nparser.Char_parser.index in
      let f_15 = Nparser.Char_parser.Failure state1_12 in
      if ix_14 < state1_12.Nparser.Char_parser.length
      then
        let e1_16 = (state1_12.Nparser.Char_parser.input).[ix_14] in
        (if e1_16 = 'c'
         then
           Nparser.Char_parser.Success
             (e1_16,
               {
                 state1_12 with
                 Nparser.Char_parser.index = (ix_14 + 1);
                 Nparser.Char_parser.col =
                   (state1_12.Nparser.Char_parser.col + 1)
               })
         else f_15)
      else f_15 in
    (match res2_17 with
     | Nparser.Char_parser.Success (_,state2_18) ->
         Nparser.Char_parser.Success (r1_11, state2_18)
     | Nparser.Char_parser.Failure _ as f_19 -> f_19)
| Nparser.Char_parser.Failure _ as f_13 -> f_13>.
