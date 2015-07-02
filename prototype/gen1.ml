.<
fun state_23  ->
  let res1_24 =
    (fun state_13  ->
       let res1_14 =
         (fun state_1  ->
            let ix_2 = state_1.Nparser.Char_parser.index in
            let f_3 = Nparser.Char_parser.Failure state_1 in
            if ix_2 < state_1.Nparser.Char_parser.length
            then
              let e1_4 = (state_1.Nparser.Char_parser.input).[ix_2] in
              (if e1_4 = 'a'
               then
                 (if 'a' = '\n'
                  then
                    let row1_5 = state_1.Nparser.Char_parser.row + 1
                    and col1_6 = 0 in
                    Nparser.Char_parser.Success
                      (e1_4,
                        {
                          state_1 with
                          Nparser.Char_parser.index = (ix_2 + 1);
                          Nparser.Char_parser.row = row1_5;
                          Nparser.Char_parser.col = col1_6
                        })
                  else
                    Nparser.Char_parser.Success
                      (e1_4,
                        {
                          state_1 with
                          Nparser.Char_parser.index = (ix_2 + 1);
                          Nparser.Char_parser.col =
                            (state_1.Nparser.Char_parser.col + 1)
                        }))
               else f_3)
            else f_3) state_13 in
       match res1_14 with
       | Nparser.Char_parser.Success (_,state1_15) ->
           ((fun state_7  ->
               let ix_8 = state_7.Nparser.Char_parser.index in
               let f_9 = Nparser.Char_parser.Failure state_7 in
               if ix_8 < state_7.Nparser.Char_parser.length
               then
                 let e1_10 = (state_7.Nparser.Char_parser.input).[ix_8] in
                 (if e1_10 = 'b'
                  then
                    (if 'b' = '\n'
                     then
                       let row1_11 = state_7.Nparser.Char_parser.row + 1
                       and col1_12 = 0 in
                       Nparser.Char_parser.Success
                         (e1_10,
                           {
                             state_7 with
                             Nparser.Char_parser.index = (ix_8 + 1);
                             Nparser.Char_parser.row = row1_11;
                             Nparser.Char_parser.col = col1_12
                           })
                     else
                       Nparser.Char_parser.Success
                         (e1_10,
                           {
                             state_7 with
                             Nparser.Char_parser.index = (ix_8 + 1);
                             Nparser.Char_parser.col =
                               (state_7.Nparser.Char_parser.col + 1)
                           }))
                  else f_9)
               else f_9)) state1_15
       | Nparser.Char_parser.Failure state1_16 ->
           Nparser.Char_parser.Failure state1_16) state_23 in
  match res1_24 with
  | Nparser.Char_parser.Success (r1_25,state1_26) ->
      let res2_28 =
        (fun state_17  ->
           let ix_18 = state_17.Nparser.Char_parser.index in
           let f_19 = Nparser.Char_parser.Failure state_17 in
           if ix_18 < state_17.Nparser.Char_parser.length
           then
             let e1_20 = (state_17.Nparser.Char_parser.input).[ix_18] in
             (if e1_20 = 'c'
              then
                (if 'c' = '\n'
                 then
                   let row1_21 = state_17.Nparser.Char_parser.row + 1
                   and col1_22 = 0 in
                   Nparser.Char_parser.Success
                     (e1_20,
                       {
                         state_17 with
                         Nparser.Char_parser.index = (ix_18 + 1);
                         Nparser.Char_parser.row = row1_21;
                         Nparser.Char_parser.col = col1_22
                       })
                 else
                   Nparser.Char_parser.Success
                     (e1_20,
                       {
                         state_17 with
                         Nparser.Char_parser.index = (ix_18 + 1);
                         Nparser.Char_parser.col =
                           (state_17.Nparser.Char_parser.col + 1)
                       }))
              else f_19)
           else f_19) state1_26 in
      (match res2_28 with
       | Nparser.Char_parser.Success (_,state2_29) ->
           Nparser.Char_parser.Success (r1_25, state2_29)
       | Nparser.Char_parser.Failure s_30 -> Nparser.Char_parser.Failure s_30)
  | Nparser.Char_parser.Failure state2_27 ->
      Nparser.Char_parser.Failure state2_27>.
