.<
let t_1 = Pervasives.ref (fun _  -> assert false) in
let t_3 = Pervasives.ref (fun _  -> assert false) in
let t_20 =
  t_3 :=
    (fun s_4  ->
       let res1_12 =
         let res1_8 =
           let ix_5 = s_4.Sparser.BasicFParser.index in
           let f_6 = Sparser.BasicFParser.Failure s_4 in
           if ix_5 < s_4.Sparser.BasicFParser.length
           then
             let e1_7 = (s_4.Sparser.BasicFParser.input).[ix_5] in
             (if e1_7 = '['
              then
                Sparser.BasicFParser.Success
                  (e1_7,
                    {
                      s_4 with
                      Sparser.BasicFParser.index = (ix_5 + 1);
                      Sparser.BasicFParser.col =
                        (s_4.Sparser.BasicFParser.col + 1)
                    })
              else f_6)
           else f_6 in
         match res1_8 with
         | Sparser.BasicFParser.Success (_,state1_9) ->
             let res2_10 = (! t_1) state1_9 in
             (match res2_10 with
              | Sparser.BasicFParser.Success (_,_) as s_11 -> s_11
              | Sparser.BasicFParser.Failure _ ->
                  Sparser.BasicFParser.Failure s_4)
         | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_4 in
       match res1_12 with
       | Sparser.BasicFParser.Success (r1_13,state1_14) ->
           let res2_18 =
             let ix_15 = state1_14.Sparser.BasicFParser.index in
             let f_16 = Sparser.BasicFParser.Failure state1_14 in
             if ix_15 < state1_14.Sparser.BasicFParser.length
             then
               let e1_17 = (state1_14.Sparser.BasicFParser.input).[ix_15] in
               (if e1_17 = ']'
                then
                  Sparser.BasicFParser.Success
                    (e1_17,
                      {
                        state1_14 with
                        Sparser.BasicFParser.index = (ix_15 + 1);
                        Sparser.BasicFParser.col =
                          (state1_14.Sparser.BasicFParser.col + 1)
                      })
                else f_16)
             else f_16 in
           (match res2_18 with
            | Sparser.BasicFParser.Success (_,state2_19) ->
                Sparser.BasicFParser.Success (r1_13, state2_19)
            | Sparser.BasicFParser.Failure _ ->
                Sparser.BasicFParser.Failure s_4)
       | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_4) in
let t_34 =
  t_1 :=
    (fun s_2  ->
       let res1_25 =
         let res_21 = (! t_3) s_2 in
         match res_21 with
         | Sparser.BasicFParser.Success (r_22,s_23) ->
             Sparser.BasicFParser.Success ((Sparser.FT2Parser.A r_22), s_23)
         | Sparser.BasicFParser.Failure _ as f_24 -> f_24 in
       match res1_25 with
       | Sparser.BasicFParser.Success (_,_) as s_26 -> s_26
       | Sparser.BasicFParser.Failure _ ->
           let res_30 =
             let ix_27 = s_2.Sparser.BasicFParser.index in
             let f_28 = Sparser.BasicFParser.Failure s_2 in
             if ix_27 < s_2.Sparser.BasicFParser.length
             then
               let e1_29 = (s_2.Sparser.BasicFParser.input).[ix_27] in
               (if e1_29 = 'c'
                then
                  Sparser.BasicFParser.Success
                    (e1_29,
                      {
                        s_2 with
                        Sparser.BasicFParser.index = (ix_27 + 1);
                        Sparser.BasicFParser.col =
                          (s_2.Sparser.BasicFParser.col + 1)
                      })
                else f_28)
             else f_28 in
           (match res_30 with
            | Sparser.BasicFParser.Success (r_31,s_32) ->
                Sparser.BasicFParser.Success
                  ((Sparser.FT2Parser.C r_31), s_32)
            | Sparser.BasicFParser.Failure _ as f_33 -> f_33)) in
! t_1>.
