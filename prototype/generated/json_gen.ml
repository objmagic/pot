.<
let t_1 = Pervasives.ref (fun _  -> assert false) in
let t_3 = Pervasives.ref (fun _  -> assert false) in
let t_5 = Pervasives.ref (fun _  -> assert false) in
let t_43 =
  t_5 :=
    (fun s_6  ->
       let res1_30 =
         let res1_22 =
           let res1_10 =
             let ix_7 = s_6.Sparser.BasicFParser.index in
             let f_8 = Sparser.BasicFParser.Failure s_6 in
             if ix_7 < s_6.Sparser.BasicFParser.length
             then
               let e1_9 = (s_6.Sparser.BasicFParser.input).[ix_7] in
               (if e1_9 = '"'
                then
                  Sparser.BasicFParser.Success
                    (e1_9,
                      {
                        s_6 with
                        Sparser.BasicFParser.index = (ix_7 + 1);
                        Sparser.BasicFParser.col =
                          (s_6.Sparser.BasicFParser.col + 1)
                      })
                else f_8)
             else f_8 in
           match res1_10 with
           | Sparser.BasicFParser.Success (_,state1_11) ->
               let res2_20 =
                 let buf_12 = Buffer.create 10 in
                 let len_13 = state1_11.Sparser.BasicFParser.length in
                 let rec tw_parser_14 s_15 =
                   let i_16 = s_15.Sparser.BasicFParser.index in
                   if i_16 < len_13
                   then
                     let c_17 = (s_15.Sparser.BasicFParser.input).[i_16] in
                     (if c_17 <> '"'
                      then
                        (Buffer.add_char buf_12 c_17;
                         if c_17 = '\n'
                         then
                           tw_parser_14
                             {
                               s_15 with
                               Sparser.BasicFParser.index = (i_16 + 1);
                               Sparser.BasicFParser.row =
                                 (s_15.Sparser.BasicFParser.row + 1);
                               Sparser.BasicFParser.col = 0
                             }
                         else
                           tw_parser_14
                             {
                               s_15 with
                               Sparser.BasicFParser.index = (i_16 + 1);
                               Sparser.BasicFParser.col =
                                 (s_15.Sparser.BasicFParser.col + 1)
                             })
                      else ((Buffer.contents buf_12), s_15))
                   else ((Buffer.contents buf_12), s_15) in
                 let (str_18,state_final_19) = tw_parser_14 state1_11 in
                 Sparser.BasicFParser.Success (str_18, state_final_19) in
               (match res2_20 with
                | Sparser.BasicFParser.Success (_,_) as s_21 -> s_21
                | Sparser.BasicFParser.Failure _ ->
                    Sparser.BasicFParser.Failure s_6)
           | Sparser.BasicFParser.Failure _ ->
               Sparser.BasicFParser.Failure s_6 in
         match res1_22 with
         | Sparser.BasicFParser.Success (r1_23,state1_24) ->
             let res2_28 =
               let ix_25 = state1_24.Sparser.BasicFParser.index in
               let f_26 = Sparser.BasicFParser.Failure state1_24 in
               if ix_25 < state1_24.Sparser.BasicFParser.length
               then
                 let e1_27 = (state1_24.Sparser.BasicFParser.input).[ix_25] in
                 (if e1_27 = '"'
                  then
                    Sparser.BasicFParser.Success
                      (e1_27,
                        {
                          state1_24 with
                          Sparser.BasicFParser.index = (ix_25 + 1);
                          Sparser.BasicFParser.col =
                            (state1_24.Sparser.BasicFParser.col + 1)
                        })
                  else f_26)
               else f_26 in
             (match res2_28 with
              | Sparser.BasicFParser.Success (_,state2_29) ->
                  Sparser.BasicFParser.Success (r1_23, state2_29)
              | Sparser.BasicFParser.Failure _ ->
                  Sparser.BasicFParser.Failure s_6)
         | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_6 in
       match res1_30 with
       | Sparser.BasicFParser.Success (r1_31,state1_32) ->
           let res2_40 =
             let res1_36 =
               let ix_33 = state1_32.Sparser.BasicFParser.index in
               let f_34 = Sparser.BasicFParser.Failure state1_32 in
               if ix_33 < state1_32.Sparser.BasicFParser.length
               then
                 let e1_35 = (state1_32.Sparser.BasicFParser.input).[ix_33] in
                 (if e1_35 = ':'
                  then
                    Sparser.BasicFParser.Success
                      (e1_35,
                        {
                          state1_32 with
                          Sparser.BasicFParser.index = (ix_33 + 1);
                          Sparser.BasicFParser.col =
                            (state1_32.Sparser.BasicFParser.col + 1)
                        })
                  else f_34)
               else f_34 in
             match res1_36 with
             | Sparser.BasicFParser.Success (_,state1_37) ->
                 let res2_38 = (! t_1) state1_37 in
                 (match res2_38 with
                  | Sparser.BasicFParser.Success (_,_) as s_39 -> s_39
                  | Sparser.BasicFParser.Failure _ ->
                      Sparser.BasicFParser.Failure state1_32)
             | Sparser.BasicFParser.Failure _ ->
                 Sparser.BasicFParser.Failure state1_32 in
           (match res2_40 with
            | Sparser.BasicFParser.Success (r2_41,state2_42) ->
                Sparser.BasicFParser.Success ((r1_31, r2_41), state2_42)
            | Sparser.BasicFParser.Failure _ ->
                Sparser.BasicFParser.Failure s_6)
       | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_6) in
let t_72 =
  t_3 :=
    (fun s_4  ->
       let res1_64 =
         let res1_47 =
           let ix_44 = s_4.Sparser.BasicFParser.index in
           let f_45 = Sparser.BasicFParser.Failure s_4 in
           if ix_44 < s_4.Sparser.BasicFParser.length
           then
             let e1_46 = (s_4.Sparser.BasicFParser.input).[ix_44] in
             (if e1_46 = '{'
              then
                Sparser.BasicFParser.Success
                  (e1_46,
                    {
                      s_4 with
                      Sparser.BasicFParser.index = (ix_44 + 1);
                      Sparser.BasicFParser.col =
                        (s_4.Sparser.BasicFParser.col + 1)
                    })
              else f_45)
           else f_45 in
         match res1_47 with
         | Sparser.BasicFParser.Success (_,state1_48) ->
             let res2_62 =
               let rec repsep_parser_49 state_50 acc_51 =
                 let res_52 = (! t_5) state_50 in
                 match res_52 with
                 | Sparser.BasicFParser.Success (r1_53,s1_54) ->
                     let res2_59 =
                       let ix_56 = s1_54.Sparser.BasicFParser.index in
                       let f_57 = Sparser.BasicFParser.Failure s1_54 in
                       if ix_56 < s1_54.Sparser.BasicFParser.length
                       then
                         let e1_58 =
                           (s1_54.Sparser.BasicFParser.input).[ix_56] in
                         (if e1_58 = ','
                          then
                            Sparser.BasicFParser.Success
                              (e1_58,
                                {
                                  s1_54 with
                                  Sparser.BasicFParser.index = (ix_56 + 1);
                                  Sparser.BasicFParser.col =
                                    (s1_54.Sparser.BasicFParser.col + 1)
                                })
                          else f_57)
                       else f_57 in
                     (match res2_59 with
                      | Sparser.BasicFParser.Success (_,s2_60) ->
                          repsep_parser_49 s2_60 (r1_53 :: acc_51)
                      | Sparser.BasicFParser.Failure s3_61 ->
                          Sparser.BasicFParser.Success
                            ((List.rev (r1_53 :: acc_51)), s1_54))
                 | Sparser.BasicFParser.Failure s1_55 ->
                     Sparser.BasicFParser.Success ((List.rev acc_51), s1_55) in
               repsep_parser_49 state1_48 [] in
             (match res2_62 with
              | Sparser.BasicFParser.Success (_,_) as s_63 -> s_63
              | Sparser.BasicFParser.Failure _ ->
                  Sparser.BasicFParser.Failure s_4)
         | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_4 in
       match res1_64 with
       | Sparser.BasicFParser.Success (r1_65,state1_66) ->
           let res2_70 =
             let ix_67 = state1_66.Sparser.BasicFParser.index in
             let f_68 = Sparser.BasicFParser.Failure state1_66 in
             if ix_67 < state1_66.Sparser.BasicFParser.length
             then
               let e1_69 = (state1_66.Sparser.BasicFParser.input).[ix_67] in
               (if e1_69 = '}'
                then
                  Sparser.BasicFParser.Success
                    (e1_69,
                      {
                        state1_66 with
                        Sparser.BasicFParser.index = (ix_67 + 1);
                        Sparser.BasicFParser.col =
                          (state1_66.Sparser.BasicFParser.col + 1)
                      })
                else f_68)
             else f_68 in
           (match res2_70 with
            | Sparser.BasicFParser.Success (_,state2_71) ->
                Sparser.BasicFParser.Success (r1_65, state2_71)
            | Sparser.BasicFParser.Failure _ ->
                Sparser.BasicFParser.Failure s_4)
       | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_4) in
let t_73 = Pervasives.ref (fun _  -> assert false) in
let t_103 =
  t_73 :=
    (fun s_74  ->
       let res1_95 =
         let res1_78 =
           let ix_75 = s_74.Sparser.BasicFParser.index in
           let f_76 = Sparser.BasicFParser.Failure s_74 in
           if ix_75 < s_74.Sparser.BasicFParser.length
           then
             let e1_77 = (s_74.Sparser.BasicFParser.input).[ix_75] in
             (if e1_77 = '['
              then
                Sparser.BasicFParser.Success
                  (e1_77,
                    {
                      s_74 with
                      Sparser.BasicFParser.index = (ix_75 + 1);
                      Sparser.BasicFParser.col =
                        (s_74.Sparser.BasicFParser.col + 1)
                    })
              else f_76)
           else f_76 in
         match res1_78 with
         | Sparser.BasicFParser.Success (_,state1_79) ->
             let res2_93 =
               let rec repsep_parser_80 state_81 acc_82 =
                 let res_83 = (! t_1) state_81 in
                 match res_83 with
                 | Sparser.BasicFParser.Success (r1_84,s1_85) ->
                     let res2_90 =
                       let ix_87 = s1_85.Sparser.BasicFParser.index in
                       let f_88 = Sparser.BasicFParser.Failure s1_85 in
                       if ix_87 < s1_85.Sparser.BasicFParser.length
                       then
                         let e1_89 =
                           (s1_85.Sparser.BasicFParser.input).[ix_87] in
                         (if e1_89 = ','
                          then
                            Sparser.BasicFParser.Success
                              (e1_89,
                                {
                                  s1_85 with
                                  Sparser.BasicFParser.index = (ix_87 + 1);
                                  Sparser.BasicFParser.col =
                                    (s1_85.Sparser.BasicFParser.col + 1)
                                })
                          else f_88)
                       else f_88 in
                     (match res2_90 with
                      | Sparser.BasicFParser.Success (_,s2_91) ->
                          repsep_parser_80 s2_91 (r1_84 :: acc_82)
                      | Sparser.BasicFParser.Failure s3_92 ->
                          Sparser.BasicFParser.Success
                            ((List.rev (r1_84 :: acc_82)), s1_85))
                 | Sparser.BasicFParser.Failure s1_86 ->
                     Sparser.BasicFParser.Success ((List.rev acc_82), s1_86) in
               repsep_parser_80 state1_79 [] in
             (match res2_93 with
              | Sparser.BasicFParser.Success (_,_) as s_94 -> s_94
              | Sparser.BasicFParser.Failure _ ->
                  Sparser.BasicFParser.Failure s_74)
         | Sparser.BasicFParser.Failure _ ->
             Sparser.BasicFParser.Failure s_74 in
       match res1_95 with
       | Sparser.BasicFParser.Success (r1_96,state1_97) ->
           let res2_101 =
             let ix_98 = state1_97.Sparser.BasicFParser.index in
             let f_99 = Sparser.BasicFParser.Failure state1_97 in
             if ix_98 < state1_97.Sparser.BasicFParser.length
             then
               let e1_100 = (state1_97.Sparser.BasicFParser.input).[ix_98] in
               (if e1_100 = ']'
                then
                  Sparser.BasicFParser.Success
                    (e1_100,
                      {
                        state1_97 with
                        Sparser.BasicFParser.index = (ix_98 + 1);
                        Sparser.BasicFParser.col =
                          (state1_97.Sparser.BasicFParser.col + 1)
                      })
                else f_99)
             else f_99 in
           (match res2_101 with
            | Sparser.BasicFParser.Success (_,state2_102) ->
                Sparser.BasicFParser.Success (r1_96, state2_102)
            | Sparser.BasicFParser.Failure _ ->
                Sparser.BasicFParser.Failure s_74)
       | Sparser.BasicFParser.Failure _ -> Sparser.BasicFParser.Failure s_74) in
let t_143 =
  t_1 :=
    (fun s_2  ->
       let res1_114 =
         let res1_108 =
           let res_104 = (! t_3) s_2 in
           match res_104 with
           | Sparser.BasicFParser.Success (r_105,s_106) ->
               Sparser.BasicFParser.Success
                 ((Sparser.FJsonParser.Obj r_105), s_106)
           | Sparser.BasicFParser.Failure _ as f_107 -> f_107 in
         match res1_108 with
         | Sparser.BasicFParser.Success (_,_) as s_109 -> s_109
         | Sparser.BasicFParser.Failure _ ->
             let res_110 = (! t_73) s_2 in
             (match res_110 with
              | Sparser.BasicFParser.Success (r_111,s_112) ->
                  Sparser.BasicFParser.Success
                    ((Sparser.FJsonParser.Arr r_111), s_112)
              | Sparser.BasicFParser.Failure _ as f_113 -> f_113) in
       match res1_114 with
       | Sparser.BasicFParser.Success (_,_) as s_115 -> s_115
       | Sparser.BasicFParser.Failure _ ->
           let res_139 =
             let res1_131 =
               let res1_119 =
                 let ix_116 = s_2.Sparser.BasicFParser.index in
                 let f_117 = Sparser.BasicFParser.Failure s_2 in
                 if ix_116 < s_2.Sparser.BasicFParser.length
                 then
                   let e1_118 = (s_2.Sparser.BasicFParser.input).[ix_116] in
                   (if e1_118 = '"'
                    then
                      Sparser.BasicFParser.Success
                        (e1_118,
                          {
                            s_2 with
                            Sparser.BasicFParser.index = (ix_116 + 1);
                            Sparser.BasicFParser.col =
                              (s_2.Sparser.BasicFParser.col + 1)
                          })
                    else f_117)
                 else f_117 in
               match res1_119 with
               | Sparser.BasicFParser.Success (_,state1_120) ->
                   let res2_129 =
                     let buf_121 = Buffer.create 10 in
                     let len_122 = state1_120.Sparser.BasicFParser.length in
                     let rec tw_parser_123 s_124 =
                       let i_125 = s_124.Sparser.BasicFParser.index in
                       if i_125 < len_122
                       then
                         let c_126 =
                           (s_124.Sparser.BasicFParser.input).[i_125] in
                         (if c_126 <> '"'
                          then
                            (Buffer.add_char buf_121 c_126;
                             if c_126 = '\n'
                             then
                               tw_parser_123
                                 {
                                   s_124 with
                                   Sparser.BasicFParser.index = (i_125 + 1);
                                   Sparser.BasicFParser.row =
                                     (s_124.Sparser.BasicFParser.row + 1);
                                   Sparser.BasicFParser.col = 0
                                 }
                             else
                               tw_parser_123
                                 {
                                   s_124 with
                                   Sparser.BasicFParser.index = (i_125 + 1);
                                   Sparser.BasicFParser.col =
                                     (s_124.Sparser.BasicFParser.col + 1)
                                 })
                          else ((Buffer.contents buf_121), s_124))
                       else ((Buffer.contents buf_121), s_124) in
                     let (str_127,state_final_128) = tw_parser_123 state1_120 in
                     Sparser.BasicFParser.Success (str_127, state_final_128) in
                   (match res2_129 with
                    | Sparser.BasicFParser.Success (_,_) as s_130 -> s_130
                    | Sparser.BasicFParser.Failure _ ->
                        Sparser.BasicFParser.Failure s_2)
               | Sparser.BasicFParser.Failure _ ->
                   Sparser.BasicFParser.Failure s_2 in
             match res1_131 with
             | Sparser.BasicFParser.Success (r1_132,state1_133) ->
                 let res2_137 =
                   let ix_134 = state1_133.Sparser.BasicFParser.index in
                   let f_135 = Sparser.BasicFParser.Failure state1_133 in
                   if ix_134 < state1_133.Sparser.BasicFParser.length
                   then
                     let e1_136 =
                       (state1_133.Sparser.BasicFParser.input).[ix_134] in
                     (if e1_136 = '"'
                      then
                        Sparser.BasicFParser.Success
                          (e1_136,
                            {
                              state1_133 with
                              Sparser.BasicFParser.index = (ix_134 + 1);
                              Sparser.BasicFParser.col =
                                (state1_133.Sparser.BasicFParser.col + 1)
                            })
                      else f_135)
                   else f_135 in
                 (match res2_137 with
                  | Sparser.BasicFParser.Success (_,state2_138) ->
                      Sparser.BasicFParser.Success (r1_132, state2_138)
                  | Sparser.BasicFParser.Failure _ ->
                      Sparser.BasicFParser.Failure s_2)
             | Sparser.BasicFParser.Failure _ ->
                 Sparser.BasicFParser.Failure s_2 in
           (match res_139 with
            | Sparser.BasicFParser.Success (r_140,s_141) ->
                Sparser.BasicFParser.Success
                  ((Sparser.FJsonParser.StringLit r_140), s_141)
            | Sparser.BasicFParser.Failure _ as f_142 -> f_142)) in
! t_1>.
