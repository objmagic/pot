.<
let res1_24 =
  let res1_19 =
    let res1_14 =
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
      | Nparser.Char_parser.Success (_,_) as s_10 -> s_10
      | Nparser.Char_parser.Failure _ ->
          let ix_11 = (* CSP s *).Nparser.Char_parser.index in
          let f_12 = Nparser.Char_parser.Failure (* CSP s *) in
          if ix_11 < (* CSP s *).Nparser.Char_parser.length
          then
            let e1_13 = ((* CSP s *).Nparser.Char_parser.input).[ix_11] in
            (if e1_13 = 'c'
             then
               Nparser.Char_parser.Success
                 (e1_13,
                   {
                     (* CSP s *) with
                     Nparser.Char_parser.index = (ix_11 + 1);
                     Nparser.Char_parser.col =
                       ((* CSP s *).Nparser.Char_parser.col + 1)
                   })
             else f_12)
          else f_12 in
    match res1_14 with
    | Nparser.Char_parser.Success (_,_) as s_15 -> s_15
    | Nparser.Char_parser.Failure _ ->
        let ix_16 = (* CSP s *).Nparser.Char_parser.index in
        let f_17 = Nparser.Char_parser.Failure (* CSP s *) in
        if ix_16 < (* CSP s *).Nparser.Char_parser.length
        then
          let e1_18 = ((* CSP s *).Nparser.Char_parser.input).[ix_16] in
          (if e1_18 = 'd'
           then
             Nparser.Char_parser.Success
               (e1_18,
                 {
                   (* CSP s *) with
                   Nparser.Char_parser.index = (ix_16 + 1);
                   Nparser.Char_parser.col =
                     ((* CSP s *).Nparser.Char_parser.col + 1)
                 })
           else f_17)
        else f_17 in
  match res1_19 with
  | Nparser.Char_parser.Success (_,_) as s_20 -> s_20
  | Nparser.Char_parser.Failure _ ->
      let ix_21 = (* CSP s *).Nparser.Char_parser.index in
      let f_22 = Nparser.Char_parser.Failure (* CSP s *) in
      if ix_21 < (* CSP s *).Nparser.Char_parser.length
      then
        let e1_23 = ((* CSP s *).Nparser.Char_parser.input).[ix_21] in
        (if e1_23 = 'e'
         then
           Nparser.Char_parser.Success
             (e1_23,
               {
                 (* CSP s *) with
                 Nparser.Char_parser.index = (ix_21 + 1);
                 Nparser.Char_parser.col =
                   ((* CSP s *).Nparser.Char_parser.col + 1)
               })
         else f_22)
      else f_22 in
match res1_24 with
| Nparser.Char_parser.Success (_,_) as s_25 -> s_25
| Nparser.Char_parser.Failure _ ->
    let ix_26 = (* CSP s *).Nparser.Char_parser.index in
    let f_27 = Nparser.Char_parser.Failure (* CSP s *) in
    if ix_26 < (* CSP s *).Nparser.Char_parser.length
    then
      let e1_28 = ((* CSP s *).Nparser.Char_parser.input).[ix_26] in
      (if e1_28 = 'f'
       then
         Nparser.Char_parser.Success
           (e1_28,
             {
               (* CSP s *) with
               Nparser.Char_parser.index = (ix_26 + 1);
               Nparser.Char_parser.col =
                 ((* CSP s *).Nparser.Char_parser.col + 1)
             })
       else f_27)
    else f_27>.
