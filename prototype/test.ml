open Print_code
open Format

let pp_code code = print_code Format.std_formatter code
let f x = x * 2
let c = .<f 10>.
let () = pp_code c
