(* Some basic parsers. They are slow. These parser modules should be
   put into functor (fparser.ml) and then we'll have an optimized parser *)

open Codemap

type _ nttype = ..

type 'a typeable = {
  constructor : 'a nttype;
  eq : 'b. 'b nttype -> ('a, 'b) Eq.t option
}

module type NTType = sig
  type t
  type _ nttype += T : t nttype
  val typeable : t typeable
end

module MakeNTType (S: sig type t end) : NTType with type t = S.t = struct
  type t = S.t
  type _ nttype += T : t nttype
  let typeable = {
    constructor = T;
    eq =
      let f : type b. b nttype -> (t, b) Eq.t option = fun t ->
        match t with
        | T -> Some Eq.Refl
        | _ -> None
      in f
  }
end

let f (type a) (x : a) : (module NTType with type t = a) =
  (module MakeNTType(struct type t = a end))

type s = A and t = B

let g (type a) (type b) (x : a) (y : b) =
  let m = f x in
  let n = f y in
  let module M = (val m : NTType with type t = a) in
  let module N = (val n : NTType with type t = b) in
  M.typeable.eq N.typeable.constructor

let h (type a) (x: a) =
  let m = f x in
  let module M = (val m : NTType with type t = a) in
  M.typeable.eq M.typeable.constructor


let test1 () =
  let x = A in
  match h x with
  | Some _ -> print_endline "true"
  | None -> print_endline "false"

let test2 () =
  let x = A and y = B in
  match g x y with
  | Some _ -> print_endline "true"
  | None -> print_endline "false"

let test3 () =
  let x = A and y = A in
  match g x y with
  | Some _ -> print_endline "true"
  | None -> print_endline "false"
 

let () =
  test1 (); (* true *)
  test2 (); (* false *)
  test3 () (* false *)
(*

type 'a typeable = {
  t : 'a nttype;
  eq : 'b. 'b nttype -> ('a, 'b) Eq.t option
}

let f a =
*)

(*
module Test_I_parser = struct
  include Nparser.BasicCharParser

  type t2 = A of t3 | C of char and t3 = t2
  
  module T = struct
      
    type _ nt_type =
      | T2 : t2 nt_type
      | T3 : t3 nt_type
  end

  include T

  type _ cgrammar += NT : 'a nt_type * 'a cgrammar Lazy.t -> 'a cgrammar

  open Codemap

  module TEq = struct
    type 'a key = 'a nt_type
    let equal : type a b. a key -> b key -> (a, b) Eq.t option =
      fun x y ->
        match x, y with
        | T2, T2 -> Some Eq.Refl
        | T3, T3 -> Some Eq.Refl
        | _ -> None
  end

  module Map = CodeMap(TEq)

  let rec t2p = NT (T2, lazy (either [
    (fun arr -> A arr) <*> t3p;
    (fun c -> C c) <*> lit 'c']))
  and t3p = NT (T3, lazy (lit '[' >> t2p << lit ']'))

end
*)
let () = Runcode.(add_search_path "./_build")
