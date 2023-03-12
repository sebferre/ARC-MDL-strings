
exception TODO

module TEST = (* for profiling visually, used for the JS version *)
  struct
    let prof name f =
      print_endline (name ^ "...");
      let res = f () in
      print_endline ("DONE " ^ name);
      res
  end

(* tuples *)

type 'a double = 'a * 'a
type 'a triple = 'a * 'a * 'a
  
(* list *)
  
let rec list_update (f : 'a -> 'a) (i : int) : 'a list -> 'a list = function
  | [] -> raise Not_found
  | x::l ->
     if i = 0
     then f x :: l
     else x :: list_update f (i-1) l

let rec list_partition_map (f : 'a -> ('b,'c) Result.t) (selected : 'a list) (others : 'c list) : 'b list * 'c list =
  match selected with
  | [] -> [], others
  | x::r ->
     let r1, r2 = list_partition_map f r others in
     ( match f x with
     | Result.Ok y -> y::r1, r2
     | Result.Error z -> r1, z::r2 )
    
(* result *)
        
type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

let catch (r : 'a result) (h : exn -> 'a result) : 'a result =
  match r with
  | Result.Ok _ -> r
  | Result.Error exn -> h exn
                   
let rec list_map_result (f : 'a -> ('b,'c) Result.t) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = list_map_result f lx1 in
     Result.Ok (y::ly1)

let result_list_bind_some (lx_res : ('a list,'c) Result.t) (f : 'a -> ('b list,'c) Result.t) : ('b list, 'c) Result.t =
  let rec aux = function
  | [] -> invalid_arg "Model2.bind_map_ok: empty list"
  | [x] -> f x
  | x::lx1 ->
     let open Result in
     match f x, aux lx1 with
     | Ok ly0, Ok ly1 -> Ok (List.append ly0 ly1)
     | Ok ly0, Error _ -> Ok ly0
     | Error _, Ok ly1 -> Ok ly1
     | Error e1, Error _ -> Error e1
  in
  let| lx = lx_res in
  aux lx
let ( let+|+ ) = result_list_bind_some


(* seq *)

module Seq = Stdlib.Seq (* the standard one, not Myseq *)

let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]
let ( let*? ) seq f = seq |> Myseq.filter_map f [@@inline]
let ( let*! ) seq f = seq |> Myseq.map f [@@inline]
let myseq_cons_if cond x seq =
  if cond
  then Myseq.cons x seq
  else seq
let myseq_concat_if cond seq1 seq2 =
  if cond
  then Myseq.concat [seq1; seq2]
  else seq2


(* regex *)
  
let regexp_match_full (re : Str.regexp) (s : string) : bool = (* TODO: optimize *)
  Str.string_match re s 0
  && Str.match_end () = String.length s

(* printing *)
  
let xp_string (print : Xprint.t) (s : string) =
  print#string "<pre class=\"inline\">";
  print#string s;
  print#string "</pre>"
let pp_string = Xprint.to_stdout xp_string

let xp_brackets (print : Xprint.t) (xp : Xprint.t -> unit) : unit =
  print#string "<div class=\"model-brackets\">";
  xp print;
  print#string "</div>"

  
(* combinatorics *)

let rec sum_conv (lf : (int -> float) list) (n : int) : float =
  (* distributes [n] over functions in [lf], multiply results, sums over all distribs *)
  assert (n > 0);
  match lf with
  | [] -> assert false
  | [f1] -> f1 n
  | f1::lf1 ->
     Common.fold_for
       (fun n1 res ->
         let card1 = f1 n1 in
         let n' = n - n1 in
         if card1 > 0. && n' > 0
         then res +. card1 *. sum_conv lf1 n'
         else res)
       1 n 0.

(* mdl *)
                   
type dl = Mdl.bits

let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]


type 't asd = ASD of ('t -> (string * 't list) list)
    
let make_dl_ast (ASD asd : 't asd)
    : 't (* AST type *) -> int (* AST size *) -> dl (* dl of ASTs of that size *) =
  let tab : ('t * int, float) Hashtbl.t = Hashtbl.create 1013 in
  let rec aux (t : 't) (n : int) : float =
    assert (n > 0); (* no null-sized AST *)
    match Hashtbl.find_opt tab (t,n) with
    | Some card -> card
    | None ->
       let prods = asd t in
       let card =
         List.fold_left (* sum over productions *)
           (fun res (_name, args) ->
             let card_prod =
               if args = [] then (* leaf node *)
                 if n = 1 then 1. else 0.
               else (* internal node *)
                 if n > 1
                 then sum_conv (List.map aux args) (n-1)
                 else 0. in
             res +. card_prod)
           0. prods
       in
       Hashtbl.add tab (t,n) card;
       card
  in
  fun t n ->
  let card = aux t n in
  assert (card > 0.);
  Mdl.log2 card
                    
                    
(* for cumulated bell-shape probability with center value at median *)
let sigmoid ~median x = 1. /. (1. +. exp (median -. x))

(* DL of value x, known to be in range, given bell-shaped prob distrib of (x - median) *)
let dl_bell_range ~(median : float) ~(range : int * int) (x : int) : dl =
  let a, b = range in
  assert (a <= b);
  assert (a <= x && x <= b);
  let prob =
    (sigmoid ~median (float x +. 0.5) -. sigmoid ~median (float x -. 0.5))
    /. (sigmoid ~median (float b +. 0.5) -. sigmoid ~median (float a -. 0.5)) in
  -. (Mdl.log2 prob)

