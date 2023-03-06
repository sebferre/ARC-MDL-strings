
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


(* mdl *)
                   
type dl = Mdl.bits

let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]

(* regex *)
  
let regexp_match_full (re : Str.regexp) (s : string) : bool = (* TODO: optimize *)
  Str.string_match re s 0
  && Str.match_end () = String.length s

