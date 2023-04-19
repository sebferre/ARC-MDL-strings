
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

let chars_letters =
  ['A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H'; 'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P'; 'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X'; 'Y'; 'Z';
   'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n'; 'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v'; 'w'; 'x'; 'y'; 'z']
let chars_digits =
  ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']
let chars_decimal =
  '.' :: chars_digits
let chars_word =
  '_' :: chars_digits @ chars_letters
let chars_operators =
  ['#'; '$'; '%'; '&'; '*'; '+'; '-'; '/'; '<'; '='; '>'; '@'; '\\'; '^'; '|'; '~']
let chars_content =
  chars_word @ chars_operators
let chars_spaces =
  [' '; '\n'; '\t'; '\r']
let chars_puncts =
  ['!'; ','; '.'; ':'; ';'; '?']
let chars_quotes =
  ['"'; '\''; '`']
let chars_brackets =
  ['('; ')'; '['; ']'; '{'; '}']
let chars_separators =
  chars_spaces @ chars_puncts @ chars_quotes @ chars_brackets
let chars =
  chars_content @ chars_separators
  
let regexp_match_full (re : Str.regexp) (s : string) : bool = (* TODO: optimize *)
  Str.string_match re s 0
  && Str.match_end () = String.length s

(* printing *)
  
let xp_string ~(html : bool) (print : Xprint.t) (s : string) =
  if html
  then (
    print#string "<pre class=\"inline\">";
    print#string s;
    print#string "</pre>")
  else
    print#string ("\"" ^ String.escaped s ^ "\"")
let pp_string = Xprint.to_stdout (xp_string ~html:false)

let xp_brackets ~html (print : Xprint.t) (xp : Xprint.t -> unit) : unit =
  if html then print#string "<div class=\"model-brackets\">" else print#string "(";
  xp print;
  if html then print#string "</div>" else print#string ")"

  
(* combinatorics *)

let rec sum_conv (lf : (int -> float) list) (n : int) : float =
  (* distributes [n] over functions in [lf], multiply results, sums over all distribs *)
  (* TODO: memoize recursive calls? *)
  match lf with
  | [] -> assert false
  | [f1] -> f1 n
  | f1::lf1 ->
     Common.fold_for
       (fun n1 res ->
         let card1 = f1 n1 in
         let n' = n - n1 in
         if card1 > 0.
         then res +. card1 *. sum_conv lf1 n'
         else res)
       0 n 0.

(* mdl *)
                   
type dl = Mdl.bits

let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]

type 't asd = ASD of ('t -> (string * int * 't list) list) (* constructor name, size, and args *)
(* there must be a single AST at most with size=0 *)
(* typical constructor size is 1 *)
    
let make_dl_ast (ASD asd : 't asd)
    : 't (* AST type *) -> int (* AST size *) -> dl (* dl of ASTs of that size *) =
  let tab : ('t * int, float) Hashtbl.t = Hashtbl.create 1013 in
  let rec aux (t : 't) (n : int) : float =
    match Hashtbl.find_opt tab (t,n) with
    | Some card -> card
    | None ->
       let prods = asd t in
       let card =
         List.fold_left (* sum over productions *)
           (fun res (_name, size, args) ->
             let card_prod =
               if args = [] then (* leaf node *)
                 if n = size then 1. else 0.
               else (* internal node *)
                 if n >= size
                 then sum_conv (List.map aux args) (n-size)
                 else 0. in
             res +. card_prod)
           0. prods
       in
       Hashtbl.add tab (t,n) card;
       card
  in
  fun t n ->
  let card =
    if n = 0 then 1. (* only one empty ast *)
    else aux t n in
  assert (card > 0.);
  Mdl.log2 card
                    
                    
(* for cumulated bell-shape probability with center value at median *)
let sigmoid ~median x = 1. /. (1. +. exp (median -. x))

(* DL of value x, known to be in range, given bell-shaped prob distrib of (x - median) *)
let dl_bell_range ~(median : float) ~(range : int * int option) (x : int) : dl =
  let a, b_opt = range in
  assert (a <= x);
  b_opt
  |> Option.iter (fun b ->
         assert (a <= b);
         assert (x <= b));
  let prob =
    let sig_a = sigmoid ~median (float a -. 0.5) in
    let sig_b =
      match b_opt with
      | Some b -> sigmoid ~median (float b +. 0.5)
      | None -> 1. in
    (sigmoid ~median (float x +. 0.5) -. sigmoid ~median (float x -. 0.5))
    /. (sig_b -. sig_a) in
  -. (Mdl.log2 prob)

let dl_chars ?init_occs (chars : char list) (s : string) : dl =
  (* using prequential code, assuming string length known *)
  let pc = new Mdl.Code.prequential ?init_occs chars () in
  String.iter
    (fun c -> ignore (pc#code c))
    s;
  pc#cumulated_dl

let dl_string_ascii (s : string) : dl = dl_chars chars s
