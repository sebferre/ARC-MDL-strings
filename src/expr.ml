(* Managing expressions for expressing computations in models *)

(* utilities *)

exception TODO

type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

let rec list_map_result (f : 'a -> ('b,'c) Result.t) (lx : 'a list) : ('b list, 'c) Result.t =
  match lx with
  | [] -> Result.Ok []
  | x::lx1 ->
     let| y = f x in
     let| ly1 = list_map_result f lx1 in
     Result.Ok (y::ly1)

                   
let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]

type dl = Mdl.bits

        
(* types *)

(*type typ = [`String | `Int | `Date | `Time | `List of typ | `Function of typ * typ ]*)

         
(* values *)
         
type date = { year : int; month : int; day : int }
type time = { hours : int; minutes : int; seconds : int }
        
type value =
  [ `String of string
  | `Int of int
  | `Date of date
  | `Time of time
  | `List of value list
  | `Function of (value -> value result) ]

let string_of_value : value -> string result = function
  | `String s -> Result.Ok s
  | _ -> Result.Error (Invalid_argument "Expr.string_of_value") (* an ill-formed expression was built *)
let int_of_value : value -> int result = function
  | `Int i -> Result.Ok i
  | _ -> Result.Error (Invalid_argument "Expr.int_of_value")
let date_of_value : value -> date result = function
  | `Date d -> Result.Ok d
  | _ -> Result.Error (Invalid_argument "Expr.date_of_value")
let time_of_value : value -> time result = function
  | `Time t -> Result.Ok t
  | _ -> Result.Error (Invalid_argument "Expr.time_of_value")
let list_of_value (elt_of_value : value -> 'a result) : value -> 'a list result = function
  | `List lv -> list_map_result elt_of_value lv
  | _ -> Result.Error (Invalid_argument "Expr.list_of_value")
let fun_of_value : value -> (value -> value result) result = function
  | `Function f -> Result.Ok f
  | _ -> Result.Error (Invalid_argument "Expr.fun_of_value")
       
(* functions *)

module Funct =
  struct
    type unary =
      [ `Uppercase
      | `Lowercase
      | `Initial
      | `Length
      | `Day
      | `Month
      | `Year 
      | `Hours 
      | `Minutes 
      | `Seconds ]

    type binary =
      [ `Concat
      | `Map_list ]

    let uppercase (s : string) : string = String.uppercase_ascii s
    let lowercase (s : string) : string = String.uppercase_ascii s
    let initial (s : string) : string result =
      if s = ""
      then Result.Error (Invalid_argument "function initial")
      else Result.Ok (String.sub s 0 1)
    let length (s : string) : int = String.length s
    let year (d : date) : int = d.year
    let month (d : date) : int = d.month
    let day (d : date) : int = d.day
    let hours (t : time) : int = t.hours
    let minutes (t : time) : int = t.minutes
    let seconds (t : time) : int = t.seconds
                                  
    let concat (s1 : string) (s2 : string) : string = s1 ^ s2
    let map_list (f : 'a -> 'b result) (l : 'a list) : 'b list result =
      list_map_result f l

    let xp_unary (print : Xprint.t) : unary -> unit = function
      | `Uppercase -> print#string "uppercase"
      | `Lowercase -> print#string "lowercase"
      | `Initial -> print#string "initial"
      | `Length -> print#string "length"
      | `Day -> print#string "day"
      | `Month -> print#string "month"
      | `Year -> print#string "year"
      | `Hours -> print#string "hours"
      | `Minutes -> print#string "minutes"
      | `Seconds -> print#string "seconds"

    let xp_binary (print : Xprint.t) : binary -> unit = function
      | `Concat -> print#string "concat"
      | `Map_list -> print#string "map_list"
      
  end
                                                
(* expressions *)
     
type 'var expr =
  [ `Ref of 'var
  | `Lambda
  | `Unary of Funct.unary * 'var expr
  | `Binary of Funct.binary * 'var expr * 'var expr ]

let rec xp_expr (xp_var : 'var Xprint.xp) (print : Xprint.t) : 'var expr -> unit = function
  | `Ref p -> print#string "!"; xp_var print p
  | `Lambda -> print#string "_"
  | `Unary (f,e1) -> Funct.xp_unary print f; print#string "("; xp_expr xp_var print e1; print#string ")"
  | `Binary (`Concat, e1,e2) -> xp_expr xp_var print e1; print#string " + "; xp_expr xp_var print e2
  | `Binary (f,e1,e2) -> Funct.xp_binary print f; print#string "("; xp_expr xp_var print e1; print#string ","; xp_expr xp_var print e2; print#string ")"
  
let rec eval (lookup : 'var -> value result) : 'var expr -> value result = function
  | `Ref x -> lookup x
  | `Lambda -> raise TODO
  | `Unary (f,e1) ->
     let| v1 = eval lookup e1 in
     eval_unary f v1
  | `Binary (f,e1,e2) ->
     let| v1 = eval lookup e1 in
     let| v2 = eval lookup e2 in
     eval_binary f v1 v2
and eval_unary f v1 =
  match f with
  | `Uppercase ->
     let| s1 = string_of_value v1 in
     let res = Funct.uppercase s1 in
     Result.Ok (`String res)
  | `Lowercase ->
     let| s1 = string_of_value v1 in
     let res = Funct.lowercase s1 in
     Result.Ok (`String res)
  | `Initial ->
     let| s1 = string_of_value v1 in
     let| res = Funct.initial s1 in
     Result.Ok (`String res)
  | `Length ->
     let| s1 = string_of_value v1 in
     let res = Funct.length s1 in
     Result.Ok (`Int res)
  | `Day ->
     let| d1 = date_of_value v1 in
     let res = Funct.day d1 in
     Result.Ok (`Int res)
  | `Month ->
     let| d1 = date_of_value v1 in
     let res = Funct.month d1 in
     Result.Ok (`Int res)
  | `Year ->
     let| d1 = date_of_value v1 in
     let res = Funct.year d1 in
     Result.Ok (`Int res)
  | `Hours ->
     let| t1 = time_of_value v1 in
     let res = Funct.hours t1 in
     Result.Ok (`Int res)
  | `Minutes ->
     let| t1 = time_of_value v1 in
     let res = Funct.minutes t1 in
     Result.Ok (`Int res)
  | `Seconds ->
     let| t1 = time_of_value v1 in
     let res = Funct.seconds t1 in
     Result.Ok (`Int res)
and eval_binary f v1 v2 =
  match f with
  | `Concat ->
     let| s1 = string_of_value v1 in
     let| s2 = string_of_value v2 in
     let res = Funct.concat s1 s2 in
     Result.Ok (`String res)
  | `Map_list -> raise TODO
(*     let| f1 = fun_of_value v1 in
     let| l2 = list_of_value v2 in
     let res = Funct.map_list f1 l2 in
     res *)

let dl_funct = Mdl.Code.uniform 12 (* 12 functions *)
     
let rec dl_expr (dl_var : 'var -> dl) (e : 'var expr) : dl =
  let nb_funct = dl_expr_stats e in
  Mdl.Code.universal_int_star nb_funct
  +. dl_expr_aux dl_var nb_funct e
and dl_expr_aux dl_var nb_funct = function
  | `Ref p ->
     assert (nb_funct = 0); (* must be a ref *)
     dl_var p
  | `Lambda -> raise TODO
  | `Unary (f,e1) ->
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Binary (f,e1,e2) ->
     let nb1 = dl_expr_stats e1 in
     let nb2 = dl_expr_stats e2 in
     assert (nb1 + nb2 + 1 = nb_funct);
     dl_funct
     +. Mdl.Code.uniform (nb1 + 1) (* choosing split of functions between e1 and e2 *)
     +. dl_expr_aux dl_var nb1 e1
     +. dl_expr_aux dl_var nb2 e2
and dl_expr_stats : 'var expr -> int = function (* counting function applications *)
  | `Ref _ -> 0
  | `Lambda -> 0
  | `Unary (f,e1) -> 1 + dl_expr_stats e1
  | `Binary (f,e1,e2) -> 1 + dl_expr_stats e1 + dl_expr_stats e2

                     
(* expression sets : idea taken from FlashMeta *)
    
type 'var exprset = 'var expritem list
and 'var expritem =
  [ `Ref of 'var
  | `Lambda
  | `Unary of Funct.unary * 'var exprset
  | `Binary of Funct.binary * 'var exprset * 'var exprset ]

let rec exprset_to_seq (es : 'var exprset) : 'var expr Myseq.t =
  let* item = Myseq.from_list es in
  expritem_to_seq item
and expritem_to_seq : 'var expritem -> 'var expr Myseq.t = function
  | `Ref x -> Myseq.return (`Ref x)
  | `Lambda -> Myseq.return (`Lambda)
  | `Unary (f,es1) ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Unary (f,e1))
  | `Binary (f,es1,es2) ->
     let* e1 = exprset_to_seq es1 in
     let* e2 = exprset_to_seq es2 in
     Myseq.return (`Binary (f,e1,e2))
  
let rec exprset_inter (es1 : 'var exprset) (es2 : 'var exprset) : 'var exprset =
  List.fold_left
    (fun res item1 ->
      List.fold_left
        (fun res item2 ->
          match expritem_inter item1 item2 with
          | None -> res
          | Some item -> item::res)
        res es2)
    [] es1
and expritem_inter (item1 : 'var expritem) (item2 : 'var expritem) : 'var expritem option =
  match item1, item2 with
  | `Ref x1, `Ref x2 when x1 = x2 -> Some (`Ref x1)
  | `Unary (f1,e1), `Unary (f2,e2) when f1 = f2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Unary (f1,e)))
  | `Binary (f1,e11,e12), `Binary (f2,e21,e22) when f1 = f2 ->
     (match exprset_inter e11 e21, exprset_inter e12 e22  with
      | [], _ | _, [] -> None
      | e1, e2 -> Some (`Binary (f1,e1,e2)))
  | _ -> None

let rec exprset_inter_list (esl1 : 'var exprset list) (esl2 : 'var exprset list) : 'var exprset list =
  List.fold_left
    (fun res es1 ->
      List.fold_left
        (fun res es2 ->
          match exprset_inter es1 es2 with
          | [] -> res
          | es -> es::res)
        res esl2)
    [] esl1


(* indexes : idea inspired from FlashMeta *)

module MakeIndex (V : Map.OrderedType) =
  struct
    module M = Map.Make(V)
                       
    type 'var t = 'var exprset M.t

    let empty = M.empty
                
    let bind (v : V.t) (item : 'var expritem) (index : 'var t) : 'var t =
      M.update v
        (function
         | None -> Some [item]
         | Some exprs -> Some (item :: exprs))
        index

    let find_opt = M.find_opt

    let fold = M.fold
  end

  
module StringIndex = MakeIndex(struct type t = string let compare = Stdlib.compare end)

module IntIndex = MakeIndex(struct type t = int let compare = Stdlib.compare end)
           
module DateIndex = MakeIndex(struct type t = date let compare = Stdlib.compare end)

module TimeIndex = MakeIndex(struct type t = time let compare = Stdlib.compare end)

(* TODO: lists and functions, need for polymorphism *)

type 'var index =
  { by_string : 'var StringIndex.t;
    by_int : 'var IntIndex.t }

let index_lookup (v : value) (index : 'var index) : 'var exprset =
  match v with
  | `String s ->
     (match StringIndex.find_opt s index.by_string with
      | None -> []
      | Some exprs -> exprs)
  | `Int i ->
     (match IntIndex.find_opt i index.by_int with
      | None -> []
      | Some exprs -> exprs)
  
let make_index (bindings : ('var * value) list) : 'var index =
  let index_string, index_int =
    List.fold_left
      (fun (idx_s,idx_i) (x,v) ->
        match v with
        | `String s -> StringIndex.bind s (`Ref x) idx_s, idx_i
        | `Int i -> idx_s, IntIndex.bind i (`Ref x) idx_i
        | _ -> raise TODO)
      (StringIndex.empty, IntIndex.empty) bindings in
  let index_int =
    StringIndex.fold
      (fun s exprs res ->
        IntIndex.bind (Funct.length s) (`Unary (`Length, exprs)) res)
      index_string IntIndex.empty in
  let index_string =
    StringIndex.fold
      (fun s exprs res ->
        match Funct.initial s with
        | Result.Ok s' -> StringIndex.bind s' (`Unary (`Initial, exprs)) res
        | Result.Error _ -> res)
      index_string index_string in
  let index_string =
    StringIndex.fold
      (fun s exprs res ->
        let res = StringIndex.bind (Funct.uppercase s) (`Unary (`Uppercase, exprs)) res in
        let res = StringIndex.bind (Funct.lowercase s) (`Unary (`Lowercase, exprs)) res in
        res)
      index_string index_string in
  let index_string =
    StringIndex.fold
      (fun s1 exprs1 res ->
        StringIndex.fold
          (fun s2 exprs2 res ->
            res |> StringIndex.bind (s1 ^ s2) (`Binary (`Concat, exprs1, exprs2)))
          index_string res)
      index_string index_string in
  { by_string = index_string;
    by_int = index_int }
