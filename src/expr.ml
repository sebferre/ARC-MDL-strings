(* Managing expressions for expressing computations in models *)

open Utilities
        
(* types *)

(*type typ = [`String | `Int | `Date | `Time | `List of typ | `Fun of typ * typ ]*)

         
(* values *)
         
type date = { year : int; month : int; day : int }
type time = { hours : int; minutes : int; seconds : int }

type value =
  [ `Null (* undefined, missing value *)
  | `String of string
  | `Int of int
  | `Date of date
  | `Time of time
  | `List of value list
  | `Fun of (value -> value result) ]

(* extraction functions, not used much *)
(* deprecated
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
 *)
  
(* functions *)

module Funct =
  struct
    type unary =
      [ `Uppercase
      | `Lowercase
      | `Initial
      | `Length
      | `Concat
      | `Day
      | `Month
      | `Year 
      | `Hours 
      | `Minutes 
      | `Seconds ]
    let nb_unary = 11

    type binary =
      [ `Append
      | `Map_list ]
    let nb_binary = 2

    let uppercase (s : string) : string = String.uppercase_ascii s
    let lowercase (s : string) : string = String.lowercase_ascii s
    let initial (s : string) : string result =
      if s = ""
      then Result.Error (Invalid_argument "function initial")
      else Result.Ok (String.sub s 0 1)
    let length (s : string) : int = String.length s
    let concat (l : string list) : string = String.concat "" l
    let year (d : date) : int = d.year
    let month (d : date) : int = d.month
    let day (d : date) : int = d.day
    let hours (t : time) : int = t.hours
    let minutes (t : time) : int = t.minutes
    let seconds (t : time) : int = t.seconds
                                  
    let append (s1 : string) (s2 : string) : string = s1 ^ s2
    let map_list (f : 'a -> 'b result) (l : 'a list) : 'b list result =
      list_map_result f l

    let xp_unary (print : Xprint.t) : unary -> unit = function
      | `Uppercase -> print#string "uppercase"
      | `Lowercase -> print#string "lowercase"
      | `Initial -> print#string "initial"
      | `Length -> print#string "length"
      | `Concat -> print#string "concat"
      | `Day -> print#string "day"
      | `Month -> print#string "month"
      | `Year -> print#string "year"
      | `Hours -> print#string "hours"
      | `Minutes -> print#string "minutes"
      | `Seconds -> print#string "seconds"

    let xp_binary (print : Xprint.t) : binary -> unit = function
      | `Append -> print#string "append"
      | `Map_list -> print#string "map_list"
      
  end
                                                
(* expressions *)
  
type 'var expr =
  [ `Ref of 'var
  | `Unary of Funct.unary * 'var expr
  | `Binary of Funct.binary * 'var expr * 'var expr
  | `Arg (* implicit unique argument of functions *)
  | `Fun of 'var expr (* support for unary functions, to be used as arg of higher-order functions *)
  ]

let rec xp_expr (xp_var : 'var Xprint.xp) (print : Xprint.t) : 'var expr -> unit = function
  | `Ref p -> print#string "!"; xp_var print p
  | `Unary (f,e1) -> Funct.xp_unary print f; print#string "("; xp_expr xp_var print e1; print#string ")"
  | `Binary (`Append, e1,e2) -> xp_expr xp_var print e1; print#string " + "; xp_expr xp_var print e2
  | `Binary (f,e1,e2) -> Funct.xp_binary print f; print#string "("; xp_expr xp_var print e1; print#string ","; xp_expr xp_var print e2; print#string ")"
  | `Arg -> print#string "_"
  | `Fun e1 -> print#string "fun { "; xp_expr xp_var print e1; print#string " }"
  
exception Invalid_eval_unary of Funct.unary * value
exception Invalid_eval_binary of Funct.binary * value * value

let rec eval (lookup : 'var -> value result) : 'var expr -> value result = function
  | `Ref x -> lookup x
  | `Unary (f,e1) ->
     let| v1 = eval lookup e1 in
     eval_unary f v1
  | `Binary (f,e1,e2) ->
     let| v1 = eval lookup e1 in
     let| v2 = eval lookup e2 in
     eval_binary f v1 v2
  | `Arg -> Result.Ok (`Fun (fun arg -> Result.Ok arg))
  | `Fun e1 -> eval lookup e1
and eval_unary f v1 =
  match f, v1 with
  | `Uppercase, `String s1 ->
     let res = Funct.uppercase s1 in
     Result.Ok (`String res)
  | `Lowercase, `String s1 ->
     let res = Funct.lowercase s1 in
     Result.Ok (`String res)
  | `Initial, `String s1 ->
     let| res = Funct.initial s1 in
     Result.Ok (`String res)
  | `Length, `String s1 ->
     let res = Funct.length s1 in
     Result.Ok (`Int res)
  | `Concat, `List (`String _ :: _ as l1) ->
     let| ls = list_map_result (function `String s -> Result.Ok s | _ -> Result.Error (Invalid_argument "concat")) l1 in
     let res = Funct.concat ls in
     Result.Ok (`String res)
  | `Day, `Date d1 ->
     let res = Funct.day d1 in
     Result.Ok (`Int res)
  | `Month, `Date d1 ->
     let res = Funct.month d1 in
     Result.Ok (`Int res)
  | `Year, `Date d1 ->
     let res = Funct.year d1 in
     Result.Ok (`Int res)
  | `Hours, `Time t1 ->
     let res = Funct.hours t1 in
     Result.Ok (`Int res)
  | `Minutes, `Time t1 ->
     let res = Funct.minutes t1 in
     Result.Ok (`Int res)
  | `Seconds, `Time t1 ->
     let res = Funct.seconds t1 in
     Result.Ok (`Int res)
  | _, `Null ->
     Result.Ok `Null
  | _, `List l1 ->
     let| lres = list_map_result (eval_unary f) l1 in
     Result.Ok (`List lres)
  | _, `Fun fun1 ->
     Result.Ok
       (`Fun (fun arg ->
            let| v1' = fun1 arg in
            eval_unary f v1'))
  | _ -> Result.Error (Invalid_eval_unary (f,v1))
and eval_binary f v1 v2 =
  match f, v1, v2 with
  | `Append, `String s1, `String s2 ->
     let res = Funct.append s1 s2 in
     Result.Ok (`String res)
  | `Map_list, `Fun fun1, `List l2 ->     
     let| lres = Funct.map_list fun1 l2 in
     Result.Ok (`List lres)
  | _, `Null, _ ->
     Result.Ok `Null
  | _, _, `Null ->
     Result.Ok `Null
  | _, `List l1, `List l2 ->
     if List.length l1 = List.length l2
     then
       let| lres =
         list_map_result
           (fun (x1,x2) -> eval_binary f x1 x2)
           (List.combine l1 l2) in
       Result.Ok (`List lres)
     else Result.Error (Invalid_eval_binary (f,v1,v2))
  | _, `List l1, _ ->
     let| lres = list_map_result (fun x1 -> eval_binary f x1 v2) l1 in
     Result.Ok (`List lres)
  | _, _, `List l2 ->
     let| lres = list_map_result (fun x2 -> eval_binary f v1 x2) l2 in
     Result.Ok (`List lres)
  | _, `Fun fun1, _ ->
     Result.Ok
       (`Fun (fun arg ->
            let| v1' = fun1 arg in
            eval_binary f v1' v2))
  | _, _, `Fun fun2 ->
     Result.Ok
       (`Fun (fun arg ->
            let| v2' = fun2 arg in
            eval_binary f v1 v2'))
  | _ -> Result.Error (Invalid_eval_binary (f,v1,v2))

let dl_funct = Mdl.Code.uniform (Funct.nb_unary + Funct.nb_binary + 1 (* Fun *))
     
let rec dl_expr (dl_var : 'var -> dl) (e : 'var expr) : dl =
  let nb_funct, nb_arg = dl_expr_stats e in
  assert (nb_arg = 0);
  Mdl.Code.universal_int_star nb_funct
  +. dl_expr_aux dl_var nb_funct 0 e
and dl_expr_aux dl_var nb_funct nb_arg = function
  | `Ref p ->
     assert (nb_funct = 0);
     assert (nb_arg = 0);
     dl_var p
  | `Unary (f,e1) ->
     assert (nb_funct > 0);
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) nb_arg e1
  | `Binary (f,e1,e2) ->
     assert (nb_funct > 0);
     let nbf1, nba1 = dl_expr_stats e1 in
     let nbf2, nba2 = dl_expr_stats e2 in
     assert (nbf1 + nbf2 + 1 = nb_funct);
     assert (nba1 + nba2 = nb_arg);
     dl_funct
     +. Mdl.Code.uniform nb_funct (* choosing split of functions between e1 and e2 *)
     +. Mdl.Code.uniform (nb_arg + 1) (* choosing split of args between e1 and e2 *)
     +. dl_expr_aux dl_var nbf1 nba1 e1
     +. dl_expr_aux dl_var nbf2 nba2 e2
  | `Arg ->
     assert (nb_funct = 0);
     assert (nb_arg = 1);
     0.
  | `Fun e1 ->
     assert (nb_funct > 0);
     assert (nb_arg = 0);
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) 1 e1
and dl_expr_stats : 'var expr -> int * int = function (* counting function applications and abstractions, and nb of args *)
  | `Ref _ -> 0, 0
  | `Unary (f,e1) ->
     let nbf1, nba1 = dl_expr_stats e1 in
     1 + nbf1, nba1
  | `Binary (f,e1,e2) ->
     let nbf1, nba1 = dl_expr_stats e1 in
     let nbf2, nba2 = dl_expr_stats e2 in
     1 + nbf1 + nbf2, nba1 + nba2
  | `Arg -> 0, 1
  | `Fun e1 ->
     let nbf1, nba1 = dl_expr_stats e1 in
     1 + nbf1, 0 (* assuming all args in e1 bound by Fun *)

                     
(* expression sets : idea taken from FlashMeta *)
    
type 'var exprset = 'var expritem list
and 'var expritem =
  [ `Ref of 'var
  | `Unary of Funct.unary * 'var exprset
  | `Binary of Funct.binary * 'var exprset * 'var exprset
  | `Arg
  | `Fun of 'var exprset ]

let rec exprset_to_seq (es : 'var exprset) : 'var expr Myseq.t =
  let* item = Myseq.from_list es in
  expritem_to_seq item
and expritem_to_seq : 'var expritem -> 'var expr Myseq.t = function
  | `Ref x -> Myseq.return (`Ref x)
  | `Unary (f,es1) ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Unary (f,e1))
  | `Binary (f,es1,es2) ->
     let* e1 = exprset_to_seq es1 in
     let* e2 = exprset_to_seq es2 in
     Myseq.return (`Binary (f,e1,e2))
  | `Arg -> Myseq.return (`Arg)
  | `Fun es1 ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Fun e1)
  
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
  | `Arg, `Arg -> Some (`Arg)
  | `Fun e1, `Fun e2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Fun e))
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

module Index = MakeIndex(struct type t = value let compare = Stdlib.compare end)
  
type 'var index = 'var Index.t
                
let index_lookup (v : value) (index : 'var index) : 'var exprset =
  match Index.find_opt v index with
  | None -> []
  | Some exprs -> exprs
                
let make_index (bindings : ('var * value) list) : 'var index =
  let add_layer_unary index (get_functions : value -> Funct.unary list) =
    Index.fold
      (fun v1 exprs1 res ->
        List.fold_left
          (fun res f ->
            match eval_unary f v1 with
            | Result.Ok v -> Index.bind v (`Unary (f, exprs1)) res
            | Result.Error _ -> res)
          res (get_functions v1))
      index index in
  let add_layer_binary index (get_functions : value * value -> Funct.binary list) =
    Index.fold
      (fun v1 exprs1 res ->
        Index.fold
          (fun v2 exprs2 res ->
            List.fold_left
              (fun res f ->
                match eval_binary f v1 v2 with
                | Result.Ok v -> Index.bind v (`Binary (f, exprs1, exprs2)) res
                | Result.Error _ -> res)
              res (get_functions (v1,v2)))
          index res)
      index index
  in
  let index =
    List.fold_left
      (fun res (x,v) -> Index.bind v (`Ref x) res)
      Index.empty bindings in
  let index =
    add_layer_unary index
      (function
       | `String _ -> [`Length; `Initial]
       | `List (`String _ :: _) -> [`Length; `Initial; `Concat]
       | `Date _ | `List (`Date _ :: _) -> [`Year; `Month; `Day]
       | `Time _ | `List (`Time _ :: _) -> [`Hours; `Minutes; `Seconds]
       | _ -> []) in
  let index =
    add_layer_unary index
      (function
       | `String _ | `List (`String _ :: _) -> [`Uppercase; `Lowercase]
       | _ -> []) in
  let index =
    add_layer_binary index
      (function
       | `String _, `String _ -> [`Append]
       | _ -> []) in
  index
