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
  | `Bool of bool
  | `Date of date
  | `Time of time
  | `List of value list
  | `Fun of (value -> value result) ]

let xp_value (print : Xprint.t) : value -> unit = function
  | `Null -> print#string "null"
  | `String s -> xp_string print s
  | `Int i -> print#int i
  | `Bool b -> print#string (if b then "true" else "false")
  | `Date date -> raise TODO
  | `Time time -> raise TODO
  | `List _ -> raise TODO
  | `Fun _ -> print#string "_fun_" 
      
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
      | `Upper_letters (* string filter, only uppercase letters *)
      | `Lower_letters (* string filter, only lowercase letters *)
      | `Letters (* string filter, only letters *)
      | `Digits (* string filter, only digits *)
      | `Prefix of int (* 1: first char, -1: all chars except the last *)
      | `Suffix of int (* 1: last char, -1: all chars except the first *)
      (* TODO: make Prefix and Suffix binary, to use ints from data ? *)
      | `Length
      | `Concat
      | `Day
      | `Month
      | `Year 
      | `Hours 
      | `Minutes 
      | `Seconds
      | `Equals of value ]
    let nb_unary = 17

    type binary =
      [ `Append
      | `Map_list ]
    let nb_binary = 2

    let string_filter (p : char -> bool) (s : string) : string =
      s
      |> String.to_seq
      |> Seq.filter p
      |> String.of_seq
    let uppercase (s : string) : string = String.uppercase_ascii s
    let lowercase (s : string) : string = String.lowercase_ascii s
    let upper_letters (s : string) : string =
      s |> string_filter (fun c -> c >= 'A' && c <= 'Z')
    let lower_letters (s : string) : string =
      s |> string_filter (fun c -> c >= 'a' && c <= 'z')
    let letters (s : string) : string =
      s |> string_filter (fun c -> c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z')
    let digits (s : string) : string =
      s |> string_filter (fun c -> c >= '0' && c <= '9')
    let prefix ~(pos : int) (s : string) : string result =
      let n = String.length s in
      if s = "" then Result.Error (Invalid_argument "function prefix")
      else if pos > 0 && pos <= n then Result.Ok (String.sub s 0 pos)
      else if pos < 0 && pos > -n then Result.Ok (String.sub s 0 (n + pos))
      else Result.Error (Invalid_argument "function prefix, wrong pos")
    let suffix ~(pos : int) (s : string) : string result =
      let n = String.length s in
      if s = "" then Result.Error (Invalid_argument "function suffix")
      else if pos > 0 && pos < n then Result.Ok (String.sub s pos (n-pos))
      else if pos < 0 && pos >= -n then Result.Ok (String.sub s (n+pos) (-pos))
      else Result.Error (Invalid_argument "function suffix, wrong pos")
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
      | `Upper_letters -> print#string "upper_letters"
      | `Lower_letters -> print#string "lower_letters"
      | `Letters -> print#string "letters"
      | `Digits -> print#string "digits"
      | `Prefix pos -> print#string "prefix["; print#int pos; print#string "]"
      | `Suffix pos -> print#string "suffix["; print#int pos; print#string "]"
      | `Length -> print#string "length"
      | `Concat -> print#string "concat"
      | `Day -> print#string "day"
      | `Month -> print#string "month"
      | `Year -> print#string "year"
      | `Hours -> print#string "hours"
      | `Minutes -> print#string "minutes"
      | `Seconds -> print#string "seconds"
      | `Equals v -> print#string "equals["; xp_value print v; print#string "]"

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

let expr_asd : [`Expr] asd =
  let expr_prods =
    ["Ref", 0, []; (* the null-size expression *)
     "Unary", 1, [`Expr];
     "Binary", 1, [`Expr; `Expr]]
      (* Arg and Fun left for future work *)
  in
  ASD (function `Expr -> expr_prods)
  
let rec xp_expr (xp_var : 'var Xprint.xp) (print : Xprint.t) : 'var expr -> unit = function
  | `Ref p -> xp_var print p
  | `Unary (`Equals v0,e1) -> xp_expr xp_var print e1; print#string " = "; xp_value print v0
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
  | `Upper_letters, `String s1 ->
     let res = Funct.upper_letters s1 in
     if res = "" then Result.Error (Failure "No upper letters")
     else Result.Ok (`String res)
  | `Lower_letters, `String s1 ->
     let res = Funct.lower_letters s1 in
     if res = "" then Result.Error (Failure "No lower letters")
     else Result.Ok (`String res)
  | `Letters, `String s1 ->
     let res = Funct.letters s1 in
     if res = "" then Result.Error (Failure "No letters")
     else Result.Ok (`String res)
  | `Digits, `String s1 ->
     let res = Funct.digits s1 in
     if res = "" then Result.Error (Failure "No digits")
     else Result.Ok (`String res)
  | `Prefix pos, `String s1 ->
     let| res = Funct.prefix ~pos s1 in
     Result.Ok (`String res)
  | `Suffix pos, `String s1 ->
     let| res = Funct.suffix ~pos s1 in
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
  | `Equals v0, _ -> (* TODO: consider moving below `Null, `List, and `Fun *)
     Result.Ok (`Bool (v1 = v0))
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

       
(* description lengths *)
       
let dl_funct_unary (f : Funct.unary) : dl =
  Mdl.Code.uniform Funct.nb_unary
  +. (match f with
      | `Uppercase | `Lowercase -> 0.
      | `Upper_letters | `Lower_letters | `Letters | `Digits -> 0.
      | `Prefix pos | `Suffix pos ->
         Mdl.Code.universal_int_plus (abs pos) +. 1. (* pos sign *)
      | `Length | `Concat -> 0.
      | `Day | `Month | `Year | `Hours | `Minutes | `Seconds -> 0.
      | `Equals v ->
         (match v with
          | `String s ->
             dl_bell_range ~median:3. ~range:(0,None) (String.length s)
             +. dl_string_ascii s
          | `Int i ->
             let n = if i >= 0 then i * 2 else (-2 * i - 1) in
             Mdl.Code.universal_int_star n
          | _ -> assert false)
     )

let dl_funct_binary (f : Funct.binary) : dl =
  Mdl.Code.uniform Funct.nb_binary

let dl_expr_ast = make_dl_ast expr_asd

let rec dl_expr_aux (dl_var : 'var -> dl) : 'var expr -> int (* AST size *) * float = function
  | `Ref p -> 0, dl_var p
  | `Unary (f,e1) ->
     let n1, dl1 = dl_expr_aux dl_var e1 in
     1 + n1,
     dl_funct_unary f +. dl1
  | `Binary (f,e1,e2) ->
     let n1, dl1 = dl_expr_aux dl_var e1 in
     let n2, dl2 = dl_expr_aux dl_var e2 in
     1 + n1 + n2,
     dl_funct_binary f +. dl1 +. dl2
  | `Arg -> raise TODO
  | `Fun _ -> raise TODO

let dl_expr (dl_var : 'var -> dl) (e : 'var expr) : dl =
  let n, dl_leaves = dl_expr_aux dl_var e in
  Mdl.Code.universal_int_star n (* encoding expr AST size *)
  +. dl_expr_ast `Expr n (* encoding AST structure *)
  +. dl_leaves (* encoding AST leaves *)

                     
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

    let bind_set (v : V.t) (es : 'var exprset) (index : 'var t) : 'var t =
      M.update v
        (function
         | None -> Some es
         | Some exprs -> Some (List.rev_append es exprs))
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
  let index = (* level 0 *)
    List.fold_left
      (fun res (x,v) -> Index.bind v (`Ref x) res)
      Index.empty bindings in
  let index = (* level 1 *)
    add_layer_unary index
      (function
       | `String _ ->
          let res = [`Length; `Upper_letters; `Lower_letters; `Letters; `Digits] in
          let res =
            List.fold_left
              (fun res pos ->
                `Prefix pos :: `Prefix (-pos) ::
                  `Suffix pos :: `Suffix (-pos) :: res)
              res [1;2;3] in
          res
       | `List (`String _ :: _) ->
          let res = [`Length; `Concat; `Upper_letters; `Lower_letters; `Letters; `Digits] in
          let res =
            List.fold_left
              (fun res pos ->
                `Prefix pos :: `Prefix (-pos) ::
                  `Suffix pos :: `Suffix (-pos) :: res)
              res [1;2;3] in
          res
       | `Date _ | `List (`Date _ :: _) ->
          [`Year; `Month; `Day]
       | `Time _ | `List (`Time _ :: _) ->
          [`Hours; `Minutes; `Seconds]
       | _ -> []) in
  let index = (* level 2 *)
    add_layer_unary index
      (function
       | `String _ | `List (`String _ :: _) -> [`Uppercase; `Lowercase]
       | _ -> []) in
  (*let index = (* level 3 *) (* too many combinations *)
    add_layer_binary index
      (function
       | `String _, `String _ -> [`Append]
       | _ -> []) in*)
  Index.fold
    (fun v exprs res ->
      let res = (* adding equalities on specific values *)
        match v with
        | `Bool _ -> res (* not equalities on Booleans *)
        | `String _ | `Int _ ->
           Index.bind (`Bool true) (`Unary (`Equals v, exprs)) res
        | _ -> res in (* TODO: consider equality on other types *)
      let res = (* making some values available as strings, extend Model.apply/Expr accordingly *)
        match v with
        | `Int i -> Index.bind_set (`String (string_of_int i)) exprs res
        | _ -> res in
      res)
    index index
