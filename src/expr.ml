(* Managing expressions for expressing computations in models *)

(* utilities *)

type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

let ( let* ) seq f = seq |> Myseq.flat_map f [@@inline]

type dl = Mdl.bits

        
(* types *)
        
type typ = [`String | `Int]

         
(* values *)
         
type value =
  [ `String of string
  | `Int of int ]

let string_of_value : value -> string result = function
  | `String s -> Result.Ok s
  | _ -> Result.Error (Invalid_argument "Expr.string_of_value") (* an ill-formed expression was built *)
let int_of_value : value -> int result = function
  | `Int i -> Result.Ok i
  | _ -> Result.Error (Invalid_argument "Expr.int_of_value")

       
(* functions *)

module Funct =
  struct
    type 'a t =
      [ `Uppercase of 'a
      | `Lowercase of 'a
      | `Initial of 'a
      | `Length of 'a
      | `Concat of 'a * 'a ]

    let uppercase (s : string) : string = String.uppercase_ascii s
    let lowercase (s : string) : string = String.uppercase_ascii s
    let initial (s : string) : string result =
      if s = ""
      then Result.Error (Invalid_argument "function initial")
      else Result.Ok (String.sub s 0 1)
    let length (s : string) : int = String.length s
    let concat (s1 : string) (s2 : string) : string = s1 ^ s2
  end
                                                
(* expressions *)
     
type 'var expr =
  [ `Ref of 'var
  | 'var expr Funct.t ]

let rec xp_expr (xp_var : 'var Xprint.xp) (print : Xprint.t) : 'var expr -> unit = function
  | `Ref p -> print#string "!"; xp_var print p
  | `Uppercase e1 -> print#string "uppercase("; xp_expr xp_var print e1; print#string ")"
  | `Lowercase e1 -> print#string "lowercase("; xp_expr xp_var print e1; print#string ")"
  | `Initial e1 -> print#string "initial("; xp_expr xp_var print e1; print#string ")"
  | `Length e1 -> print#string "length("; xp_expr xp_var print e1; print#string ")"
  | `Concat (e1,e2) -> xp_expr xp_var print e1; print#string " + "; xp_expr xp_var print e2
  
let rec eval (lookup : 'var -> value result) : 'var expr -> value result = function
  | `Ref x -> lookup x
  | `Uppercase e1 ->
     let| v = eval lookup e1 in
     let| s = string_of_value v in
     let res = Funct.uppercase s in
     Result.Ok (`String res)
  | `Lowercase e1 ->
     let| v = eval lookup e1 in
     let| s = string_of_value v in
     let res = Funct.lowercase s in
     Result.Ok (`String res)
  | `Initial e1 ->
     let| v = eval lookup e1 in
     let| s = string_of_value v in
     let| res = Funct.initial s in
     Result.Ok (`String res)
  | `Length e1 ->
     let| v = eval lookup e1 in
     let| s = string_of_value v in
     let res = Funct.length s in
     Result.Ok (`Int res)
  | `Concat (e1,e2) ->
     let| v1 = eval lookup e1 in
     let| s1 = string_of_value v1 in
     let| v2 = eval lookup e2 in
     let| s2 = string_of_value v2 in
     let res = Funct.concat s1 s2 in
     Result.Ok (`String res)

let dl_funct = Mdl.Code.uniform 5 (* 5 functions *)
     
let rec dl_expr (dl_var : 'var -> dl) (e : 'var expr) : dl =
  let nb_funct = dl_expr_stats e in
  Mdl.Code.universal_int_star nb_funct
  +. dl_expr_aux dl_var nb_funct e
and dl_expr_aux dl_var nb_funct = function
  | `Ref p ->
     assert (nb_funct = 0); (* must be a ref *)
     dl_var p
  | `Uppercase e1 ->
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Lowercase e1 ->
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Initial e1 ->
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Length e1 -> (* not yet used *)
     dl_funct
     +. dl_expr_aux dl_var (nb_funct - 1) e1
  | `Concat (e1,e2) ->
     let nb1 = dl_expr_stats e1 in
     let nb2 = dl_expr_stats e2 in
     assert (nb1 + nb2 + 1 = nb_funct);
     dl_funct
     +. Mdl.Code.uniform (nb1 + 1) (* choosing split of functions between e1 and e2 *)
     +. dl_expr_aux dl_var nb1 e1
     +. dl_expr_aux dl_var nb2 e2
and dl_expr_stats : 'var expr -> int = function (* counting function applications *)
  | `Ref _ -> 0
  | `Uppercase e1 -> 1 + dl_expr_stats e1
  | `Lowercase e1 -> 1 + dl_expr_stats e1
  | `Initial e1 -> 1 + dl_expr_stats e1
  | `Length e1 -> 1 + dl_expr_stats e1
  | `Concat (e1,e2) -> 1 + dl_expr_stats e1 + dl_expr_stats e2

                     
(* expression sets : idea taken from FlashMeta *)
    
type 'var exprset = 'var expritem list
and 'var expritem =
  [ `Ref of 'var
  | 'var exprset Funct.t ]

let rec exprset_to_seq (es : 'var exprset) : 'var expr Myseq.t =
  let* item = Myseq.from_list es in
  expritem_to_seq item
and expritem_to_seq : 'var expritem -> 'var expr Myseq.t = function
  | `Ref x -> Myseq.return (`Ref x)
  | `Uppercase es1 ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Uppercase e1)
  | `Lowercase es1 ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Lowercase e1)
  | `Initial es1 ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Initial e1)
  | `Length es1 ->
     let* e1 = exprset_to_seq es1 in
     Myseq.return (`Length e1)
  | `Concat (es1,es2) ->
     let* e1 = exprset_to_seq es1 in
     let* e2 = exprset_to_seq es2 in
     Myseq.return (`Concat (e1,e2))
  
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
  | `Uppercase e1, `Uppercase e2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Uppercase e))
  | `Lowercase e1, `Lowercase e2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Lowercase e))
  | `Initial e1, `Initial e2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Initial e))
  | `Length e1, `Length e2 ->
     (match exprset_inter e1 e2 with
      | [] -> None
      | e -> Some (`Length e))
  | `Concat (e1,f1), `Concat (e2,f2) ->
     (match exprset_inter e1 e2, exprset_inter f1 f2 with
      | [], _ | _, [] -> None
      | e, f -> Some (`Concat (e,f)))
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
(*type 'var string_index = 'var exprset StringIndex.t
let bind_string (v : string) (item : 'var expritem) (index : 'var string_index) : 'var string_index =
  StringIndex.update v
    (function
     | None -> Some [item]
     | Some exprs -> Some (item :: exprs))
    index*)

module IntIndex = MakeIndex(struct type t = int let compare = Stdlib.compare end)
(*type 'var int_index = 'var exprset IntIndex.t                
let bind_int (v : int) (item : 'var expritem) (index : 'var int_index) : 'var int_index =
  IntIndex.update v
    (function
     | None -> Some [item]
     | Some exprs -> Some (item :: exprs))
    index*)
           
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
        | `Int i -> idx_s, IntIndex.bind i (`Ref x) idx_i)
      (StringIndex.empty, IntIndex.empty) bindings in
  let index_int =
    StringIndex.fold
      (fun s exprs res ->
        IntIndex.bind (String.length s) (`Length exprs) res)
      index_string IntIndex.empty in
  let index_string =
    StringIndex.fold
      (fun s exprs res ->
        match Funct.initial s with
        | Result.Ok s' -> StringIndex.bind s' (`Initial exprs) res
        | Result.Error _ -> res)
      index_string index_string in
  let index_string =
    StringIndex.fold
      (fun s exprs res ->
        let res = StringIndex.bind (Funct.uppercase s) (`Uppercase exprs) res in
        let res = StringIndex.bind (Funct.lowercase s) (`Lowercase exprs) res in
        res)
      index_string index_string in
  let index_string =
    StringIndex.fold
      (fun s1 exprs1 res ->
        StringIndex.fold
          (fun s2 exprs2 res ->
            res |> StringIndex.bind (s1 ^ s2) (`Concat (exprs1,exprs2)))
          index_string res)
      index_string index_string in
  { by_string = index_string;
    by_int = index_int }
