
(* parameters *)

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 256 string_of_int (* max nb of considered doc parses *)
let max_nb_doc_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_refinements = def_param "max_refinements" 200 string_of_int (* max nb of considered refinements *)


(* utilities *)
          
open Task

exception TODO

module TEST = (* for profiling visually, used for the JS version *)
  struct
    let prof name f =
      print_endline (name ^ "...");
      let res = f () in
      print_endline ("DONE " ^ name);
      res
  end

type 'a result = ('a,exn) Result.t
let ( let| ) res f = Result.bind res f [@@inline]

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

type 'a triple = 'a * 'a * 'a

type dl = Mdl.bits

let dl0 = 0.

let dl_round dl = Float.round (dl *. 1e9) /. 1e9

let dl_compare (dl1 : float) (dl2 : float) =
  if dl1 < dl2 then -1
  else if dl1 = dl2 then 0
  else 1 [@@inline]

(* types of model, data according to a model... *)

type doc_path =
  | ThisDoc
  | Left of doc_path
  | Middle of token_path
  | Right of doc_path
and token_path =
  | ThisToken
        
type doc_model =
  | Nil
  | Any
  | Factor of doc_model * token_model * doc_model
and token_model =
  | Const of string
  | Regex of regex_model
  | Expr of string_expr
and string_expr =
  | Ref of doc_path
  | Lowercase of string_expr
  | Uppercase of string_expr
and regex_model =
  | Alphas (* [-A-Za-z0-9_]+ *)
  | Nums (* [0-9]+\([.][0-9]*\)? *)
  | Letters (* [A-Za-z-]+ *)

let doc_model0 = Any

type doc_data =
  | DNil
  | DAny of string
  | DFactor of doc_data * token_data * doc_data
and token_data =
  | DToken of string
            
let doc_data0 = DNil

type env = doc_data

(* printing *)

let xp_string (print : Xprint.t) (s : string) =
  print#string s
let pp_string = Xprint.to_stdout xp_string
         
let rec xp_doc_path (print : Xprint.t) = function
  | ThisDoc -> print#string ""
  | Left p1 -> print#string "0"; xp_doc_path print p1
  | Middle p1 -> print#string "."; xp_token_path print p1
  | Right p1 -> print#string "1"; xp_doc_path print p1
and xp_token_path print = function
  | ThisToken -> ()
let pp_doc_path = Xprint.to_stdout xp_doc_path

let rec xp_doc_model (print : Xprint.t) (m : doc_model) =
  xp_doc_model_aux print m;
  print#string "|"
and xp_doc_model_aux print = function
  | Nil -> ()
  | Any ->
     print#string "|";
     print#string "*"
  | Factor (l,t,r) ->
     xp_doc_model_aux print l;
     print#string "|";
     xp_token_model print t;
     xp_doc_model_aux print r
and xp_token_model print = function
  | Const s -> xp_string print s
  | Regex re -> xp_regex_model print re
  | Expr e -> xp_string_expr print e
and xp_regex_model print = function
  | Alphas -> print#string "?_Alphas_"
  | Nums -> print#string "?_Nums_"
  | Letters -> print#string "?_Letters_"
and xp_string_expr print = function
  | Ref p -> xp_doc_path print p
  | Uppercase e1 -> print#string "uppercase("; xp_string_expr print e1; print#string ")"
  | Lowercase e1 -> print#string "lowercase("; xp_string_expr print e1; print#string ")"
let pp_doc_model = Xprint.to_stdout xp_doc_model
let string_of_doc_model = Xprint.to_string xp_doc_model
                    
let rec xp_doc_data (print : Xprint.t) (d : doc_data) =
  xp_doc_data_aux print d;
  print#string "|"
and xp_doc_data_aux print = function
  | DNil -> ()
  | DAny s ->
     print#string "|";
     xp_string print s
  | DFactor (l,t,r) ->
     xp_doc_data_aux print l;
     print#string "|";
     xp_token_data print t;
     xp_doc_data_aux print r
and xp_token_data print = function
  | DToken s -> xp_string print s
let pp_doc_data = Xprint.to_stdout xp_doc_data
let string_of_doc_data = Xprint.to_string xp_doc_data
         
(* get and apply *)

let rec doc_of_doc_data : doc_data -> string = function
  | DNil -> ""
  | DAny s -> s
  | DFactor (l,t,r) -> doc_of_doc_data l
                       ^ doc_of_token_data t
                       ^ doc_of_doc_data r
and doc_of_token_data : token_data -> string = function
  | DToken s -> s
         
let rec doc_find (p : doc_path) (d : doc_data) : string result =
  match p, d with
  | ThisDoc, _ -> Result.Ok (doc_of_doc_data d)
  | Left p1, DFactor (l,_,_) -> doc_find p1 l
  | Middle p1, DFactor (_,t,_) -> token_find p1 t
  | Right p1, DFactor (_,_,r) -> doc_find p1 r
  | _ -> assert false
and token_find p d =
  match p, d with
  | ThisToken, _ -> Result.Ok (doc_of_token_data d)


let rec doc_apply (m : doc_model) (env : env) : doc_model result =
  match m with
  | Nil -> Result.Ok Nil
  | Any -> Result.Ok Any
  | Factor (l,t,r) ->
     let| l' = doc_apply l env in
     let| t' = token_apply t env in
     let| r' = doc_apply r env in
     Result.Ok (Factor (l',t',r'))
and token_apply t env : token_model result =
  match t with
  | Const s -> Result.Ok (Const s)
  | Regex re -> Result.Ok (Regex re)
  | Expr e ->
     let| s = string_expr_apply e env in
     Result.Ok (Const s)
and string_expr_apply e env : string result =
  match e with
  | Ref p -> doc_find p env
  | Lowercase e1 ->
     let| s1 = string_expr_apply e1 env in
     Result.Ok (String.lowercase_ascii s1)
  | Uppercase e1 ->
     let| s1 = string_expr_apply e1 env in
     Result.Ok (String.uppercase_ascii s1)

     
(* generate *)

let rec doc_generate : doc_model -> doc_data = function
  | Nil -> DNil
  | Any -> DAny "..."
  | Factor (l,t,r) -> DFactor (doc_generate l, token_generate t, doc_generate r)
and token_generate : token_model -> token_data = function
  | Const s -> DToken s
  | Regex re -> DToken (regex_generate re)
  | Expr _ -> assert false
and regex_generate : regex_model -> string = function
  | Alphas -> "X1_Y2"
  | Nums -> "123"
  | Letters -> "Abc"


(* parse *)

exception Parse_failure

type 'a parseur = string -> 'a Myseq.t
             
let re_alphas = Str.regexp "[A-Za-z_0-9]+"
let re_nums = Str.regexp "[0-9]+"
let re_letters = Str.regexp "[A-Za-z]+"

let re_of_regexp = function
  | Alphas -> re_alphas
  | Nums -> re_nums
  | Letters -> re_letters

let regexp_match_full (re : Str.regexp) (s : string) : bool = (* TODO: optimize *)
  Str.string_match re s 0
  && Str.match_end () = String.length s
               
let regexp_match_slices (re : Str.regexp) (s : string) (len : int) (start : int) : (int * int) Myseq.t =
  assert (s <> "");
  (* beware of the re matches "" *)
  let rec aux start =
    if start >= len
    then Myseq.empty
    else
      try
        let i = Str.search_forward re s start in
        let j = Str.match_end () in
        if i = j (* not keeping nil matches *)
        then aux (i+1)
        else Myseq.cons (i,j) (aux j)
      with Not_found ->
        Myseq.empty
  in
  aux start

let token_parse (m : token_model) : (string * token_data * string) parseur =
  fun s ->
  assert (s <> "");
  let re =
    match m with
    | Const s -> Str.regexp_string s
    | Regex rm ->
       ( match rm with
       | Alphas -> re_alphas
       | Nums -> re_nums
       | Letters -> re_letters )
    | Expr _ -> assert false in
  let n = String.length s in
  let* i, j = regexp_match_slices re s n 0 in
  let sl = String.sub s 0 i in
  let st = String.sub s i (j-i) in
  let sr = String.sub s j (n-j) in
  let dt = DToken st in
  Myseq.return (sl,dt,sr)
let token_parse, reset_token_parse =
  let f, reset =
    Common.memoize ~size:1003
      (fun (m,s) -> Myseq.memoize (token_parse m s)) in
  let f = fun m s -> f (m,s) in
  f, reset

let rec doc_parse (m : doc_model) : doc_data parseur =
  fun s ->
  match m with
  | Nil ->
     if s = ""
     then Myseq.return DNil
     else Myseq.empty
  | Any ->
     Myseq.return (DAny s)
  | Factor (l,t,r) ->
     if s = ""
     then Myseq.empty
     else
       let* sl, dt, sr = token_parse t s in
       let* dl = doc_parse l sl in
       let* dr = doc_parse r sr in
       Myseq.return (DFactor (dl, dt, dr))


(* description lengths *)

let ascii_chars =
  let l = ref [] in
  for i = 0 to 127 do
    l := Char.chr i :: !l
  done;
  !l

let letter_chars =
  let l = ref [] in
  for i = Char.code 'a' to Char.code 'z' do l := Char.chr i :: !l done;
  for i = Char.code 'A' to Char.code 'Z' do l := Char.chr i :: !l done;
  l := '-' :: !l;
  !l

let num_chars =
  let l = ref [] in
  for i = Char.code '0' to Char.code '9' do l := Char.chr i :: !l done;
  !l

let alpha_chars = '_' :: num_chars @ letter_chars
     
let dl_string ?(chars = ascii_chars) (s : string) : dl =
  (* using prequential code *)
  let pc = new Mdl.Code.prequential chars in
  String.iter
    (fun c -> ignore (pc#code c))
    s;
  Mdl.Code.universal_int_star (String.length s)
  +. pc#cumulated_dl

let rec dl_doc_path : doc_path -> dl = function
  | ThisDoc -> Mdl.Code.usage 0.5
  | Left p1 -> Mdl.Code.usage 0.1 +. dl_doc_path p1
  | Middle p1 -> Mdl.Code.usage 0.3 +. dl_token_path p1
  | Right p1 -> Mdl.Code.usage 0.1 +. dl_doc_path p1
and dl_token_path : token_path -> dl = function
  | ThisToken -> 0.
  
let rec dl_doc_model : doc_model -> dl = function
  | Nil -> Mdl.Code.usage 0.5
  | Any -> Mdl.Code.usage 0.25
  | Factor (l,m,r) ->
     Mdl.Code.usage 0.25
     +. dl_doc_model l
     +. dl_token_model m
     +. dl_doc_model r
and dl_token_model : token_model -> dl = function
  | Const s ->
     Mdl.Code.usage 0.5
     +. dl_string s
  | Regex re ->
     Mdl.Code.usage 0.25
     +. dl_regex_model re
  | Expr e ->
     Mdl.Code.usage 0.25
     +. dl_string_expr e  
and dl_regex_model : regex_model -> dl = function
  | Alphas -> Mdl.Code.usage 0.5
  | Nums -> Mdl.Code.usage 0.25
  | Letters -> Mdl.Code.usage 0.25
and dl_string_expr : string_expr -> dl = function
  | Ref p ->
     Mdl.Code.usage 0.8
     +. dl_doc_path p (* TODO: should take env-model into account *)
  | Uppercase e1 ->
     Mdl.Code.usage 0.1
     +. dl_string_expr e1
  | Lowercase e1 ->
     Mdl.Code.usage 0.1
     +. dl_string_expr e1

type 'a encoder = 'a -> dl

let rec doc_encoder : doc_model -> doc_data encoder = function
  (* assuming that the passed data matches the model *)
  | Nil -> (function DNil -> 0. | _ -> assert false)
  | Any -> (function DAny s -> dl_string s | _ -> assert false)
  | Factor (l,m,r) ->
     let enc_l = doc_encoder l in
     let enc_m = token_encoder m in
     let enc_r = doc_encoder r in
     (function
      | DFactor (dl,dm,dr) -> enc_l dl +. enc_m dm +. enc_r dr
      | _ -> assert false)
and token_encoder : token_model -> token_data encoder = function
  | Const _ -> (function DToken _ -> 0.)
  | Regex re ->
     let enc_re = regex_encoder re in
     (function DToken s -> enc_re s)
  | Expr _ -> (function _ -> 0.)
and regex_encoder : regex_model -> string encoder = function
  | Alphas -> dl_string ~chars:alpha_chars
  | Nums -> dl_string ~chars:num_chars
  | Letters -> dl_string ~chars:letter_chars


(* reading *)

type 'a read = env * 'a * dl
type doc_read = env * doc_data * dl
type token_read = env * token_data * dl

let limit_dl (f_dl : 'a -> dl) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x0::_ ->
     let dl0 = f_dl x0 in
     let min_dl = !max_parse_dl_factor *. dl0 in
     List.filter (fun x -> f_dl x <= min_dl) l

let read_doc ~(env : env) (m0 : doc_model) (s : string) : doc_read list result =
  Common.prof "Model.read_doc" (fun () ->
  let| m = doc_apply m0 env in (* reducing expressions *)
  let parses =
    let* data = doc_parse m s in
    let dl = (* QUICK *)
      let dl_data = doc_encoder m0 data in
      (* rounding before sorting to absorb float error accumulation *)
      dl_round dl_data in
    Myseq.return (env, data, dl) in
  let l_parses =
    Common.prof "Model.read_doc/first_parses" (fun () ->
        parses
        |> Myseq.slice ~offset:0 ~limit:(!max_nb_parse)
        |> Myseq.to_list) in
  if l_parses = []
  then Result.Error Parse_failure
  else
    let best_parses =
      l_parses (* QUICK *)
      |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
      |> (fun l -> Common.sub_list l 0 !max_nb_doc_reads)
      |> limit_dl (fun (_,_,dl) -> dl)
      |> List.mapi (fun rank (env,data,dl) ->
             let dl = dl +. Mdl.Code.universal_int_star rank in (* to penalize later parses, in case of equivalent parses *)
             (env, data, dl)) in
    Result.Ok best_parses)

type docs_reads =
  { dl_m : dl; (* DL of the model *)
    reads : doc_read list list; (* outer list over docs, inner list over parses, sorted in increasing DL *)
  }

(* writing *)

let write_doc ~(env : doc_data) (m : doc_model) : (string, exn) Result.t = Common.prof "Model.write_doc" (fun () ->
  let| m' = doc_apply m env in
  let d = doc_generate m' in
  let s = doc_of_doc_data d in
  Result.Ok s)
             
(* refinements *)

type doc_refinement =
  | RNil
  | RFactor of doc_model * token_model * doc_model (* at doc Any *)
  | RToken of token_model (* token specialization *)
  | RExpr of string_expr

let xp_doc_refinement (print : Xprint.t) = function
  | RNil -> print#string "nil"
  | RFactor (l,tok,r) ->
     xp_doc_model_aux print l;
     print#string "|";
     xp_token_model print tok;
     xp_doc_model_aux print r;
     print#string "|"
  | RToken tok -> xp_token_model print tok
  | RExpr e -> xp_string_expr print e
let pp_doc_refinement = Xprint.to_stdout xp_doc_refinement
           
let apply_doc_refinement (r : doc_refinement) (p : doc_path) (m : doc_model) : doc_model =
  let rec aux_doc p m =
    match p, m with
    | ThisDoc, Any ->
       (match r with
        | RNil -> Nil
        | RFactor (l,tok,r) -> Factor (l, tok, r)
        | _ -> assert false)
    | Left p1, Factor (l,t,r) -> Factor (aux_doc p1 l, t, r)
    | Middle p1, Factor (l, t, r) -> Factor (l, aux_token p1 t, r)
    | Right p1, Factor (l, t, r) -> Factor (l, t, aux_doc p1 r)
    | _ -> assert false
  and aux_token p m =
    match p, m with
    | ThisToken, _ ->
       (match r, m with
        | RToken tok, Regex _ -> tok
        | RExpr e, _ -> Expr e
        | _ -> assert false)
  in
  aux_doc p m

let for_all_reads (f : 'a read -> bool) (reads : 'a read list list) : bool =
  List.for_all
    (fun example_reads ->
      List.exists
        (fun read ->
          f read)
        example_reads)
    reads

let map_reads (f : 'a -> 'b) (reads : 'a read list list) : 'b read list list  =
  List.map
    (fun example_reads ->
      List.map
        (fun (env,data,dl) ->
          (env, f data, dl))
        example_reads)
    reads

let common_reads (norm : 'a -> 'b) (reads : 'a read list list) : 'b Bintree.t =
  let sets =
    List.map
      (fun example_reads ->
        List.fold_left
          (fun res (env,data,dl) -> Bintree.add (norm data) res)
          Bintree.empty example_reads)
      reads in
  match sets with
  | [] -> assert false
  | set0::sets1 ->
     List.fold_left Bintree.inter set0 sets1
     
  
let doc_refinements (m : doc_model) (dsr : docs_reads) : (doc_path * doc_refinement * doc_model) Myseq.t =
  let rec fold_doc m reads =
    match m with
    | Nil -> Myseq.empty
    | Any ->
       myseq_cons_if
         (reads |> for_all_reads (fun (env,data,dl) -> doc_of_doc_data data = ""))
         (ThisDoc, RNil)
         (myseq_concat_if
            (reads |> for_all_reads (fun (env,data,dl) -> doc_of_doc_data data <> ""))
            (let* re = Myseq.from_list [Alphas; Nums; Letters] in
             Myseq.return (ThisDoc, RFactor (Any, Regex re, Any)))
            (let _ =
               print_endline
                 (String.concat "; "
                    (List.map
                       (fun example_reads ->
                         String.concat "|"
                           (List.map
                              (function (_, DAny s, _) -> s | _ -> assert false)
                              example_reads))
                       reads)) in
             let common_strings = Bintree.elements (common_reads doc_of_doc_data reads) in
             let* s = Myseq.from_list common_strings in (* TODO: add a to_seq in bintree.ml *)
             if s = ""
             then Myseq.empty
             else (Printf.printf "common string: %s" s; Myseq.return (ThisDoc, RFactor (Nil, Const s, Nil)))))
    | Factor (l,t,r) ->
       Myseq.concat
         [ fold_token t
             (map_reads
                (function
                 | DFactor (_,dt,_) -> dt
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Middle p, r);
           fold_doc l
             (map_reads
                (function
                 | DFactor (dl,_,_) -> dl
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Left p,r);
           fold_doc r
             (map_reads
                (function
                 | DFactor (_,_,dr) -> dr
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Right p, r) ]
  and fold_token (m : token_model) (reads : token_read list list) =
    match m with
    | Const s -> Myseq.empty
    | Regex re ->
       Myseq.concat
         [ (* constant strings *)
           (let common_strings = Bintree.elements (common_reads doc_of_token_data reads) in
            let* s = Myseq.from_list common_strings in (* TODO: add a to_seq in bintree.ml *)
            Myseq.return (ThisToken, RToken (Const s)));
           
           (* regexp specialization *)
           (let* re' =
              Myseq.from_list
                (match re with
                 | Alphas -> [Nums; Letters]
                 | _ -> []) in
            if reads
               |> for_all_reads
                    (fun (env,data,dl) ->
                      regexp_match_full (re_of_regexp re') (doc_of_token_data data))
            then Myseq.return (ThisToken, RToken (Regex re'))
            else Myseq.empty) ]
    | Expr e -> Myseq.empty
  in
  let* p, r = fold_doc m dsr.reads in
  Myseq.return (p, r, apply_doc_refinement r p m)

(* examples  / pairs *)
             
type model =
  { input_model : doc_model; (* no reference *)
    output_model : doc_model
  }

let model0 = { input_model = doc_model0;
               output_model = doc_model0 }

let xp_model (print : Xprint.t) (m : model) =
  xp_doc_model print m.input_model;
  print#string " ==> ";
  xp_doc_model print m.output_model
let pp_model = Xprint.to_stdout xp_model
let string_of_model = Xprint.to_string xp_model

             
type pairs_reads = (* result of reading a list of pairs of grids *)
  { dl_mi : dl; (* input model DL *)
    dl_mo : dl; (* output model DL *)
    inputs_reads : doc_read list list; (* outer list over example inputs, inner list over parses *)
    reads : (doc_read * doc_read * dl) list list; (* outer list over examples, inner list over parses, sorted in increasing DL *)
  }

let read_pairs ?(env = doc_data0) (m : model) (pairs : Task.pair list) : pairs_reads result =
  Common.prof "Model.read_pairs" (fun () ->
  (* takes model, input env+docs, output docs *)
  let dl_mi = dl_doc_model m.input_model in    
  (*let env_sig =
    signature_of_template m.input_pattern in *)
  let dl_mo = dl_doc_model m.output_model in
  let| inputs_reads_reads =
    pairs
    |> list_map_result
         (fun {input; output} ->
           let| input_reads =
             read_doc ~env m.input_model input in (* no diff allowed during training *)
           let| pair_reads = 
             let+|+ (envi,ddi,dli as ri) = Result.Ok input_reads in      
             let+|+ (envo,ddo,dlo as ro) =
               read_doc ~env:ddi m.output_model output in
             let dl = dli +. dlo in
             Result.Ok [(ri,ro,dl)] in
           let pair_reads =
             pair_reads
             |> List.stable_sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
           (* TODO |> limit_dl (fun (_,_,dl) -> dl) *) in (* bounding by dl_factor *) 
           Result.Ok (input_reads, pair_reads)) in
  let inputs_reads, reads = List.split inputs_reads_reads in
  Result.Ok {dl_mi; dl_mo; inputs_reads; reads})

let dl_model_data (psr : pairs_reads) : dl triple triple = (* QUICK *)
  let lmi = psr.dl_mi in
  let lmo = psr.dl_mo in
  let ldi, ldo =
    List.fold_left
      (fun (ldi,ldo) ->
        function
        | ((_,_,dli),(_,_,dlo),dl)::_ -> (ldi +. dli, ldo +. dlo)
        | _ -> assert false)
      (0.,0.) psr.reads in
  let ldi, ldo = !alpha *. ldi, !alpha *. ldo in
  let lmdi = lmi +. ldi in
  let lmdo = lmo +. ldo in
  (lmi, lmo, lmi +. lmo),
  (ldi, ldo, ldi +. ldo),
  (lmdi, lmdo, lmdi +. lmdo)

let make_norm_dl_model_data () : pairs_reads -> dl triple triple =
  let lmdi0 = ref (-1.) in
  let lmdo0 = ref (-1.) in
  fun psr ->
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi,lmdo,lmd) =
    dl_model_data psr in
  let () = (* setting initial DLs *)
    if !lmdi0 < 0.
    then ( lmdi0 := lmdi; lmdo0 := lmdo ) in
  let nlmi, nldi, nlmdi = lmi /. !lmdi0, ldi /. !lmdi0, lmdi /. !lmdi0 in
  let nlmo, nldo, nlmdo = lmo /. !lmdo0, ldo /. !lmdo0, lmdo /. !lmdo0 in
  (nlmi, nlmo, nlmi +. nlmo),
  (nldi, nldo, nldi +. nldo),
  (nlmdi, nlmdo, nlmdi +. nlmdo)

let split_pairs_read (prs : pairs_reads) : docs_reads * docs_reads =
  let project_reads proj =
    List.map
      (fun pair_reads ->
        pair_reads
        |> List.map proj)
      prs.reads in
  let inputs_reads = project_reads (fun (dri,_,_) -> dri) in
  let outputs_reads = project_reads (fun (_,dro,_) -> dro) in
  let dsri = { dl_m = prs.dl_mi; reads = inputs_reads } in
  let dsro = { dl_m = prs.dl_mo; reads = outputs_reads } in
  dsri, dsro


let apply_model ?(env = doc_data0) (m : model) (doc_i : string) : ((doc_data * string) list, exn) Result.t =
  Common.prof "Model.apply_model" (fun () ->
  let+|+ _, di, _ =
    read_doc ~env m.input_model doc_i in
  let| doc_o =
    write_doc ~env:di m.output_model in
  Result.Ok [(di, doc_o)])

  
type refinement =
  | RInit
  | Rinput of doc_path * doc_refinement
  | Routput of doc_path * doc_refinement

let xp_refinement (print : Xprint.t) = function
  | RInit -> print#string "init"
  | Rinput (p,ri) -> print#string "IN "; xp_doc_path print p; print#string " <- "; xp_doc_refinement print ri
  | Routput (p,ro) -> print#string "OUT "; xp_doc_path print p; print#string " <- "; xp_doc_refinement print ro
let pp_refinement = Xprint.to_stdout xp_refinement
let string_of_refinement = Xprint.to_string xp_refinement

let apply_refinement (r : refinement) (m : model) : (refinement * model) result =
  match r with
  | RInit -> Result.Error (Failure "apply_refinement")
  | Rinput (p,ri) ->
     Result.Ok (r, {m with input_model = apply_doc_refinement ri p m.input_model})
  | Routput (p,ro) ->
     Result.Ok (r, {m with output_model = apply_doc_refinement ro p m.output_model})

let model_refinements (last_r : refinement) (m : model) (dsri : docs_reads) (dsro : docs_reads) : (refinement * model) Myseq.t =
  Myseq.concat (* TODO: rather order by estimated dl *)
    [ (let* p, ri, mi = doc_refinements m.input_model dsri in
       Myseq.return (Rinput (p,ri), {m with input_model = mi}));
      (let* p, ro, mo = doc_refinements m.output_model dsro in
       Myseq.return (Routput (p,ro), {m with output_model = mo})) ]

  
(* learning *)

let learn_model
      ?(verbose = false)
      ?(pause = 0.)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (pairs_reads * docs_reads * docs_reads) * dl) list * bool
  = Common.prof "Model.learn_model" (fun () ->
  let norm_dl_model_data = make_norm_dl_model_data () in
  Mdl.Strategy.beam
    ~timeout
    ~beam_width
    ~refine_degree
    ~m0:(RInit, init_model)
    ~data:(fun (r,m) ->
      try
        (*if verbose then (
          print_string "\t=> "; pp_refinement r; print_newline ()); *)
        Result.to_option
          (let| prs = read_pairs m pairs in
           let drsi, drso = split_pairs_read prs in
           Result.Ok (prs,drsi,drso))
      with
      | Common.Timeout as exn -> raise exn
      | exn ->
         print_endline "ERROR while parsing examples with new model";
	 print_endline (Printexc.to_string exn);
	 pp_refinement r; print_newline ();
         pp_model m; print_newline ();
	 raise exn)
    ~code:(fun (r,m) (gpsr,gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     norm_dl_model_data gpsr in
           if verbose then (
             Printf.printf "\t?? %.3f\t" lmd;
             pp_refinement r; print_newline ();
(*
	     Printf.printf "\t\tl = %.3f = %.3f + %.3f = (%.3f + %.3f) + (%.3f + %.3f)\n" lmd lm ld lmi lmo ldi ldo;
             print_endline " ===> all reads for first example";
             List.hd gpsr.reads
             |> List.iter
                  (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                    print_endline " --- some read ---";
                    pp_data d_i; print_newline ();
                    pp_data d_o; print_newline ();
                    Printf.printf "\tdl=%.6f\n" dl);
             print_newline ()
              
             print_endline " ===> best read for all examples";
             gpsr.reads
             |> List.iter
                  (fun read ->
                    List.hd read
                    |> (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                     print_endline " --- some read ---";
                     pp_data d_i; print_newline ();
                     pp_data d_o; print_newline ();
                     Printf.printf "\tdl=%.3f\n" dl));
             print_newline ();
  *)
           );
	   flush stdout;
           lmd)
    ~refinements:
    (fun (r,m) (gpsr,gsri,gsro) dl ->
      if verbose then print_newline ();
      Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ();
      if verbose then (
        print_endline " ===> first read for first example";
        List.hd (List.hd gpsr.reads)
        |> (fun ((_,d_i,dl_i), (_, d_o, dl_o), dl) ->
          print_endline " --- some read ---";
          pp_doc_data d_i; print_newline ();
          pp_doc_data d_o; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = model_refinements r m gsri gsro in
      refs))

