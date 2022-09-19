
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

type doc_ctx = doc_path -> doc_path
type token_ctx = token_path -> doc_path

let doc_ctx0 = (fun p -> p)
               
type expr = doc_path Expr.expr (* using doc_paths as vars *)
        
type doc_model =
  | Nil
  | Any
  | Factor of doc_model * token_model * doc_model
and token_model =
  | Const of string
  | Regex of regex_model
  | Expr of expr
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
  print#string "<pre class=\"inline\">";
  print#string s;
  print#string "</pre>"
let pp_string = Xprint.to_stdout xp_string

let number_of_doc_path (p : doc_path) : int * token_path option =
  (* mapping doc_paths to unique integers + residual token_path *)
  let rec aux power2 acc = function
    | ThisDoc -> power2 + acc, None
    | Left p1 -> aux (2 * power2) acc p1
    | Middle p1 -> power2 + acc, Some p1
    | Right p1 -> aux (2 * power2) (power2 + acc) p1
  in
  aux 1 0 p
              
let xp_doc_path (print : Xprint.t) (p : doc_path) =
  let n, tp_opt = number_of_doc_path p in
  print#string "<span class=\"model-path\">";
  ( match tp_opt with
  | None -> print#string "doc"; print#int n
  | Some ThisToken -> print#string "tok"; print#int n);
  print#string "</span>"
(*  | ThisDoc -> print#string "."
  | Left p1 -> print#string "0"; xp_doc_path print p1
  | Middle p1 -> print#string "@"; xp_token_path print p1
  | Right p1 -> print#string "1"; xp_doc_path print p1
and xp_token_path print = function
  | ThisToken -> () *)
let pp_doc_path = Xprint.to_stdout xp_doc_path

let rec xp_doc_model (print : Xprint.t) ?(ctx : doc_ctx option) = function
  | Nil -> ()
  | Any ->
     print#string "<span class=\"model-any\">*</span>"
  | Factor (l,t,r) ->
     let ctx_l = ctx |> Option.map (fun ctx -> (fun p -> ctx (Left p))) in
     let ctx_t = ctx |> Option.map (fun ctx -> (fun p -> ctx (Middle p))) in
     let ctx_r = ctx |> Option.map (fun ctx -> (fun p -> ctx (Right p))) in
     print#string "<span class=\"model-factor\">";
     xp_doc_model print ?ctx:ctx_l l;
     xp_token_model print ?ctx:ctx_t t;
     xp_doc_model print ?ctx:ctx_r r;
     print#string "</span>"
and xp_token_model print ?(ctx : token_ctx option) = function
  | Const s ->
     print#string "<span class=\"model-const\">";
     xp_string print s;
     print#string "</span>"
  | Regex re ->
     let p_opt = ctx |> Option.map (fun ctx -> ctx ThisToken) in
     print#string "<span class=\"model-regex\">";
     print#string "?";
     p_opt |> Option.iter (fun p -> xp_doc_path print p);
     print#string " : ";
     xp_regex_model print re;
     print#string "</span>"
  | Expr e ->
     print#string "<span class=\"model-expr\">";
     Expr.xp_expr xp_doc_path print e;
     print#string "</span>"
and xp_regex_model print = function
  | Alphas -> print#string "Alphas"
  | Nums -> print#string "Digits"
  | Letters -> print#string "Letters"
let pp_doc_model m = Xprint.to_stdout (xp_doc_model ~ctx:doc_ctx0) m
let string_of_doc_model m = Xprint.to_string (xp_doc_model ~ctx:doc_ctx0) m
                    
let rec xp_doc_data (print : Xprint.t) = function
  | DNil -> ()
  | DAny s ->
     print#string "<span class=\"data-any\">";
     xp_string print s;
     print#string "</span>"
  | DFactor (l,t,r) ->
     xp_doc_data print l;
     xp_token_data print t;
     xp_doc_data print r
and xp_token_data print = function
  | DToken s ->
     print#string "<span class=\"data-token\">";
     xp_string print s;
     print#string "</span>"
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

let rec doc_data_length : doc_data -> int = function
  | DNil -> 0
  | DAny s -> String.length s
  | DFactor (l,t,r) -> doc_data_length l + token_data_length t + doc_data_length r
and token_data_length = function
  | DToken s -> String.length s
              
let rec doc_find (p : doc_path) (d : doc_data) : Expr.value result =
  match p, d with
  | ThisDoc, _ -> Result.Ok (`String (doc_of_doc_data d))
  | Left p1, DFactor (l,_,_) -> doc_find p1 l
  | Middle p1, DFactor (_,t,_) -> token_find p1 t
  | Right p1, DFactor (_,_,r) -> doc_find p1 r
  | _ -> assert false
and token_find p d =
  match p, d with
  | ThisToken, _ -> Result.Ok (`String (doc_of_token_data d))

let doc_bindings (d : doc_data) : (doc_path * Expr.value) list =
  let rec aux_doc ctx d acc =
    match d with
    | DNil -> acc
    | DAny _ -> acc
    | DFactor (l,t,r) ->
       let acc = aux_doc (fun p -> ctx (Right p)) r acc in
       let acc = aux_doc (fun p -> ctx (Left p)) l acc in
       let acc = aux_token (fun p -> ctx (Middle p)) t acc in
       acc
  and aux_token ctx t acc =
    match t with
    | DToken s -> (ctx ThisToken, `String s) :: acc
  in
  aux_doc (fun p -> p) d []

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
     let| v = Expr.eval (fun p -> doc_find p env) e in
     let| s = Expr.string_of_value v in     
     Result.Ok (Const s)

     
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

let ascii_init_occs =
  (* freely adapted from http://millikeys.sourceforge.net/freqanalysis.html *)
  [ ' ', 70.;
    'E', 18.;
    'e', 18.;
    'T', 13.;
    't', 13.;
    'A', 12.;
    'a', 12.;
    'O', 11.;
    'o', 11.;
    'I', 10.;
    'i', 10.;
    'N', 10.;
    'n', 10.;
    '0', 10.;
    '1', 10.;
    '2', 10.;
    '3', 10.;
    '4', 10.;
    '5', 10.;
    '6', 10.;
    '7', 10.;
    '8', 10.;
    '9', 10.;
    'H', 9.;
    'h', 9.;
    'S', 9.;
    's', 9.;
    'R', 8.;
    'r', 8.;
    'D', 7.;
    'd', 7.;
    'L', 6.;
    'l', 6.;
    'U', 4.;
    'u', 4.;
    'M', 4.;
    'm', 4.;
    'C', 4.;
    'c', 4.;
    'W', 3.5;
    'w', 3.5;
    'G', 3.;
    'g', 3.;
    'F', 3.;
    'f', 3.;
    'Y', 3.;
    'y', 3.;
    'P', 2.5;
    'p', 2.5;
    '_', 5.;
    ',', 4.8;
    '.', 4.7;
    'B', 2.3;
    'b', 2.3;
    'K', 1.4;
    'k', 1.4;
    'V', 1.4;
    'v', 1.4;
    '"', 2.6;
    '\'', 1.7;
    '-', 1.0;
    '?', 0.47;
    'X', 0.23;
    'x', 0.23;
    'J', 0.22;
    'j', 0.22;
    ';', 0.31;
    '!', 0.30;
    'Q', 0.14;
    'q', 0.14;
    'Z', 0.13;
    'z', 0.13;
  ]
       
let ascii_chars =
  let l = ref [] in
  for i = 0 to 127 do
    l := Char.chr i :: !l
  done;
  !l
let make_ascii_prequential = new Mdl.Code.prequential ascii_chars (* no assumption on char freq, not to spoil the value of specific regexs *)

let letter_chars =
  let l = ref [] in
  for i = Char.code 'a' to Char.code 'z' do l := Char.chr i :: !l done;
  for i = Char.code 'A' to Char.code 'Z' do l := Char.chr i :: !l done;
  !l
let make_letter_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs letter_chars

let num_chars =
  let l = ref [] in
  for i = Char.code '0' to Char.code '9' do l := Char.chr i :: !l done;
  !l
let make_num_prequential = new Mdl.Code.prequential num_chars

let alpha_chars = '_' :: num_chars @ letter_chars
let make_alpha_prequential = new Mdl.Code.prequential ~init_occs:ascii_init_occs alpha_chars
     
let dl_char_plus (make_pc : unit -> char Mdl.Code.prequential) (s : string) : dl =
  (* using prequential code *)
  let pc = make_pc () in
  String.iter
    (fun c -> ignore (pc#code c))
    s;
  pc#cumulated_dl

(* let rec dl_doc_path : doc_path -> dl = function (* TODO: take env into account *)
  | ThisDoc -> Mdl.Code.usage 0.5
  | Left p1 -> Mdl.Code.usage 0.1 +. dl_doc_path p1
  | Middle p1 -> Mdl.Code.usage 0.3 +. dl_token_path p1
  | Right p1 -> Mdl.Code.usage 0.1 +. dl_doc_path p1
and dl_token_path : token_path -> dl = function
  | ThisToken -> 0. *)
  
let rec dl_doc_model ~(env : doc_model) (m : doc_model) : dl =
  let nb_env_paths = dl_doc_model_env_stats env in (* useful view on the environment model *)
  let nb_any, nb_factor = dl_doc_model_stats m in
  Mdl.Code.universal_int_star nb_factor (* encoding total nb of factors/tokens *)
  +. Mdl.Code.universal_int_star nb_any (* encoding total nb of Any's, to favor Nil's *)
  (* +. Mdl.Code.uniform (nb_factor + 2) (* alternate encoding, based on bound for nb_any *) *)
  +. dl_doc_model_aux ~nb_env_paths nb_any nb_factor m
and dl_doc_model_aux ~nb_env_paths nb_any nb_factor = function
  | Nil ->
     assert (nb_any = 0 && nb_factor = 0);
     0.
  | Any ->
     assert (nb_any = 1 && nb_factor = 0);
     0.
  | Factor (l,t,r) ->
     let na_l, nf_l = dl_doc_model_stats l in
     let na_r, nf_r = dl_doc_model_stats r in
     assert (na_l + na_r = nb_any);
     assert (nf_l + 1 + nf_r = nb_factor);
     Mdl.Code.uniform (nb_factor) (* encoding split of remaining Factor's *)
     +. Mdl.Code.uniform (* encoding split of Any's, values of na_l, na_r *)
          (min nb_any (nf_l + 1) (* maximal possible value for na_l *)
           - max 0 (nb_any - (nf_r + 1)) (* minimal possible value for na_l *)
           + 1)
     +. dl_doc_model_aux ~nb_env_paths na_l nf_l l
     +. dl_token_model ~nb_env_paths t
     +. dl_doc_model_aux ~nb_env_paths na_r nf_r r
and dl_doc_model_stats : doc_model -> int * int = function
  (* counting Any and Factor inside doc_model *)
  | Nil -> 0, 0
  | Any -> 1, 0
  | Factor (l,t,r) ->
     let na_l, nf_l = dl_doc_model_stats l in
     let na_r, nf_r = dl_doc_model_stats r in
     na_l + na_r, nf_l + 1 + nf_r
and dl_doc_model_env_stats : doc_model -> int = function
  (* counting paths to tokens *)
  | Nil -> 0
  | Any -> 0
  | Factor (l,t,r) ->
     dl_doc_model_env_stats l
     + 1
     + dl_doc_model_env_stats r
and dl_token_model ~nb_env_paths : token_model -> dl = function
  | Const s ->
     Mdl.Code.usage 0.3
     +. Mdl.Code.universal_int_plus (String.length s)
     +. dl_char_plus make_ascii_prequential s
  | Regex re ->
     Mdl.Code.usage 0.2
     +. dl_regex_model re
  | Expr e ->
     Mdl.Code.usage 0.5
     +. Expr.dl_expr (fun p -> Mdl.Code.uniform nb_env_paths) e  
and dl_regex_model : regex_model -> dl = function
  | Alphas -> Mdl.Code.usage 0.4
  | Nums -> Mdl.Code.usage 0.3
  | Letters -> Mdl.Code.usage 0.3

type 'a encoder = 'a -> dl

let rec doc_encoder (m : doc_model) : doc_data encoder =
  (* assuming that the passed data matches the model *)
  let enc_m = doc_encoder_aux m in
  fun d ->
  let n = doc_data_length d in
  Mdl.Code.universal_int_star n
  +. enc_m d
and doc_encoder_aux : doc_model -> doc_data encoder = function
  | Nil ->
     (function
      | DNil -> 0.
      | _ -> assert false)
  | Any ->
     (function
      | DAny s -> dl_char_plus make_ascii_prequential s
      | _ -> assert false)
  | Factor (l,t,r) ->
     let enc_split = (* TODO: better take into account actual l, t, r *)
       let range_l = doc_encoder_range l in
       let range_t = token_encoder_range t in
       let range_r = doc_encoder_range r in
       (fun (nl,nt,nr) ->
         let n = nl + nt + nr in (* n is assumed known from above *)
         let range_nl = Range.inter_list [
                            Range.make_closed 0 n;
                            range_l;
                            Range.sub
                              (Range.make_exact n)
                              (Range.add range_t range_r) ] in
         let range_nt = Range.inter_list [ (* given nl *)
                            Range.make_closed 1 (n - nl);
                            range_t;
                            Range.sub
                              (Range.make_exact (n - nl))
                              range_r ] in
         Range.dl nl range_nl (* encoding nl given n, and ranges *)
         +. Range.dl nt range_nt  (* encoding nt given n, n, and ranges  *)
         +. 0. (* encoding nr = n - nl - nt *)
       ) in
     let enc_l = doc_encoder_aux l in
     let enc_t = token_encoder t in
     let enc_r = doc_encoder_aux r in
     (function
      | DFactor (dl,dt,dr) ->
         let nl_nt_nr = doc_data_length dl, token_data_length dt, doc_data_length dr in
         enc_split nl_nt_nr +. enc_l dl +. enc_t dt +. enc_r dr
      | _ -> assert false)
and doc_encoder_range : doc_model -> Range.t = function
  (* min-max length range for doc_models *)
  | Nil -> Range.make_exact 0
  | Any -> Range.make_open 0
  | Factor (l,t,r) ->
     let range_l = doc_encoder_range l in
     let range_t = token_encoder_range t in
     let range_r = doc_encoder_range r in
     Range.sum [range_l; range_t; range_r]
and token_encoder_range : token_model -> Range.t = function
  | Const s -> Range.make_exact (String.length s)
  | Regex _ -> Range.make_open 1
  | Expr _ -> assert false
and token_encoder : token_model -> token_data encoder = function
  | Const _ -> (function DToken _ -> 0.)
  | Regex re ->
     let enc_re = regex_encoder re in
     (function DToken s -> enc_re s)
  | Expr _ -> assert false
and regex_encoder : regex_model -> string encoder = function
  | Alphas -> dl_char_plus make_alpha_prequential
  | Nums -> dl_char_plus make_num_prequential
  | Letters -> dl_char_plus make_letter_prequential


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
      let dl_data = doc_encoder m data in
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
  | RExpr of expr

let xp_doc_refinement (print : Xprint.t) = function
  | RNil -> print#string "<span class=\"model-nil\">∅</span>"
  | RFactor (l,tok,r) ->
     xp_doc_model print l;
     xp_token_model print tok;
     xp_doc_model print r
  | RToken tok -> xp_token_model print tok
  | RExpr e -> xp_token_model print (Expr e)
     (* Expr.xp_expr xp_doc_path print e *)
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

let map_reads (f : 'a -> 'b) (reads : 'a list list) : 'b list list  =
  List.map
    (fun example_reads ->
      List.map f example_reads)
    reads

let inter_union_reads (* to find things common to all examples, in at least one parse of each example *)
      ~(init : 'b) (* neutral value for union *)
      ~(union : 'b -> 'a -> 'b) (* taking into account another parse of the same example *)
      ~(inter : 'b -> 'b -> 'b) (* finding what is in common between two examples *)
      (reads : 'a list list) : 'b =
  let process_example reads =
    List.fold_left
      (fun res read -> union res read)
      init reads in
  match reads with
  | [] -> assert false
  | example0_reads :: other_reads ->
     let res0 = process_example example0_reads in
     List.fold_left
       (fun res exampleI_reads ->
         let resI = process_example exampleI_reads in
         inter res resI)
       res0 other_reads
  
let doc_refinements (m : doc_model) (dsr : docs_reads) : (doc_path * doc_refinement * doc_model) Myseq.t =
  let reads = (* replacing env's with expression index's over them *)
    map_reads
      (fun (env,data,dl) ->
        (Expr.make_index (doc_bindings env), data, dl))
      dsr.reads in
  let rec fold_doc m reads =
    match m with
    | Nil -> Myseq.empty
    | Any ->
       let is_nil, is_not_nil, common_strings =
         inter_union_reads
           ~init:(false, false, Bintree.empty)
           ~union:(fun (is_nil, is_not_nil, common_strings) (_,data,_) ->
             let s = doc_of_doc_data data in
             is_nil || s = "",
             is_not_nil || s <> "",
             if s = "" then common_strings else Bintree.add s common_strings)
           ~inter:(fun (is_nil1, is_not_nil1, common_strings1)
                       (is_nil2, is_not_nil2, common_strings2) ->
             is_nil1 && is_nil2,
             is_not_nil1 && is_not_nil2,
             Bintree.inter common_strings1 common_strings2)
           reads in       
       myseq_cons_if is_nil
         (ThisDoc, RNil)
         (myseq_concat_if is_not_nil
            (let* re = Myseq.from_list [Alphas; Nums; Letters] in
             Myseq.return (ThisDoc, RFactor (Any, Regex re, Any)))
            ((*let _ =
               print_endline
                 (String.concat "; "
                    (List.map
                       (fun example_reads ->
                         String.concat "｜"
                           (List.map
                              (function (_, DAny s, _) -> s | _ -> assert false)
                              example_reads))
                       reads)) in *)
             let* s = Myseq.from_list (Bintree.elements common_strings) in (* TODO: add a to_seq in bintree.ml *)
             Myseq.return (ThisDoc, RFactor (Nil, Const s, Nil))))
    | Factor (l,t,r) ->
       Myseq.concat
         [ fold_token t
             (map_reads
                (function
                 | (idx, DFactor (_,dt,_), l) -> (idx, dt, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Middle p, r);
           fold_doc l
             (map_reads
                (function
                 | (idx, DFactor (dl,_,_), l) -> (idx, dl, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Left p,r);
           fold_doc r
             (map_reads
                (function
                 | (idx, DFactor (_,_,dr), l) -> (idx, dr, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r) -> Right p, r) ]
  and fold_token (m : token_model) reads =
    match m with
    | Const s -> Myseq.empty
    | Regex re ->
       let re'_ok_init =
         match re with
         | Alphas -> [Nums, false; Letters, false]
         | _ -> [] in
       let (common_strings : string Bintree.t), (re'_ok : (regex_model * bool) list), (exprs : doc_path Expr.exprset list) =
         inter_union_reads
           ~init:(Bintree.empty, re'_ok_init, [])
           ~union:(fun (common_strings, re'_ok, exprs) (idx,data,_) ->
             let s = doc_of_token_data data in
             let es = Expr.index_lookup (`String s) idx in
             (if s = "" then common_strings else Bintree.add s common_strings),
             List.map
               (fun (re',ok) ->
                 (re', ok || regexp_match_full (re_of_regexp re') s))
               re'_ok,
             (if es = [] then exprs else es::exprs))
           ~inter:(fun (common_strings1, re'_ok1, exprs1) (common_strings2, re'_ok2, exprs2) ->
             Bintree.inter common_strings1 common_strings2,
             List.map2
               (fun (re'1,ok1) (re'2,ok2) ->
                 assert (re'1 = re'2);
                 (re'1, ok1 && ok2))
               re'_ok1 re'_ok2,
             Expr.exprset_inter_list exprs1 exprs2)
           reads in
       Myseq.concat
         [ (* constant strings *)
           (let* s = Myseq.from_list (Bintree.elements common_strings) in (* TODO: add a to_seq in bintree.ml *)
            Myseq.return (ThisToken, RToken (Const s)));
           
           (* regexp specialization *)
           (let* re', ok = Myseq.from_list re'_ok in
            if ok
            then Myseq.return (ThisToken, RToken (Regex re'))
            else Myseq.empty);

           (* expressions *)
           (let* es = Myseq.from_list exprs in
            let* e = Expr.exprset_to_seq es in
            Myseq.return (ThisToken, RExpr e)) ]
    | Expr e -> Myseq.empty
  in
  let* p, r = fold_doc m reads in
  Myseq.return (p, r, apply_doc_refinement r p m)

(* examples  / pairs *)
             
type model =
  { input_model : doc_model; (* no reference *)
    output_model : doc_model
  }

let model0 = { input_model = doc_model0;
               output_model = doc_model0 }

let xp_model (print : Xprint.t) (m : model) =
  xp_doc_model print ~ctx:doc_ctx0 m.input_model;
  print#string " ➜ ";
  xp_doc_model print ~ctx:doc_ctx0 m.output_model
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
  let dl_mi = dl_doc_model ~env:Any m.input_model in    
  (*let env_sig =
    signature_of_template m.input_pattern in *)
  let dl_mo = dl_doc_model ~env:m.input_model m.output_model in
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
  | Rinput (p,ri) -> print#string "IN "; xp_doc_path print p; print#string " ← "; xp_doc_refinement print ri
  | Routput (p,ro) -> print#string "OUT "; xp_doc_path print p; print#string " ← "; xp_doc_refinement print ro
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

