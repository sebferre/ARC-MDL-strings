
(* parameters *)

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

let alpha = def_param "alpha" 10. string_of_float
let max_nb_parse = def_param "max_nb_parse" 256 string_of_int (* max nb of considered doc parses *)
let max_nb_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)


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

let rec list_update (f : 'a -> 'a) (i : int) : 'a list -> 'a list = function
  | [] -> raise Not_found
  | x::l ->
     if i = 0
     then f x :: l
     else x :: list_update f (i-1) l
  
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

type row_path =
  | Col of int (* column index *) * cell_path
and cell_path =
  | ThisDoc
  | Left of cell_path
  | Middle of token_path
  | Right of cell_path
and token_path =
  | ThisToken

type row_ctx = row_path -> row_path
type cell_ctx = cell_path -> row_path
type token_ctx = token_path -> row_path

let ctx0 = (fun p -> p)

type var = row_path
type expr = var Expr.expr (* using doc_paths as vars *)
        
type row_model = cell_model list
and cell_model =
  | Nil
  | Any
  | Factor of cell_model * token_model * cell_model
and token_model =
  | Const of string
  | Regex of regex_model
  | Expr of expr
and regex_model =
  | Alphas (* [-A-Za-z0-9_]+ *)
  | Nums (* [0-9]+\([.][0-9]*\)? *)
  | Letters (* [A-Za-z-]+ *)

let row_model0 (row_size : int) : row_model = List.init row_size (fun _ -> Any)

type row_data = cell_data list
and cell_data =
  | DNil
  | DAny of string
  | DFactor of cell_data * token_data * cell_data
and token_data =
  | DToken of string
            
let row_data0 (row_size : int) = List.init row_size (fun _ -> DNil)

type env = row_data

(* printing *)

let xp_string (print : Xprint.t) (s : string) =
  print#string "<pre class=\"inline\">";
  print#string s;
  print#string "</pre>"
let pp_string = Xprint.to_stdout xp_string

let rec id_of_row_path (p : row_path) : string =
  match p with
  | Col (i,cp) ->
     let j, tp_opt = number_of_cell_path cp in
     let id_cell = String.make 1 (Char.chr (Char.code 'A' + i)) ^ string_of_int j in
     match tp_opt with
     | None -> id_cell
     | Some ThisToken -> id_cell ^ "#"  
and number_of_cell_path (p : cell_path) : int * token_path option =
  (* mapping cell_paths to unique integers + residual token_path *)
  let rec aux power2 acc = function
    | ThisDoc -> power2 + acc, None
    | Left p1 -> aux (2 * power2) acc p1
    | Middle p1 -> power2 + acc, Some p1
    | Right p1 -> aux (2 * power2) (power2 + acc) p1
  in
  aux 1 0 p
              
let xp_row_path (print : Xprint.t) (p : row_path) =
  let id = id_of_row_path p in
  print#string "<span class=\"model-path\">";
  print#string id;
  print#string "</span>"
(*  | ThisDoc -> print#string "."
  | Left p1 -> print#string "0"; xp_doc_path print p1
  | Middle p1 -> print#string "@"; xp_token_path print p1
  | Right p1 -> print#string "1"; xp_doc_path print p1
and xp_token_path print = function
  | ThisToken -> () *)
let pp_row_path = Xprint.to_stdout xp_row_path

let rec xp_row_model (print : Xprint.t) ?(ctx : row_ctx option) lm =
  List.iteri
    (fun i m ->
      let ctx_cell = ctx |> Option.map (fun ctx -> (fun p -> ctx (Col (i,p)))) in
      if i > 0 then print#string "</br>";
      xp_cell_model print ?ctx:ctx_cell m)
    lm
and xp_cell_model (print : Xprint.t) ?(ctx : cell_ctx option) = function
  | Nil -> ()
  | Any ->
     print#string "<span class=\"model-any\">*</span>"
  | Factor (l,t,r) ->
     let ctx_l = ctx |> Option.map (fun ctx -> (fun p -> ctx (Left p))) in
     let ctx_t = ctx |> Option.map (fun ctx -> (fun p -> ctx (Middle p))) in
     let ctx_r = ctx |> Option.map (fun ctx -> (fun p -> ctx (Right p))) in
     print#string "<span class=\"model-factor\">";
     xp_cell_model print ?ctx:ctx_l l;
     xp_token_model print ?ctx:ctx_t t;
     xp_cell_model print ?ctx:ctx_r r;
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
     p_opt |> Option.iter (fun p -> xp_row_path print p);
     print#string " : ";
     xp_regex_model print re;
     print#string "</span>"
  | Expr e ->
     print#string "<span class=\"model-expr\">";
     Expr.xp_expr xp_row_path print e;
     print#string "</span>"
and xp_regex_model print = function
  | Alphas -> print#string "Alphas"
  | Nums -> print#string "Digits"
  | Letters -> print#string "Letters"
let pp_row_model m = Xprint.to_stdout (xp_row_model ~ctx:ctx0) m
let string_of_row_model m = Xprint.to_string (xp_row_model ~ctx:ctx0) m
                    
let rec xp_row_data (print : Xprint.t) ld =
  List.iteri
    (fun i d ->
      if i > 0 then print#string "</br>";
      xp_cell_data print d)
    ld
and xp_cell_data (print : Xprint.t) = function
  | DNil -> ()
  | DAny s ->
     print#string "<span class=\"data-any\">";
     xp_string print s;
     print#string "</span>"
  | DFactor (l,t,r) ->
     xp_cell_data print l;
     xp_token_data print t;
     xp_cell_data print r
and xp_token_data print = function
  | DToken s ->
     print#string "<span class=\"data-token\">";
     xp_string print s;
     print#string "</span>"
let pp_row_data = Xprint.to_stdout xp_row_data
let string_of_row_data = Xprint.to_string xp_row_data

                       
(* get and apply *)

let rec row_of_row_data (ld : row_data) : string list =
  List.map cell_of_cell_data ld
and cell_of_cell_data : cell_data -> string = function
  | DNil -> ""
  | DAny s -> s
  | DFactor (l,t,r) -> cell_of_cell_data l
                       ^ token_of_token_data t
                       ^ cell_of_cell_data r
and token_of_token_data : token_data -> string = function
  | DToken s -> s

let rec cell_data_length : cell_data -> int = function
  | DNil -> 0
  | DAny s -> String.length s
  | DFactor (l,t,r) -> cell_data_length l + token_data_length t + cell_data_length r
and token_data_length = function
  | DToken s -> String.length s
              
let rec row_find (p : row_path) (ld : row_data) : Expr.value result =
  match p with
  | Col (i,p) ->
     let d = try List.nth ld i with _ -> assert false in
     cell_find p d
and cell_find (p : cell_path) (d : cell_data) : Expr.value result =
  match p, d with
  | ThisDoc, _ -> Result.Ok (`String (cell_of_cell_data d))
  | Left p1, DFactor (l,_,_) -> cell_find p1 l
  | Middle p1, DFactor (_,t,_) -> token_find p1 t
  | Right p1, DFactor (_,_,r) -> cell_find p1 r
  | _ -> assert false
and token_find p d =
  match p, d with
  | ThisToken, _ -> Result.Ok (`String (token_of_token_data d))

let row_bindings (ld : row_data) : (row_path * Expr.value) list =
  let rec aux_row ctx ld acc =
    let _, acc =
      List.fold_left
        (fun (i,acc) d ->
          i+1, aux_cell (fun p -> ctx (Col (i,p))) d acc)
        (0,acc) ld in
    acc
  and aux_cell ctx d acc =
    match d with
    | DNil -> acc
    | DAny _ -> acc
    | DFactor (l,t,r) ->
       let acc = aux_cell (fun p -> ctx (Right p)) r acc in
       let acc = aux_cell (fun p -> ctx (Left p)) l acc in
       let acc = aux_token (fun p -> ctx (Middle p)) t acc in
       acc
  and aux_token ctx t acc =
    match t with
    | DToken s -> (ctx ThisToken, `String s) :: acc
  in
  aux_row (fun p -> p) ld []

let rec row_apply (lm : row_model) (env : env) : row_model result =
  list_map_result
    (fun m -> cell_apply m env)
    lm    
and cell_apply (m : cell_model) (env : env) : cell_model result =
  match m with
  | Nil -> Result.Ok Nil
  | Any -> Result.Ok Any
  | Factor (l,t,r) ->
     let| l' = cell_apply l env in
     let| t' = token_apply t env in
     let| r' = cell_apply r env in
     Result.Ok (Factor (l',t',r'))
and token_apply t env : token_model result =
  match t with
  | Const s -> Result.Ok (Const s)
  | Regex re -> Result.Ok (Regex re)
  | Expr e ->
     let| v = Expr.eval (fun p -> row_find p env) e in
     let| s = Expr.string_of_value v in     
     Result.Ok (Const s)

     
(* generate *)

let rec row_generate (lm : row_model) : row_data =
  List.map cell_generate lm
and cell_generate : cell_model -> cell_data = function
  | Nil -> DNil
  | Any -> DAny "..."
  | Factor (l,t,r) -> DFactor (cell_generate l, token_generate t, cell_generate r)
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

type ('a,'b) parseur = 'a -> 'b Myseq.t
             
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

let token_parse (m : token_model) : (string, string * token_data * string) parseur =
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

let rec cell_parse (m : cell_model) : (string, cell_data) parseur =
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
       let* dl = cell_parse l sl in
       let* dr = cell_parse r sr in
       Myseq.return (DFactor (dl, dt, dr))

let row_parse (lm : row_model) : (string list, row_data) parseur =
  fun ls ->
  Myseq.product_fair (List.map2 cell_parse lm ls)


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
  
let rec dl_row_model ~(env : row_model) (lm : row_model) : dl =
  let nb_env_paths = dl_row_model_env_stats env in (* useful view on the environment model *)
  Mdl.sum (* cell models are independent *)
    lm
    (fun m -> dl_cell_model ~nb_env_paths m)
and dl_row_model_env_stats (lm : row_model) : int =
  List.fold_left
    (fun res m -> res + dl_cell_model_env_stats m)
    0 lm
and dl_cell_model_env_stats : cell_model -> int = function
  (* counting paths to tokens *)
  | Nil -> 0
  | Any -> 0
  | Factor (l,t,r) ->
     dl_cell_model_env_stats l
     + 1
     + dl_cell_model_env_stats r
and dl_cell_model ~nb_env_paths (m : cell_model) : dl =
  let nb_any, nb_factor = dl_cell_model_stats m in
  Mdl.Code.universal_int_star nb_factor (* encoding total nb of factors/tokens *)
  +. Mdl.Code.universal_int_star nb_any (* encoding total nb of Any's, to favor Nil's *)
  (* +. Mdl.Code.uniform (nb_factor + 2) (* alternate encoding, based on bound for nb_any *) *)
  +. dl_cell_model_aux ~nb_env_paths nb_any nb_factor m
and dl_cell_model_aux ~nb_env_paths nb_any nb_factor = function
  | Nil ->
     assert (nb_any = 0 && nb_factor = 0);
     0.
  | Any ->
     assert (nb_any = 1 && nb_factor = 0);
     0.
  | Factor (l,t,r) ->
     let na_l, nf_l = dl_cell_model_stats l in
     let na_r, nf_r = dl_cell_model_stats r in
     assert (na_l + na_r = nb_any);
     assert (nf_l + 1 + nf_r = nb_factor);
     Mdl.Code.uniform (nb_factor) (* encoding split of remaining Factor's *)
     +. Mdl.Code.uniform (* encoding split of Any's, values of na_l, na_r *)
          (min nb_any (nf_l + 1) (* maximal possible value for na_l *)
           - max 0 (nb_any - (nf_r + 1)) (* minimal possible value for na_l *)
           + 1)
     +. dl_cell_model_aux ~nb_env_paths na_l nf_l l
     +. dl_token_model ~nb_env_paths t
     +. dl_cell_model_aux ~nb_env_paths na_r nf_r r
and dl_cell_model_stats : cell_model -> int * int = function
  (* counting Any and Factor inside doc_model *)
  | Nil -> 0, 0
  | Any -> 1, 0
  | Factor (l,t,r) ->
     let na_l, nf_l = dl_cell_model_stats l in
     let na_r, nf_r = dl_cell_model_stats r in
     na_l + na_r, nf_l + 1 + nf_r
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

let rec row_encoder (lm : row_model) : row_data encoder =
  (* assuming that the passed data matches the model *)
  let l_enc_m = List.map cell_encoder lm in
  fun ld ->
  List.fold_left2
    (fun res enc_m d -> res +. enc_m d)
    0. l_enc_m ld
and cell_encoder (m : cell_model) : cell_data encoder =
  let enc_m = cell_encoder_aux m in
  fun d ->
  let n = cell_data_length d in
  Mdl.Code.universal_int_star n
  +. enc_m d
and cell_encoder_aux : cell_model -> cell_data encoder = function
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
       let range_l = cell_encoder_range l in
       let range_t = token_encoder_range t in
       let range_r = cell_encoder_range r in
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
     let enc_l = cell_encoder_aux l in
     let enc_t = token_encoder t in
     let enc_r = cell_encoder_aux r in
     (function
      | DFactor (dl,dt,dr) ->
         let nl_nt_nr = cell_data_length dl, token_data_length dt, cell_data_length dr in
         enc_split nl_nt_nr +. enc_l dl +. enc_t dt +. enc_r dr
      | _ -> assert false)
and cell_encoder_range : cell_model -> Range.t = function
  (* min-max length range for doc_models *)
  | Nil -> Range.make_exact 0
  | Any -> Range.make_open 0
  | Factor (l,t,r) ->
     let range_l = cell_encoder_range l in
     let range_t = token_encoder_range t in
     let range_r = cell_encoder_range r in
     Range.sum [range_l; range_t; range_r]
and token_encoder_range : token_model -> Range.t = function
  | Const s -> Range.make_exact (String.length s)
  | Regex _ -> Range.make_open 1
  | Expr _ -> Range.make_open 1
and token_encoder : token_model -> token_data encoder = function
  | Const _ -> (function DToken _ -> 0.)
  | Regex re ->
     let enc_re = regex_encoder re in
     (function DToken s -> enc_re s)
  | Expr _ -> (fun _ -> 0.) (* nothing to code, evaluated *)
and regex_encoder : regex_model -> string encoder = function
  | Alphas -> dl_char_plus make_alpha_prequential
  | Nums -> dl_char_plus make_num_prequential
  | Letters -> dl_char_plus make_letter_prequential


(* reading *)

type 'a read = env * 'a * dl
type row_read = env * row_data * dl
type cell_read = env * cell_data * dl
type token_read = env * token_data * dl

let limit_dl (f_dl : 'a -> dl) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x0::_ ->
     let dl0 = f_dl x0 in
     let min_dl = !max_parse_dl_factor *. dl0 in
     List.filter (fun x -> f_dl x <= min_dl) l

let read_row ~(env : env) (m0 : row_model) (s : string list) : row_read list result =
  Common.prof "Model.read_row" (fun () ->
  let| m = row_apply m0 env in (* reducing expressions *)
  let parses =
    let* data = row_parse m s in
    let dl = (* QUICK *)
      let dl_data = row_encoder m data in
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
      |> (fun l -> Common.sub_list l 0 !max_nb_reads)
      |> limit_dl (fun (_,_,dl) -> dl)
      |> List.mapi (fun rank (env,data,dl) ->
             let dl = dl +. Mdl.Code.universal_int_star rank in (* to penalize later parses, in case of equivalent parses *)
             (env, data, dl)) in
    Result.Ok best_parses)

type rows_reads =
  { dl_m : dl; (* DL of the model *)
    reads : row_read list list; (* outer list over docs, inner list over parses, sorted in increasing DL *)
  }

(* writing *)

let write_row ~(env : env) (m : row_model) : (string list, exn) Result.t = Common.prof "Model.write_doc" (fun () ->
  let| m' = row_apply m env in
  let d = row_generate m' in
  let ls = row_of_row_data d in
  Result.Ok ls)
             
(* refinements *)

type cell_refinement =
  | RCell of cell_model (* cell specialization *)
  | RToken of token_model (* token specialization *)

let xp_cell_refinement (print : Xprint.t) = function
  | RCell Nil -> print#string "<span class=\"model-nil\">∅</span>"
  | RCell cell -> xp_cell_model print cell
  | RToken tok -> xp_token_model print tok
let pp_cell_refinement = Xprint.to_stdout xp_cell_refinement
           
let apply_cell_refinement (r : cell_refinement) (p : row_path) (lm : row_model) : row_model =
  let rec aux_row p lm =
    match p with
    | Col (i,p) ->
       (try list_update (aux_cell p) i lm
        with Not_found -> assert false)
  and aux_cell p m =
    match p, m, r with
    | ThisDoc, Any, RCell cell -> cell
    | Left p1, Factor (l,t,r), _ -> Factor (aux_cell p1 l, t, r)
    | Middle p1, Factor (l, t, r), _ -> Factor (l, aux_token p1 t, r)
    | Right p1, Factor (l, t, r), _ -> Factor (l, t, aux_cell p1 r)
    | _ -> assert false
  and aux_token p m =
    match p, m, r with
    | ThisToken, Regex _, RToken tok -> tok
    | _ -> assert false
  in
  aux_row p lm

let map_reads (f : 'a -> 'b) (reads : 'a list list) : 'b list list  =
  List.map
    (fun example_reads ->
      List.map f example_reads)
    reads

let inter_union_reads
      (f : 'read -> ('r * 'd) list)
      (reads : 'read list list)
    : ('r, ('read * 'd) list) Mymap.t =
  (* given a function extracting refinement information [type 'r] from each read,
     return a set of such ref-info, each mapped to the dl-shortest reads supporting it, along with new data *)
  let process_example reads =
    List.fold_left
      (fun res read ->
        List.fold_left
          (fun res (r,data') ->
            if Mymap.mem r res
            then res
            else Mymap.add r (read,data') res)
          res (f read))
      Mymap.empty reads in
  match reads with
  | [] -> assert false
  | example0_reads :: other_reads ->
     let res0 =
       process_example example0_reads
       |> Mymap.map (fun best_read -> [best_read]) in
     List.fold_left
       (fun res exampleI_reads ->
         let resI = process_example exampleI_reads in
         Mymap.merge
           (fun r best_reads_opt best_readI_opt ->
             match best_reads_opt, best_readI_opt with
             | Some best_reads, Some best_readI -> Some (best_readI::best_reads)
             | _ -> None)
           res resI)
       res0 other_reads

let row_refinements ~nb_env_paths (lm : row_model) ?(dl_M : dl = 0.) (rsr : rows_reads) : (row_path * cell_refinement * row_model) Myseq.t =
  (* NOTE: dl_M does not matter for ranking because invariant of parsing and refinement *)
  let reads = (* replacing env's with expression index's over them *)
    map_reads
      (fun (env,data,dl) ->
        (Expr.make_index (row_bindings env), data, dl))
      rsr.reads in
  let rec fold_row lm reads =
    Myseq.interleave
      (List.mapi
         (fun i m ->
           let m_reads =
             map_reads
               (fun (env, ld, dl) ->
                 let d_i = try List.nth ld i with _ -> assert false in
                 (env, d_i, dl))
               reads in
           fold_cell m m_reads
           |> Myseq.map (fun (p,r,dl') -> (Col (i,p), r, dl')))
         lm)
  and fold_cell m reads =
    match m with
    | Nil -> Myseq.empty
    | Any ->
       let encoder_m = cell_encoder m in
       let dl_m = dl_cell_model ~nb_env_paths m in
       let r_best_reads : ([`IsNil | `RE of regex_model | `CommonStr of string], _) Mymap.t =
         inter_union_reads
           (fun (_,data,_) ->
             let s = cell_of_cell_data data in
             let rs = if s = "" then [(`IsNil, DNil)] else [] in
             let rs =
               if s <> ""
               then
                 List.fold_left
                   (fun rs re ->
                     let best_len, (sl, data', sr as best_slice) =
                       Myseq.fold_left
                         (fun (best_len, best_slide as best) (_, data', _ as slice) ->
                           let len = token_data_length data' in
                           if len > best_len
                           then (len, slice)
                           else best)
                         (0, ("", DToken "", "")) (token_parse (Regex re) s) in
                     if best_len > 0
                     then (`RE re, DFactor (DAny sl, data', DAny sr)) :: rs
                     else rs)
                   rs [Alphas; Nums; Letters]
               else rs in
             let rs =
               if s <> ""
               then (`CommonStr s, DFactor (DNil, DToken s, DNil)) :: rs
               else rs in
             rs)
           reads in
       let* r_info, best_reads = Mymap.to_seq r_best_reads in
       let m' =
         match r_info with
         | `IsNil -> Nil
         | `RE re -> Factor (Any, Regex re, Any)
         | `CommonStr s -> Factor (Nil, Const s, Nil) in
       let r = RCell m' in
       let encoder_m' = cell_encoder m' in
       let dl_m' = dl_cell_model ~nb_env_paths m' in
       let dl' =
         dl_M -. dl_m +. dl_m'
         +. Mdl.sum best_reads
              (fun ((_,data,dl_D), data') ->
                dl_D -. encoder_m data +. encoder_m' data') in
       Myseq.return (ThisDoc, r, dl')
    | Factor (l,t,r) ->
       Myseq.concat
         [ fold_token t
             (map_reads
                (function
                 | (idx, DFactor (_,dt,_), l) -> (idx, dt, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r,dl') -> Middle p, r, dl');
           fold_cell l
             (map_reads
                (function
                 | (idx, DFactor (dl,_,_), l) -> (idx, dl, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r,dl') -> Left p, r, dl');
           fold_cell r
             (map_reads
                (function
                 | (idx, DFactor (_,_,dr), l) -> (idx, dr, l)
                 | _ -> assert false)
                reads)
           |> Myseq.map (fun (p,r,dl') -> Right p, r, dl') ]
  and fold_token (m : token_model) reads =
    match m with
    | Const s -> Myseq.empty
    | Regex re ->
       let encoder_m = token_encoder m in
       let dl_m = dl_token_model ~nb_env_paths m in
       let re'_candidates =
         match re with
         | Alphas -> [Nums; Letters]
         | _ -> [] in
       let r_best_reads : ([`CommonStr of string | `RE of regex_model | `Expr of expr], _) Mymap.t =
         inter_union_reads
           (fun (idx,data,_) ->
             let s = token_of_token_data data in
             let es = Expr.index_lookup (`String s) idx in
             let rs = if s <> "" then [(`CommonStr s, DToken s)] else [] in
             let rs =
               List.fold_left
                 (fun rs re' ->
                   if regexp_match_full (re_of_regexp re') s
                   then (`RE re', DToken s) :: rs
                   else rs)
                 rs re'_candidates in
             let rs =
               Myseq.fold_left
                 (fun rs e -> (`Expr e, DToken s) :: rs)
                 rs (Expr.exprset_to_seq es) in
             rs)
           reads in
       let* r_info, best_reads = Mymap.to_seq r_best_reads in
       let m' =
         match r_info with
         | `CommonStr s -> Const s
         | `RE re' -> Regex re'
         | `Expr e -> Expr e in
       let r = RToken m' in
       let encoder_m' = token_encoder m' in
       let dl_m' = dl_token_model ~nb_env_paths m' in
       let dl' =
         dl_M -. dl_m +. dl_m'
         +. Mdl.sum best_reads
              (fun ((_,data,dl_D), data') ->
                dl_D -. encoder_m data +. encoder_m' data') in         
       Myseq.return (ThisToken, r, dl')
    | Expr e -> Myseq.empty
  in
  let* p, r, dl' =
    fold_row lm reads
    |> Myseq.sort (fun (_,_,dl1) (_,_,dl2) -> dl_compare dl1 dl2)
    |> Myseq.slice ~limit:!max_refinements in
  let m' = apply_cell_refinement r p lm in
  Myseq.return (p, r, m')


(* examples  / pairs *)
             
type model =
  { input_model : row_model; (* no reference *)
    output_model : row_model
  }

let init_model (t : Task.task) =
  { input_model = row_model0 (Task.input_row_size t);
    output_model = row_model0 (Task.output_row_size t) }

let xp_model (print : Xprint.t) (m : model) =
  xp_row_model print ~ctx:ctx0 m.input_model;
  print#string " ➜ ";
  xp_row_model print ~ctx:ctx0 m.output_model
let pp_model = Xprint.to_stdout xp_model
let string_of_model = Xprint.to_string xp_model

             
type pairs_reads = (* result of reading a list of pairs of grids *)
  { dl_mi : dl; (* input model DL *)
    dl_mo : dl; (* output model DL *)
    inputs_reads : row_read list list; (* outer list over example inputs, inner list over parses *)
    reads : (row_read * row_read * dl) list list; (* outer list over examples, inner list over parses, sorted in increasing DL *)
  }

let read_pairs ?(env = row_data0 0) (m : model) (pairs : Task.pair list) : pairs_reads result =
  Common.prof "Model.read_pairs" (fun () ->
  (* takes model, input env+docs, output docs *)
  let dl_mi = dl_row_model ~env:[] m.input_model in    
  let dl_mo = dl_row_model ~env:m.input_model m.output_model in
  let| inputs_reads_reads =
    pairs
    |> list_map_result
         (fun {input; output} ->
           let| input_reads =
             read_row ~env m.input_model input in (* no diff allowed during training *)
           let| pair_reads = 
             let+|+ (envi,ddi,dli as ri) = Result.Ok input_reads in      
             let+|+ (envo,ddo,dlo as ro) =
               read_row ~env:ddi m.output_model output in
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

let split_pairs_read (prs : pairs_reads) : rows_reads * rows_reads =
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


let apply_model ?(env = []) (m : model) (row_i : string list) : ((row_data * string list) list, exn) Result.t =
  Common.prof "Model.apply_model" (fun () ->
  let+|+ _, di, _ =
    read_row ~env m.input_model row_i in
  let| row_o =
    write_row ~env:di m.output_model in
  Result.Ok [(di, row_o)])

  
type refinement =
  | RInit
  | Rinput of row_path * cell_refinement
  | Routput of row_path * cell_refinement

let xp_refinement (print : Xprint.t) = function
  | RInit -> print#string "init"
  | Rinput (p,ri) -> print#string "IN "; xp_row_path print p; print#string " ← "; xp_cell_refinement print ri
  | Routput (p,ro) -> print#string "OUT "; xp_row_path print p; print#string " ← "; xp_cell_refinement print ro
let pp_refinement = Xprint.to_stdout xp_refinement
let string_of_refinement = Xprint.to_string xp_refinement

let apply_refinement (r : refinement) (m : model) : (refinement * model) result =
  match r with
  | RInit -> Result.Error (Failure "apply_refinement")
  | Rinput (p,ri) ->
     Result.Ok (r, {m with input_model = apply_cell_refinement ri p m.input_model})
  | Routput (p,ro) ->
     Result.Ok (r, {m with output_model = apply_cell_refinement ro p m.output_model})

let model_refinements (last_r : refinement) (m : model) (dsri : rows_reads) (dsro : rows_reads) : (refinement * model) Myseq.t =
  Myseq.concat (* TODO: rather order by estimated dl *)
    [ (let* p, ri, mi = row_refinements ~nb_env_paths:0 m.input_model dsri in
       Myseq.return (Rinput (p,ri), {m with input_model = mi}));
      (let* p, ro, mo = row_refinements ~nb_env_paths:(dl_row_model_env_stats m.input_model) m.output_model dsro in
       Myseq.return (Routput (p,ro), {m with output_model = mo})) ]

  
(* learning *)

let learn_model
      ?(verbose = false)
      ?(pause = 0.)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((refinement * model) * (pairs_reads * rows_reads * rows_reads) * dl) list * bool
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
          pp_row_data d_i; print_newline ();
          pp_row_data d_o; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = model_refinements r m gsri gsro in
      refs))

