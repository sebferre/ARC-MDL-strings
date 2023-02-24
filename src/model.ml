
(* parameters *)

let def_param name v to_str =
  Printf.printf "## %s = %s\n" name (to_str v);
  ref v

let alpha = def_param "alpha" 1. (* TEST *) string_of_float
let max_nb_parse = def_param "max_nb_parse" 256 string_of_int (* max nb of considered doc parses *)
let max_nb_reads = def_param "max_nb_doc_reads" 3 string_of_int (* max nb of selected doc reads, passed to the next stage *)
let max_parse_dl_factor = def_param "max_parse_dl_factor" 3. string_of_float (* compared to best parse, how much longer alternative parses can be *)
let max_refinements = def_param "max_refinements" 100 string_of_int (* max nb of considered refinements *)

open Utilities

(* types of model, data according to a model... *)

open Task

type row_path =
  | Col of int (* column index *) * cell_path
and cell_path =
  | ThisDoc
  | Left of cell_path
  | Middle of token_path
  | Right of cell_path
  | Index of int * cell_path
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
  | Empty (* empty language *)
  | Nil (* epsilon *)
  | Any
  | Factor of cell_model * token_model * cell_model
  | Alt of cell_model * cell_model (* Opt c = Alt (c,Nil) *)
and token_model =
  | Const of string
  | Regex of regex_model
  | Expr of expr
and regex_model =
  | Content (* word, operators *)
  | Word (* letters, digits, _ *)
  | Letters (* [A-Za-z]+ *)
  | Decimal (* digits (. digits)? *)
  | Digits (* [0-9]+ *)
  | Separators (* spaces, puncts, quotes, brackets *)
  | Spaces (* [ \n\t\r\f]+ *)

let nb_regex = 7
  
let special_consts =
  [ (* operators *)
    "#"; "$"; "%"; "&"; "*"; "+"; "-"; "/"; "<"; "="; ">"; "@"; "\\"; "^"; "|"; "~";
    (* punctuation *)
    "!"; ","; "."; ":"; ";"; "?";
    (* quotes *)
    "\""; "'"; "`";
    (* brackets *)
    "("; ")"; "["; "]"; "{"; "}" ]

let row_model0 (row_size : int) : row_model = List.init row_size (fun _ -> Any)

type row_data = cell_data list
and cell_data =
  | DNil
  | DAny of string
  | DFactor of cell_data * token_data * cell_data
  | DAlt of int * cell_data (* DOpt (Some d) = DAlt (1,d); DOpt None = DAlt (2, DNil) *)
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

let xp_brackets (print : Xprint.t) (xp : Xprint.t -> unit) : unit =
  print#string "<div class=\"model-brackets\">";
  xp print;
  print#string "</div>"
              
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
    | Index (1,p1) -> aux (2 * power2) acc p1
    | Index (2,p1) -> aux (2 * power2) (power2 + acc) p1
    | Index _ -> assert false
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

let xp_cell_brackets ~prio_ctx ~prio print xp =
  if prio <= prio_ctx
  then xp print
  else xp_brackets print xp

let rec xp_row_model (print : Xprint.t) ?(ctx : row_ctx option) lm =
  List.iteri
    (fun i m ->
      let ctx_cell = ctx |> Option.map (fun ctx -> (fun p -> ctx (Col (i,p)))) in
      if i > 0 then print#string "</br>";
      xp_cell_model ~prio_ctx:2 print ?ctx:ctx_cell m)
    lm
and xp_cell_model ?(prio_ctx = 2) (print : Xprint.t) ?(ctx : cell_ctx option) = function
  | Empty ->
     print#string "∅"
  | Nil -> ()
  | Any ->
     print#string "<span class=\"model-any\">*</span>"
  | Factor (l,t,r) ->
     let ctx_l = ctx |> Option.map (fun ctx -> (fun p -> ctx (Left p))) in
     let ctx_t = ctx |> Option.map (fun ctx -> (fun p -> ctx (Middle p))) in
     let ctx_r = ctx |> Option.map (fun ctx -> (fun p -> ctx (Right p))) in
     xp_cell_brackets ~prio_ctx ~prio:0 print
       (fun print ->
         print#string "<div class=\"model-factor\">";
         xp_cell_model ~prio_ctx:0 print ?ctx:ctx_l l;
         xp_token_model print ?ctx:ctx_t t;
         xp_cell_model ~prio_ctx:0 print ?ctx:ctx_r r;
         print#string "</div>")
  | Alt (c, Nil) (* Opt c *) ->
     let ctx_1 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (1, p)))) in
     xp_cell_brackets ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"model-opt\">";
         xp_cell_model ~prio_ctx:1 print ?ctx:ctx_1 c;
         print#string " <span class=\"model-meta-operator\">?</span>";
         print#string "</div>")
  | Alt (Nil, c) (* Opt c *) ->
     let ctx_2 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (2, p)))) in     
     xp_cell_brackets ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"model-opt\">";
         xp_cell_model ~prio_ctx:1 print ?ctx:ctx_2 c;
         print#string " <span class=\"model-meta-operator\">?</span>";
         print#string "</div>")
  | Alt (c1,c2) ->
     let ctx_1 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (1, p)))) in
     let ctx_2 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (2, p)))) in
     xp_cell_brackets ~prio_ctx ~prio:2 print
       (fun print ->
         print#string "<div class=\"model-alt\">";
         xp_cell_model ~prio_ctx:2 print ?ctx:ctx_1 c1;
         print#string " <span class=\"model-meta-operator\">|</span> ";
         xp_cell_model ~prio_ctx:2 print ?ctx:ctx_2 c2;
         print#string "</div>")
and xp_token_model print ?(ctx : token_ctx option) = function
  | Const s ->
     let p_opt = ctx |> Option.map (fun ctx -> ctx ThisToken) in
     print#string "<span class=\"model-const\"";
     p_opt |> Option.iter (* print path as tooltip *)
                (fun p ->
                  print#string " title=\"";
                  print#string (id_of_row_path p);
                  print#string "\"");
     print#string ">";
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
  | Content -> print#string "Content"
  | Word -> print#string "Word"
  | Letters -> print#string "Letters"
  | Decimal -> print#string "Decimal"
  | Digits -> print#string "Digits"
  | Separators -> print#string "Separators"
  | Spaces -> print#string "Spaces"
let pp_row_model m = Xprint.to_stdout (xp_row_model ~ctx:ctx0) m
let string_of_row_model m = Xprint.to_string (xp_row_model ~ctx:ctx0) m
                    
let rec xp_row_data (print : Xprint.t) ld =
  List.iteri
    (fun i d ->
      if i > 0 then print#string "</br>";
      xp_cell_data print d)
    ld
and xp_cell_data ?(prio_ctx = 0) (print : Xprint.t) = function
  | DNil -> ()
  | DAny s ->
     print#string "<span class=\"data-any\">";
     xp_string print s;
     print#string "</span>"
  | DFactor (l,t,r) ->
     xp_cell_brackets ~prio_ctx ~prio:0 print
       (fun print ->
         print#string "<div class=\"data-factor\">";
         xp_cell_data ~prio_ctx:0 print l;
         xp_token_data print t;
         xp_cell_data ~prio_ctx:0 print r;
         print#string "</div>")
  | DAlt (i, DNil) ->
     xp_cell_brackets ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"data-opt\">ε</div>")
  | DAlt (i,c) -> (* TODO: find better repr than 1/2: to indicate valid branch *)
     xp_cell_brackets ~prio_ctx ~prio:2 print
       (fun print ->
         print#string "<div class=\"data-alt\">";
         (* print#int i; *)
         xp_cell_data ~prio_ctx:2 print c;
         print#string "</div>")
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
  | DAlt (i,c) -> cell_of_cell_data c
and token_of_token_data : token_data -> string = function
  | DToken s -> s

let rec cell_data_length : cell_data -> int = function
  | DNil -> 0
  | DAny s -> String.length s
  | DFactor (l,t,r) -> cell_data_length l + token_data_length t + cell_data_length r
  | DAlt (_,c) -> cell_data_length c
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
  | Index (i,p1), DAlt (i',c) ->
     if i = i'
     then cell_find p1 c
     else Result.Ok `Null
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
    | DAlt (i,c) ->
       let acc = aux_cell (fun p -> ctx (Index (i,p))) c acc in
       acc
  and aux_token ctx t acc =
    match t with
    | DToken s -> (ctx ThisToken, `String s) :: acc
  in
  aux_row (fun p -> p) ld []

exception NullExpr (* error for expressions that contains a null value *)
  
let rec row_apply (lm : row_model) (env : env) : row_model result =
  list_map_result
    (fun m -> cell_apply m env)
    lm    
and cell_apply (m : cell_model) (env : env) : cell_model result =
  match m with
  | Empty -> Result.Ok Empty
  | Nil -> Result.Ok Nil
  | Any -> Result.Ok Any
  | Factor (l,t,r) ->
     let| l' = cell_apply l env in
     let| t' = token_apply t env in
     let| r' = cell_apply r env in
     Result.Ok (Factor (l',t',r'))
  | Alt (c1,c2) ->
     let res1 = cell_apply c1 env in
     let res2 = cell_apply c2 env in
     (match res1, res2 with
      | Result.Ok c1', Result.Ok c2' -> Result.Ok (Alt (c1', c2'))
      | Result.Error NullExpr, Result.Ok c2' -> Result.Ok (Alt (Empty, c2'))
      | Result.Ok c1', Result.Error NullExpr -> Result.Ok (Alt (c1', Empty))
      | _ -> res1)
and token_apply t env : token_model result =
  match t with
  | Const s -> Result.Ok (Const s)
  | Regex re -> Result.Ok (Regex re)
  | Expr e ->
     let| v = Expr.eval (fun p -> row_find p env) e in
     (match v with
      | `String s -> Result.Ok (Const s)
      (* TODO: consider converting other values to strings *)
      | `Null -> Result.Error NullExpr
      | _ -> Result.Error (Invalid_argument "Model.token_apply: string expected as expression value"))

     
(* generate *)

let rec row_generate (lm : row_model) : row_data =
  List.map cell_generate lm
and cell_generate : cell_model -> cell_data = function
  | Empty -> assert false
  | Nil -> DNil
  | Any -> DAny "..."
  | Factor (l,t,r) -> DFactor (cell_generate l, token_generate t, cell_generate r)
  | Alt (c1,c2) -> (* TODO: make stochastic ? *)
     if c1 <> Empty then DAlt (1, cell_generate c1)
     else if c2 <> Empty then DAlt (2, cell_generate c2)
     else assert false
and token_generate : token_model -> token_data = function
  | Const s -> DToken s
  | Regex re -> DToken (regex_generate re)
  | Expr _ -> assert false
and regex_generate : regex_model -> string = function
  | Content -> "A-b_1&2"
  | Word -> "Ab_1"
  | Letters -> "Abc"
  | Decimal -> "12.3"
  | Digits -> "123"
  | Separators -> ", "
  | Spaces -> " "


(* parse *)

exception Parse_failure

type ('a,'b) parseur = 'a -> 'b Myseq.t

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

let chars_of_regex = function
  | Content -> chars_content
  | Word -> chars_word
  | Letters -> chars_letters
  | Decimal -> chars_decimal
  | Digits -> chars_digits
  | Separators -> chars_separators
  | Spaces -> chars_spaces
  
let re_content = Str.regexp "[A-Za-z_0-9#$%&*+/<=>@\\^|~-]+"
let re_word = Str.regexp "[A-Za-z_0-9]+"
let re_letters = Str.regexp "[A-Za-z]+"
let re_decimal = Str.regexp "[0-9]+\\([.][0-9]+\\)?"
let re_digits = Str.regexp "[0-9]+"
let re_separators = Str.regexp "[] \n\t\r!,.:;?\"'`()[{}]+"
let re_spaces = Str.regexp "[ \n\t\r]+"

let re_of_regex = function
  | Content -> re_content
  | Word -> re_word
  | Letters -> re_letters
  | Decimal -> re_decimal
  | Digits -> re_digits
  | Separators -> re_separators
  | Spaces -> re_spaces

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
    | Regex rm -> re_of_regex rm
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
  | Empty -> Myseq.empty
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
  | Alt (c1,c2) -> (* exclusive or, like if-then-else *)
     let seq1 = cell_parse c1 s in
     if Myseq.is_empty seq1
     then
       let* dc2 = cell_parse c2 s in
       Myseq.return (DAlt (2,dc2))
     else
       let* dc1 = seq1 in
       Myseq.return (DAlt (1,dc1))

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

let init_occs_of_regex = function
  | Content | Word | Letters -> Some ascii_init_occs
  | _ -> None
     
let dl_char_plus ?init_occs chars (s : string) : dl =
  (* using prequential code *)
  let pc = new Mdl.Code.prequential ?init_occs chars () in
  String.iter
    (fun c -> ignore (pc#code c))
    s;
  pc#cumulated_dl

let dl_string_ascii (s : string) : dl = dl_char_plus chars s

let dl_string_regex (re : regex_model) (s : string) : dl =
  let chars = chars_of_regex re in
  let init_occs = init_occs_of_regex re in
  dl_char_plus ?init_occs chars s
  
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
  | Empty -> 0
  | Nil -> 0
  | Any -> 0
  | Factor (l,t,r) ->
     dl_cell_model_env_stats l
     + 1
     + dl_cell_model_env_stats r
  | Alt (c1,c2) ->
     dl_cell_model_env_stats c1
     + dl_cell_model_env_stats c2
and dl_cell_model ~nb_env_paths (m : cell_model) : dl =
  let nb_any, nb_factor, nb_alt = dl_cell_model_stats m in
  Mdl.Code.universal_int_star nb_factor (* encoding total nb of factors/tokens *)
  +. Mdl.Code.universal_int_star nb_any (* encoding total nb of Any's, to favor Nil's *)
  (* +. Mdl.Code.uniform (nb_factor + 2) (* alternate encoding, based on bound for nb_any *) *)
  +. Mdl.Code.universal_int_star nb_alt (* encoding total nb of Alt's *)
  +. dl_cell_model_aux ~nb_env_paths nb_any nb_factor nb_alt m
and dl_cell_model_aux ~nb_env_paths nb_any nb_factor nb_alt = function
  | Empty -> assert false
  | Nil ->
     assert (nb_any = 0 && nb_factor = 0 && nb_alt = 0);
     0.
  | Any ->
     assert (nb_any = 1 && nb_factor = 0 && nb_alt = 0);
     0.
  | Factor (l,t,r) ->
     let na_l, nf_l, no_l = dl_cell_model_stats l in
     let na_r, nf_r, no_r = dl_cell_model_stats r in
     assert (na_l + na_r = nb_any);
     assert (nf_l + 1 + nf_r = nb_factor);
     assert (no_l + no_r = nb_alt);
     Mdl.Code.usage (float nb_factor /. float (nb_factor + nb_alt)) (* choice between Factor and Alt *)
     +. (* encoding split of remaining Factor's *)
       (let k = nb_factor in
         assert (k > 0);
         Mdl.Code.uniform k)
     +. (* encoding split of Alt's, values of no_l, no_r *)
       (let k = nb_alt + 1
          (* min nb_alt (na_l + nf_l) (* maximal possible no_l *)
          - max 0 (nb_alt - (na_r + nf_r)) (* minimal possible no_l *)
          + 1 *) in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. (* encoding split of Any's, values of na_l, na_r *)
       (let k = (* nb_any + 1 *) (* TODO: refine *)
          min nb_any (nf_l + no_l + 1) (* maximal possible value for na_l *)
          - max 0 (nb_any - (nf_r + no_r + 1)) (* minimal possible value for na_l *)
          + 1 in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. dl_cell_model_aux ~nb_env_paths na_l nf_l no_l l
     +. dl_token_model ~nb_env_paths t
     +. dl_cell_model_aux ~nb_env_paths na_r nf_r no_r r
  | Alt (c1,c2) ->
     let na_c1, nf_c1, no_c1 = dl_cell_model_stats c1 in
     let na_c2, nf_c2, no_c2 = dl_cell_model_stats c2 in
     assert (na_c1 + na_c2 = nb_any);
     assert (nf_c1 + nf_c2 = nb_factor);
     assert (no_c1 + no_c2 + 1 = nb_alt);
     Mdl.Code.usage (float nb_alt /. float (nb_alt + nb_factor)) (* choice between Factor and Alt *)
     +.  (* one factor left, encoding split of remaining Factor's *)
       (let k = nb_factor + 1 in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. (* encoding split of Alt's, values of no_l, no_r *)
       (let k = nb_alt
          (* min nb_alt (na_c1 + nf_c1) (* maximal possible no_l *)
          - max 0 (nb_alt - (na_c2 + nf_c2)) (* minimal possible no_l *)
          + 1 *) in
        assert (k > 0);
        Mdl.Code.uniform k) (* TODO: check for more constraints on nb of Alt's *)
     +. (* encoding split of Any's, values of na_l, na_r *)
       (let k = (* nb_any + 1 *)
          min nb_any (nf_c1 + no_c1 + 1) (* maximal possible value for na_l *)
          - max 0 (nb_any - (nf_c2 + no_c2 + 1)) (* minimal possible value for na_l *)
          + 1 in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. dl_cell_model_aux ~nb_env_paths na_c1 nf_c1 no_c1 c1
     +. dl_cell_model_aux ~nb_env_paths na_c2 nf_c2 no_c2 c2
and dl_cell_model_stats : cell_model -> int * int * int (* any, factor, alt *) = function
  (* counting Any and Factor and Alt inside doc_model *)
  | Empty -> 0, 0, 0 (* assert false *)
  | Nil -> 0, 0, 0
  | Any -> 1, 0, 0
  | Factor (l,t,r) ->
     let na_l, nf_l, no_l = dl_cell_model_stats l in
     let na_r, nf_r, no_r = dl_cell_model_stats r in
     na_l + na_r,
     nf_l + 1 + nf_r,
     no_l + no_r
  | Alt (c1,c2) ->
     let na_c1, nf_c1, no_c1 = dl_cell_model_stats c1 in
     let na_c2, nf_c2, no_c2 = dl_cell_model_stats c2 in
     na_c1 + na_c2,
     nf_c1 + nf_c2,
     no_c1 + no_c2 + 1     
and dl_token_model ~nb_env_paths : token_model -> dl = function
  | Const s ->
     Mdl.Code.usage 0.3
     +. Mdl.Code.universal_int_plus (String.length s)
     +. dl_string_ascii s
  | Regex rm ->
     Mdl.Code.usage 0.2
     +. Mdl.Code.uniform nb_regex
  | Expr e ->
     Mdl.Code.usage 0.5
     +. Expr.dl_expr
          (fun p ->
            let k = nb_env_paths in
            assert (k > 0);
            Mdl.Code.uniform k)
          e  

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
  | Empty -> assert false
  | Nil ->
     (function
      | DNil | DAny "" -> 0.
      | _ -> assert false)
  | Any ->
     (function
      | DAny s -> dl_string_ascii s
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
  | Alt (c1,c2) ->
     let dl_choice = Mdl.Code.uniform 2 in
     let enc_c1 = cell_encoder_aux c1 in
     let enc_c2 = cell_encoder_aux c2 in
     (function
      | DAlt (1,dc) -> dl_choice +. enc_c1 dc
      | DAlt (2,dc) -> dl_choice +. enc_c2 dc
      | _ -> assert false)
and cell_encoder_range : cell_model -> Range.t = function
  (* min-max length range for doc_models *)
  | Empty -> assert false
  | Nil -> Range.make_exact 0
  | Any -> Range.make_open 0
  | Factor (l,t,r) ->
     let range_l = cell_encoder_range l in
     let range_t = token_encoder_range t in
     let range_r = cell_encoder_range r in
     Range.sum [range_l; range_t; range_r]
  | Alt (c1,c2) ->
     let range_c1 = cell_encoder_range c1 in
     let range_c2 = cell_encoder_range c2 in
     Range.union range_c1 range_c2
and token_encoder_range : token_model -> Range.t = function
  | Const s -> Range.make_exact (String.length s)
  | Regex _ -> Range.make_open 1
  | Expr _ -> Range.make_open 1
and token_encoder : token_model -> token_data encoder = function
  | Const _ -> (function DToken _ -> 0.)
  | Regex rm ->
     let enc_rm = dl_string_regex rm in
     (function DToken s -> enc_rm s)
  | Expr _ -> (fun _ -> 0.) (* nothing to code, evaluated *)


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
      let dl_data = row_encoder m0 data in
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
  | RCell of int (* support *) * cell_model (* cell specialization *)
  | RToken of token_model (* token specialization *)

let xp_support (print : Xprint.t) (supp : int) =
  print#string " ("; print#int supp; print#string ")"

let xp_cell_refinement (print : Xprint.t) = function
  | RCell (supp,Nil) -> print#string "<span class=\"model-nil\">ε</span>"; xp_support print supp
  | RCell (supp,cell) -> xp_cell_model ~prio_ctx:2 print cell; xp_support print supp
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
    | ThisDoc, Any, RCell (_,cell) -> cell
    | Left p1, Factor (l,t,r), _ -> Factor (aux_cell p1 l, t, r)
    | Middle p1, Factor (l, t, r), _ -> Factor (l, aux_token p1 t, r)
    | Right p1, Factor (l, t, r), _ -> Factor (l, t, aux_cell p1 r)
    | Index (1,p1), Alt (c1, c2), _ -> Alt (aux_cell p1 c1, c2)
    | Index (2,p1), Alt (c1, c2), _ -> Alt (c1, aux_cell p1 c2)
    | _ -> assert false
  and aux_token p m =
    match p, m, r with
    | ThisToken, (Const _ | Regex _), RToken tok -> tok
    | _ -> assert false
  in
  aux_row p lm

let map_reads (f : 'a -> 'b) (reads : 'a list list) : 'b list list  =
  List.map
    (fun example_reads ->
      List.map f example_reads)
    reads

let filter_map_reads (f : 'a -> 'b option) (reads : 'a list list) : 'b list list  =
  List.filter_map
    (fun example_reads ->
      let defined_example_reads = List.filter_map f example_reads in
      if defined_example_reads = []
      then None
      else Some defined_example_reads)
    reads

(* let inter_union_reads
      (f : 'read -> bool (* can be nil *) * ('r * 'd) list)
      (reads : 'read list list)
    : ('r, ('read * 'd option) list) Mymap.t =
  (* given a function extracting refinement information [type 'r] from each read,
     return a set of such ref-info, each mapped to the dl-shortest reads supporting it, along with new data *)
  let process_example reads =
    assert (reads <> []);
    List.fold_left
      (* nil_read_opt is Some (read0, None) if all reads are nil, otherwise None *)
      (fun (nil_read_opt,refs) read ->
        let isnil_read, refs_read = f read in
        let nil_read_opt =
          match nil_read_opt with
          | None -> None
          | Some _ ->
             if isnil_read
             then nil_read_opt
             else None in
        let refs =
          List.fold_left
            (fun refs (r,data') ->
              if Mymap.mem r refs
              then refs
              else Mymap.add r (read, Some data') refs)
            refs refs_read in
        nil_read_opt, refs)
      (Some (List.hd reads, None), Mymap.empty) reads in
  match reads with
  | [] -> assert false
  | example0_reads :: other_reads ->
     let nil_read_opt0, refs0 = process_example example0_reads in
     let nil_reads_opt = Option.map (fun nil_read -> [nil_read]) nil_read_opt0 in
     let refs = refs0 |> Mymap.map (fun best_read -> [best_read]) in
     let nil_reads_opt, refs =
       List.fold_left
         (fun (nil_reads_opt,refs) exampleI_reads ->
           let nil_readI_opt, refsI = process_example exampleI_reads in
           let refs =
             Mymap.merge
               (fun r best_reads_opt best_readI_opt ->
                 match best_reads_opt, best_readI_opt, nil_reads_opt, nil_readI_opt with
                 | Some best_reads, Some best_readI, _, _ -> Some (best_readI :: best_reads)
                 | Some best_reads, None, _, Some nil_readI -> Some (nil_readI :: best_reads)
                 | None, Some best_readI, Some nil_reads, _ -> Some (best_readI :: nil_reads)
                 | _ -> None)
               refs refsI in
           let nil_reads_opt =
             match nil_reads_opt, nil_readI_opt with
             | Some nil_reads, Some nil_readI -> Some (nil_readI :: nil_reads)
             | _ -> None in
           nil_reads_opt, refs)
         (nil_reads_opt, refs) other_reads in
     refs *)

let inter_union_reads
      (to_string : 'read -> string)
      (f : 'read -> ('r * 'd) list)
      (reads : 'read list list)
    : ('r, ('read * ('d,string) Result.t) list) Mymap.t =
  (* given a function extracting refinement information [type 'r] from each read,
     return a set of such ref-info, each mapped to the dl-shortest reads supporting it, along with new data *)
  let process_example reads =
    assert (reads <> []);
    let read0 = List.hd reads in
    let alt_read = (read0, Result.Error (to_string read0)) in
    let refs =
      List.fold_left
        (fun refs read ->
          let refs_read = f read in
          List.fold_left
            (fun refs (r,data') ->
              if Mymap.mem r refs
              then refs
              else Mymap.add r (read, Result.Ok data') refs)
            refs refs_read)
        Mymap.empty reads in
    alt_read, refs
  in
  match reads with
  | [] -> assert false
  | example0_reads :: other_reads ->
     let alt_read0, refs0 = process_example example0_reads in
     let alt_reads = [alt_read0] in
     let refs = refs0 |> Mymap.map (fun best_read -> [best_read]) in
     let alt_reads, refs =
       List.fold_left
         (fun (alt_reads,refs) exampleI_reads ->
           let alt_readI, refsI = process_example exampleI_reads in
           let refs =
             Mymap.merge
               (fun r best_reads_opt best_readI_opt ->
                 match best_reads_opt, best_readI_opt with
                 | Some best_reads, Some best_readI -> Some (best_readI :: best_reads)
                 | Some best_reads, None -> Some (alt_readI :: best_reads)
                 | None, Some best_readI -> Some (best_readI :: alt_reads)
                 | _ -> None)
               refs refsI in
           let alt_reads = alt_readI :: alt_reads in
           alt_reads, refs)
         (alt_reads, refs) other_reads in
     refs

let row_refinements ~nb_env_paths (lm : row_model) ?(dl_M : dl = 0.) (rsr : rows_reads) : (row_path * cell_refinement * dl * row_model) Myseq.t =
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
    if reads = [] then Myseq.empty else
    match m with
    | Empty -> Myseq.empty
    | Nil -> Myseq.empty
    | Any ->
       let encoder_m = cell_encoder m in
       let dl_m = dl_cell_model ~nb_env_paths m in
       let r_best_reads : ([`IsNil | `Token of token_model | `CommonStr of string], _) Mymap.t =
         inter_union_reads
           (fun (_,data,_) -> cell_of_cell_data data)
           (fun (_,data,_) ->
             let s = cell_of_cell_data data in
             let rs = (* the nil string *)
               if s = "" then [(`IsNil, DNil)] else [] in
             let rs = (* token models *)
               if s <> ""
               then
                 List.fold_left
                   (fun rs tm ->
                     let best_len, (sl, data', sr as best_slice) =
                       Myseq.fold_left
                         (fun (best_len, best_slide as best) (_, data', _ as slice) ->
                           let len = token_data_length data' in
                           if len > best_len
                           then (len, slice)
                           else best)
                         (0, ("", DToken "", ""))
                         (token_parse tm s) in
                     if best_len > 0
                     then (`Token tm, DFactor (DAny sl, data', DAny sr)) :: rs
                     else rs)
                   rs
                   (List.map
                      (fun re -> Regex re)
                      [(*Content;*) Word; Letters; Decimal; Digits; (*Separators;*) Spaces] (* ignoring mixed letter classes because they are too eager *)
                    @ List.map
                        (fun spe -> Const spe)
                        special_consts)
               else rs in
             (* let rs = (* constant strings *)
               if s <> ""
               then (`CommonStr s, DFactor (DNil, DToken s, DNil)) :: rs
               else rs in *) (* disable to avoid unstructured constant strings like ' 4/11', must be instance of a regexp *)
             rs)
           reads in
       let* r_info, best_reads = Mymap.to_seq r_best_reads in
       let supp, nb =
         List.fold_left
           (fun (supp,nb) (read, d_res) ->
             match d_res with
             | Result.Ok _ -> supp+1, nb+1
             | Result.Error _ -> supp, nb+1)
           (0,0) best_reads in
       let alt, best_reads =
         if supp = nb
         then false, List.map
                       (function (read, Result.Ok data') -> (read, data') | _ -> assert false)
                       best_reads
         else true, List.map
                      (fun (read, d_res) ->
                        let new_data =
                          match d_res with
                          | Result.Ok d -> DAlt (1, d)
                          | Result.Error s -> DAlt (2, DAny s) in
                        read, new_data)
                      best_reads in
       let* m' =
         match r_info with
         | `IsNil ->
            let m' = Nil in
            if alt then Myseq.empty
            else Myseq.return m'
         | `Token tm ->
            let l, r, c2 = (* for both left and right: Nil if all empty strings, Any otherwise *)
              List.fold_left
                (fun (l,r,c2) (read,data') ->
                  match data' with
                  | DFactor (DAny sl, _, DAny sr)
                    | DAlt (1, DFactor (DAny sl, _, DAny sr)) ->
                     (if sl <> "" then Any else l),
                     (if sr <> "" then Any else r),
                     c2
                  | DAlt (2, DAny sc2) ->
                     l, r, (if sc2 <> "" then Any else c2)
                  | _ -> assert false)
                (Nil, Nil, Nil) best_reads in
            let m' = Factor (l, tm, r) in
            let m'= if alt then Alt (m', c2) else m' in
            Myseq.return m'
         | `CommonStr s ->
            let c2 =
              List.fold_left
                (fun c2 (read,data') ->
                  match data' with
                  | DFactor _ | DAlt (1, DFactor _) -> c2
                  | DAlt (2, DAny sc2) -> (if sc2 <> "" then Any else c2)
                  | _ -> assert false)
                Nil best_reads in
            let m' = Factor (Nil, Const s, Nil) in
            let m' = if alt then Alt (m', c2) else m' in
            Myseq.return m' in
       let r = RCell (supp,m') in
       let encoder_m' = cell_encoder m' in
       let dl_m' = dl_cell_model ~nb_env_paths m' in
       let dl' =
         dl_M -. dl_m +. dl_m'
         +. !alpha *. Mdl.sum best_reads
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
    | Alt (c1,c2) ->
       Myseq.concat
         [ fold_cell c1
             (filter_map_reads
                (function
                 | (idx, DAlt (i, dc), l) ->
                    if i = 1 then Some (idx, dc, l) else None
                 | read -> Some read) (* for when the Alt has collapsed after evaluation *)
                reads)
           |> Myseq.map (fun (p,r,dl') -> Index (1,p), r, dl');
           fold_cell c2
             (filter_map_reads
                (function
                 | (idx, DAlt (i,dc), l) ->
                    if i = 2 then Some (idx, dc, l) else None
                 | read -> Some read)
                reads)
           |> Myseq.map (fun (p,r,dl') -> Index (2,p), r, dl') ]
  and fold_token (m : token_model) reads =
    match m with
    | Const _ | Regex _ ->
       let encoder_m = token_encoder m in
       let dl_m = dl_token_model ~nb_env_paths m in
       let re'_candidates =
         match m with
         | Regex Content -> [Word; Decimal]
         | Regex Word -> [Letters; Digits]
         | Regex Decimal -> [Digits]
         | Regex Separators -> [Spaces]
         | _ -> [] in
       let r_best_reads : ([`CommonStr of string | `RE of regex_model | `Expr of expr], _) Mymap.t =
         inter_union_reads
           (fun (_,data,_) -> token_of_token_data data)
           (fun (idx,data,_) ->
             let s = token_of_token_data data in
             let es = Expr.index_lookup (`String s) idx in
             let rs =
               if s <> "" && (match m with Regex _ -> true | _ -> false)
               then [(`CommonStr s, DToken s)]
               else [] in
             let rs =
               List.fold_left
                 (fun rs re' ->
                   if regexp_match_full (re_of_regex re') s
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
       let* best_reads =
         Myseq.from_result
           (list_map_result
              (function
               | (read, Result.Ok data') -> Result.Ok (read, data')
               | _ -> Result.Error (Failure "no Alt in tokens"))
              best_reads) in
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
         +. !alpha *. Mdl.sum best_reads
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
  Myseq.return (p, r, dl', m')


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
  | Rinput of row_path * cell_refinement * dl (* estimated result DL *)
  | Routput of row_path * cell_refinement * dl (* estimated result DL *)

let rec xp_refinement (print : Xprint.t) = function
  | RInit -> print#string "init"
  | Rinput (p,ri,dl') -> xp_refinement_aux print " In." p ri dl' "i"
  | Routput (p,ro, dl') -> xp_refinement_aux print " Out." p ro dl' "o"
and xp_refinement_aux print in_out p r dl' i_o =
  print#string (Printf.sprintf " / ~%.3f%s)  " dl' i_o);
  print#string in_out;
  xp_row_path print p;
  print#string " ← ";
  xp_cell_refinement print r
let pp_refinement = Xprint.to_stdout xp_refinement
let string_of_refinement = Xprint.to_string xp_refinement

let apply_refinement (r : refinement) (m : model) : (refinement * model) result =
  match r with
  | RInit -> Result.Error (Failure "apply_refinement")
  | Rinput (p,ri,dl') ->
     Result.Ok (r, {m with input_model = apply_cell_refinement ri p m.input_model})
  | Routput (p,ro,dl') ->
     Result.Ok (r, {m with output_model = apply_cell_refinement ro p m.output_model})

let model_refinements (last_r : refinement) (m : model) (prs : pairs_reads) (dsri : rows_reads) (dsro : rows_reads) : (refinement * model) Myseq.t =
  Myseq.concat (* TODO: rather order by estimated dl *)
    [ (let* p, ri, dli', mi = row_refinements ~nb_env_paths:0 ~dl_M:prs.dl_mi m.input_model dsri in
       Myseq.return (Rinput (p,ri,dli'), {m with input_model = mi}));
      (let* p, ro, dlo', mo = row_refinements ~nb_env_paths:(dl_row_model_env_stats m.input_model) ~dl_M:prs.dl_mo m.output_model dsro in
       Myseq.return (Routput (p,ro,dlo'), {m with output_model = mo})) ]

  
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
    ~code:(fun (r,m) (prs,gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     norm_dl_model_data prs in
           if verbose then (
             Printf.printf "\t?? %.3f\t" lmd;
             pp_refinement r; print_newline ();
(*
	     Printf.printf "\t\tl = %.3f = %.3f + %.3f = (%.3f + %.3f) + (%.3f + %.3f)\n" lmd lm ld lmi lmo ldi ldo;
             print_endline " ===> all reads for first example";
             List.hd prs.reads
             |> List.iter
                  (fun ((_,{data=d_i},dl_i), (_, {data=d_o}, dl_o), dl) ->
                    print_endline " --- some read ---";
                    pp_data d_i; print_newline ();
                    pp_data d_o; print_newline ();
                    Printf.printf "\tdl=%.6f\n" dl);
             print_newline ()
              
             print_endline " ===> best read for all examples";
             prs.reads
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
    (fun (r,m) (prs,gsri,gsro) dl ->
      if verbose then print_newline ();
      Printf.printf "%.3f\t" dl; pp_refinement r; print_newline ();
      if verbose then (
        print_endline " ===> first read for first example";
        List.hd (List.hd prs.reads)
        |> (fun ((_,d_i,dl_i), (_, d_o, dl_o), dl) ->
          print_endline " --- some read ---";
          pp_row_data d_i; print_newline ();
          pp_row_data d_o; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = model_refinements r m prs gsri gsro in
      refs))

