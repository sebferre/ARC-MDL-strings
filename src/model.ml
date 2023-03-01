
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

(* model levels *)
type row
type cell
type token

type _ path =
  | This : _ path
  | Col : int (* column index *) * cell path -> row path
  | Left : cell path -> cell path
  | Middle : token path -> cell path
  | Right : cell path -> cell path
  | Index : int * 'a path -> 'a path

type 'a ctx = 'a path -> row path
let ctx0 : row ctx = (fun p -> p)

type var = row path
type expr = var Expr.expr (* using doc_paths as vars *)
type exprset = var Expr.exprset
type index = var Expr.index

type regex_model =
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

type _ model =
  | Row : cell model list -> row model
  | Empty : _ model (* empty language *)
  | Nil : cell model (* epsilon *)
  | Any : cell model
  | Factor : cell model * token model * cell model -> cell model
  | Alt : 'a model * 'a model -> 'a model (* Opt c = Alt (c,Nil) *)
  | Const : string -> token model
  | Regex : regex_model -> token model
  | Expr : expr -> token model

let row_model0 (row_size : int) : row model = Row (List.init row_size (fun _ -> Any))

type _ data =
  | DRow : cell data list -> row data
  | DNil : cell data
  | DAny : string -> cell data
  | DFactor : cell data * token data * cell data -> cell data
  | DAlt : int * 'a data -> 'a data (* DOpt (Some d) = DAlt (1,d); DOpt None = DAlt (2, DNil) *)
  | DToken : string -> token data
            
let row_data0 (row_size : int) = DRow (List.init row_size (fun _ -> DNil))

type env = row data
let env0 = row_data0 0

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

let rec id_of_path : type a. ?power2:int -> ?acc:int -> a path -> string =
  fun ?(power2 = 1) ?(acc = 0) p ->
  match p with
  | This -> string_of_int (power2 + acc)
  | Col (i,p1) ->
     String.make 1 (Char.chr (Char.code 'A' + i)) (* column letter *)
     ^ id_of_path ~power2:1 ~acc:0 p1
  | Left p1 -> id_of_path ~power2:(2 * power2) ~acc p1
  | Right p1 -> id_of_path ~power2:(2 * power2) ~acc:(power2 + acc) p1
  | Middle p1 -> id_of_path ~power2 ~acc p1
  | Index (i,p1) -> id_of_path ~power2:(2 * power2) ~acc:(if i=1 then acc else power2 + acc) p1
  
let xp_row_path (print : Xprint.t) (p : row path) =
  let id = id_of_path p in
  print#string "<span class=\"model-path\">";
  print#string id;
  print#string "</span>"
let pp_row_path = Xprint.to_stdout xp_row_path

let xp_brackets_prio ~prio_ctx ~prio print xp =
  if prio <= prio_ctx
  then xp print
  else xp_brackets print xp

let xp_regex_model print = function
  | Content -> print#string "Content"
  | Word -> print#string "Word"
  | Letters -> print#string "Letters"
  | Decimal -> print#string "Decimal"
  | Digits -> print#string "Digits"
  | Separators -> print#string "Separators"
  | Spaces -> print#string "Spaces"

let rec xp_model : type a. ?prio_ctx:int -> Xprint.t -> ?ctx:(a ctx) -> a model -> unit =
  fun ?(prio_ctx = 2) print ?ctx m ->
  match m with
  | Row lm ->
     List.iteri
       (fun i m ->
         let ctx_cell = ctx |> Option.map (fun ctx -> (fun p -> ctx (Col (i,p)))) in
         if i > 0 then print#string "</br>";
         xp_model ~prio_ctx print ?ctx:ctx_cell m)
       lm
  | Empty -> print#string "∅"
  | Nil -> ()
  | Any -> print#string "<span class=\"model-any\">*</span>"
  | Factor (l,t,r) ->
     let ctx_l = ctx |> Option.map (fun ctx -> (fun p -> ctx (Left p))) in
     let ctx_t = ctx |> Option.map (fun ctx -> (fun p -> ctx (Middle p))) in
     let ctx_r = ctx |> Option.map (fun ctx -> (fun p -> ctx (Right p))) in
     xp_brackets_prio ~prio_ctx ~prio:0 print
       (fun print ->
         print#string "<div class=\"model-factor\">";
         xp_model ~prio_ctx:0 print ?ctx:ctx_l l;
         xp_model ~prio_ctx:0 print ?ctx:ctx_t t;
         xp_model ~prio_ctx:0 print ?ctx:ctx_r r;
         print#string "</div>")
  | Alt (c, Nil) (* Opt c *) ->
     let ctx_1 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (1, p)))) in
     xp_brackets_prio ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"model-opt\">";
         xp_model ~prio_ctx:1 print ?ctx:ctx_1 c;
         print#string " <span class=\"model-meta-operator\">?</span>";
         print#string "</div>")
  | Alt (Nil, c) (* Opt c *) ->
     let ctx_2 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (2, p)))) in     
     xp_brackets_prio ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"model-opt\">";
         xp_model ~prio_ctx:1 print ?ctx:ctx_2 c;
         print#string " <span class=\"model-meta-operator\">?</span>";
         print#string "</div>")
  | Alt (c1,c2) ->
     let ctx_1 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (1, p)))) in
     let ctx_2 = ctx |> Option.map (fun ctx -> (fun p -> ctx (Index (2, p)))) in
     xp_brackets_prio ~prio_ctx ~prio:2 print
       (fun print ->
         print#string "<div class=\"model-alt\">";
         xp_model ~prio_ctx:2 print ?ctx:ctx_1 c1;
         print#string " <span class=\"model-meta-operator\">|</span> ";
         xp_model ~prio_ctx:2 print ?ctx:ctx_2 c2;
         print#string "</div>")
  | Const s ->
     let p_opt = ctx |> Option.map (fun ctx -> ctx This) in
     print#string "<span class=\"model-const\"";
     p_opt |> Option.iter (* print path as tooltip *)
                (fun p ->
                  print#string " title=\"";
                  print#string (id_of_path p);
                  print#string "\"");
     print#string ">";
     xp_string print s;
     print#string "</span>"
  | Regex re ->
     let p_opt = ctx |> Option.map (fun ctx -> ctx This) in
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
let pp_model m = Xprint.to_stdout (xp_model ~ctx:ctx0) m
let string_of_model m = Xprint.to_string (xp_model ~ctx:ctx0) m
                    
let rec xp_data : type a. ?prio_ctx:int -> Xprint.t -> a data -> unit =
  fun ?(prio_ctx = 0) print d ->
  match d with
  | DRow ld ->
     List.iteri
       (fun i di ->
         if i > 0 then print#string "</br>";
         xp_data print di)
       ld
  | DNil -> ()
  | DAny s ->
     print#string "<span class=\"data-any\">";
     xp_string print s;
     print#string "</span>"
  | DFactor (l,t,r) ->
     xp_brackets_prio ~prio_ctx ~prio:0 print
       (fun print ->
         print#string "<div class=\"data-factor\">";
         xp_data ~prio_ctx:0 print l;
         xp_data ~prio_ctx:0 print t;
         xp_data ~prio_ctx:0 print r;
         print#string "</div>")
  | DAlt (i, DNil) ->
     xp_brackets_prio ~prio_ctx ~prio:1 print
       (fun print ->
         print#string "<div class=\"data-opt\">ε</div>")
  | DAlt (i,c) -> (* TODO: find better repr than 1/2: to indicate valid branch *)
     xp_brackets_prio ~prio_ctx ~prio:2 print
       (fun print ->
         print#string "<div class=\"data-alt\">";
         (* print#int i; *)
         xp_data ~prio_ctx:2 print c;
         print#string "</div>")
  | DToken s ->
     print#string "<span class=\"data-token\">";
     xp_string print s;
     print#string "</span>"
let pp_data = Xprint.to_stdout xp_data
let string_of_data : type a. a data -> string = fun d -> Xprint.to_string xp_data d

                       
(* get and apply *)

let rec contents_of_data : type a. a data -> string = function
  | DRow _ -> assert false
  | DNil -> ""
  | DAny s -> s
  | DFactor (l,t,r) -> contents_of_data l
                       ^ contents_of_data t
                       ^ contents_of_data r
  | DAlt (i,c) -> contents_of_data c
  | DToken s -> s

let rec contents_of_row_data (d : row data) : string list =
  match d with
  | DRow ld -> List.map contents_of_data ld
  | DAlt (_,d) -> contents_of_row_data d

let rec data_length : type a. a data -> int = function
  | DRow ld -> List.fold_left (fun res d -> res + data_length d) 0 ld
  | DNil -> 0
  | DAny s -> String.length s
  | DFactor (l,t,r) -> data_length l + data_length t + data_length r
  | DAlt (_,c) -> data_length c
  | DToken s -> String.length s

let rec find : type a. a path -> a data -> Expr.value result =
  fun p d ->
  match p, d with
  | This, _ -> Result.Ok (`String (contents_of_data d))
  | Col (i,p), DRow ld ->
     let d = try List.nth ld i with _ -> assert false in
     find p d
  | Left p1, DFactor (l,_,_) -> find p1 l
  | Middle p1, DFactor (_,t,_) -> find p1 t
  | Right p1, DFactor (_,_,r) -> find p1 r
  | Index (i,p1), DAlt (i',c) ->
     if i = i'
     then find p1 c
     else Result.Ok `Null
  | _ -> assert false

type bindings = (var * Expr.value) list
let bindings0 = []

let rec bindings_aux : type a. a ctx -> a data -> bindings -> bindings =
  fun ctx d acc ->
  match d with
  | DRow ld ->
     let _, acc =
       List.fold_left
         (fun (i,acc) d ->
           i+1, bindings_aux (fun p -> ctx (Col (i,p))) d acc)
         (0,acc) ld in
     acc
  | DNil -> acc
  | DAny _ -> acc
  | DFactor (l,t,r) ->
     let acc = bindings_aux (fun p -> ctx (Right p)) r acc in
     let acc = bindings_aux (fun p -> ctx (Left p)) l acc in
     let acc = bindings_aux (fun p -> ctx (Middle p)) t acc in
     acc
  | DAlt (i,c) ->
     let acc = bindings_aux (fun p -> ctx (Index (i,p))) c acc in
     acc
  | DToken s -> (ctx This, `String s) :: acc

let bindings (drow : row data) : bindings =
  bindings_aux ctx0 drow []

let eval_expr_on_env e env =
  Expr.eval (fun p -> find p env) e
  
exception NullExpr (* error for expressions that contains a null value *)

let rec apply : type a. a model -> env -> a model result =
  fun m env ->
  match m with
  | Row lm ->
     let| lm' =
       list_map_result
         (fun m -> apply m env)
         lm in
     Result.Ok (Row lm')
  | Empty -> Result.Ok Empty
  | Nil -> Result.Ok Nil
  | Any -> Result.Ok Any
  | Factor (l,t,r) ->
     let| l' = apply l env in
     let| t' = apply t env in
     let| r' = apply r env in
     Result.Ok (Factor (l',t',r'))
  | Alt (c1,c2) ->
     let res1 = apply c1 env in
     let res2 = apply c2 env in
     (match res1, res2 with
      | Result.Ok c1', Result.Ok c2' -> Result.Ok (Alt (c1', c2'))
      | Result.Error NullExpr, Result.Ok c2' -> Result.Ok (Alt (Empty, c2'))
      | Result.Ok c1', Result.Error NullExpr -> Result.Ok (Alt (c1', Empty))
      | _ -> res1)
  | Const s -> Result.Ok (Const s)
  | Regex re -> Result.Ok (Regex re)
  | Expr e ->
     let| v = eval_expr_on_env e env in
     (match v with
      | `String s -> Result.Ok (Const s)
      (* TODO: consider converting other values to strings *)
      | `Null -> Result.Error NullExpr
      | _ -> Result.Error (Invalid_argument "Model.token_apply: string expected as expression value"))

     
(* generate *)

let regex_generate : regex_model -> string = function
  | Content -> "A-b_1&2"
  | Word -> "Ab_1"
  | Letters -> "Abc"
  | Decimal -> "12.3"
  | Digits -> "123"
  | Separators -> ", "
  | Spaces -> " "

let rec generate : type a. a model -> a data = function
  | Row lm -> DRow (List.map generate lm)
  | Empty -> assert false
  | Nil -> DNil
  | Any -> DAny "..."
  | Factor (l,t,r) -> DFactor (generate l, generate t, generate r)
  | Alt (c1,c2) -> (* TODO: make stochastic ? *)
     if c1 <> Empty then DAlt (1, generate c1)
     else if c2 <> Empty then DAlt (2, generate c2)
     else assert false
  | Const s -> DToken s
  | Regex re -> DToken (regex_generate re)
  | Expr _ -> assert false


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

let token_parse (re : Str.regexp) (s : string) : (token data * (string * string)) Myseq.t =
  assert (s <> "");
  let n = String.length s in
  let* i, j = regexp_match_slices re s n 0 in
  let sl = String.sub s 0 i in
  let st = String.sub s i (j-i) in
  let sr = String.sub s j (n-j) in
  let dt = DToken st in
  Myseq.return (dt,(sl,sr))
let token_parse, reset_token_parse =
  let f, reset =
    Common.memoize ~size:1003
      (fun (re,s) -> Myseq.memoize (token_parse re s)) in
  let f = fun m s -> f (m,s) in
  f, reset

type (_,_,_) parsing =
  | RowParsing : (row, string list, unit) parsing
  | CellParsing : (cell, string, unit) parsing
  | TokenParsing : (token, string, string * string) parsing
  
let rec parse : type a c b. (a,c,b) parsing -> a model -> (c, a data * b) parseur =
  fun parsing m cnt ->
  match parsing, m, cnt with
  | RowParsing, Row lm, ls ->
     let* ld =
       Myseq.product_fair
         (List.map2
            (fun m s ->
              let* d, () = parse CellParsing m s in
              Myseq.return d)
            lm ls) in
     Myseq.return (DRow ld, ())
  | _, Empty, _ -> Myseq.empty
  | CellParsing, Nil, s ->
     if s = ""
     then Myseq.return (DNil, ())
     else Myseq.empty
  | CellParsing, Any, s ->
     Myseq.return (DAny s, ())
  | CellParsing, Factor (l,t,r), s ->
     if s = ""
     then Myseq.empty
     else
       let* dt, (sl, sr) = parse TokenParsing t s in
       let* dl, () = parse CellParsing l sl in
       let* dr, () = parse CellParsing r sr in
       Myseq.return (DFactor (dl, dt, dr), ())
  | _, Alt (c1,c2), _ -> (* exclusive or, like if-then-else *)
     let seq1 = parse parsing c1 cnt in
     if Myseq.is_empty seq1
     then
       let* dc2, b2 = parse parsing c2 cnt in
       Myseq.return (DAlt (2,dc2), b2)
     else
       let* dc1, b1 = seq1 in
       Myseq.return (DAlt (1,dc1), b1)
  | TokenParsing, Const cst, s -> token_parse (Str.regexp_string cst) s
  | TokenParsing, Regex rm, s -> token_parse (re_of_regex rm) s
  | _, Expr _, _ -> assert false


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
  
let rec dl_model_env_stats : type a. a model -> int = function
  (* counting paths to tokens (see bindings) *)
  | Row lm ->
     List.fold_left
       (fun res m -> res + dl_model_env_stats m)
       0 lm
  | Empty -> 0
  | Nil -> 0
  | Any -> 0
  | Factor (l,t,r) ->
     dl_model_env_stats l
     + dl_model_env_stats t
     + dl_model_env_stats r
  | Alt (c1,c2) ->
     dl_model_env_stats c1
     + dl_model_env_stats c2
  | Const _ -> 1
  | Regex _ -> 1
  | Expr _ -> 1


(* TODO: need reflexion, and a generic solution for this problem *)            
let rec dl_model_aux : type a. nb_env_paths:int -> a model -> (int * int * int) * dl = (* triple (nb_any, nb_factor, nb_alt *)
  fun ~nb_env_paths m ->
  match m with
  | Row lm ->
     (0,0,0), (* TODO: better choice? *)
     Mdl.sum lm (* cell models are independent *)
       (fun m ->
         let (nb_any, nb_factor, nb_alt), dl = dl_model_aux ~nb_env_paths m in
         Mdl.Code.universal_int_star nb_factor (* encoding total nb of factors/tokens *)
         +. Mdl.Code.universal_int_star nb_any (* encoding total nb of Any's, to favor Nil's *)
         (* +. Mdl.Code.uniform (nb_factor + 2) (* alternate encoding, based on bound for nb_any *) *)
         +. Mdl.Code.universal_int_star nb_alt (* encoding total nb of Alt's *)
         +. dl)
  | Empty -> assert false
  | Nil -> (0,0,0), 0.
  | Any -> (1,0,0), 0.
  | Factor (l,t,r) ->
     let (na_l, nf_l, no_l), dl_l = dl_model_aux ~nb_env_paths l in
     let (na_t, nf_t, no_t), dl_t = dl_model_aux ~nb_env_paths t in
     let (na_r, nf_r, no_r), dl_r = dl_model_aux ~nb_env_paths r in
     let nb_any = na_l + na_t + na_r in
     let nb_factor = 1 + nf_l + nf_t + nf_r in
     let nb_alt = no_l + nf_t + no_r in
     (nb_any, nb_factor, nb_alt),
     Mdl.Code.usage (float nb_factor /. float (nb_factor + nb_alt)) (* choice between Factor and Alt *)
     +. (* encoding split of remaining Factor's, assuming nf_t = 0 so far *)
       (let k = nb_factor in
         assert (k > 0);
         Mdl.Code.uniform k)
     +. (* encoding split of Alt's, values of no_l vs no_t+no_r *)
       (let k = nb_alt + 1
          (* min nb_alt (na_l + nf_l) (* maximal possible no_l *)
          - max 0 (nb_alt - (na_r + nf_r)) (* minimal possible no_l *)
          + 1 *) in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. (* encoding split of Alt's, no_t vs no_r *)
       (let k = nb_alt - no_l + 1 in
         assert (k > 0);
         Mdl.Code.uniform k)
     +. (* encoding split of Any's, values of na_l, na_r, assuming na_t = 0 so far *)
       (let k = (* nb_any + 1 *) (* TODO: refine *)
          min nb_any (nf_l + no_l + 1) (* maximal possible value for na_l *)
          - max 0 (nb_any - (nf_r + no_r + 1)) (* minimal possible value for na_l *)
          + 1 in
        assert (k > 0);
        Mdl.Code.uniform k)
     +. dl_l +. dl_t +. dl_r
  | Alt (c1,c2) ->
     let (na_c1, nf_c1, no_c1), dl1 = dl_model_aux ~nb_env_paths c1 in
     let (na_c2, nf_c2, no_c2), dl2 = dl_model_aux ~nb_env_paths c2 in
     let nb_any = na_c1 + na_c2 in
     let nb_factor = nf_c1 + nf_c2 in
     let nb_alt = 1 + no_c1 + no_c2 in
     (nb_any, nb_factor, nb_alt),
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
     +. dl1 +. dl2
  | Const s ->
     (0,0,0),
     Mdl.Code.usage 0.3
     +. Mdl.Code.universal_int_plus (String.length s)
     +. dl_string_ascii s
  | Regex rm ->
     (0,0,0),
     Mdl.Code.usage 0.2
     +. Mdl.Code.uniform nb_regex
  | Expr e ->
     (0,0,0),
     Mdl.Code.usage 0.5
     +. Expr.dl_expr
          (fun p ->
            let k = nb_env_paths in
            assert (k > 0);
            Mdl.Code.uniform k)
          e

let dl_model ~(nb_env_paths : int) (m : 'a model) : dl =
  let (nb_any, nb_factor, nb_alt), dl = dl_model_aux ~nb_env_paths m in
  Mdl.Code.universal_int_star nb_factor (* encoding total nb of factors/tokens *)
  +. Mdl.Code.universal_int_star nb_any (* encoding total nb of Any's, to favor Nil's *)
  +. Mdl.Code.universal_int_star nb_alt (* encoding total nb of Alt *)
  +. dl

  
type 'a encoder = 'a -> dl

let rec encoder_range : type a. a model -> Range.t = function  (* min-max length range for doc_models *)
  | Row _ -> assert false
  | Empty -> assert false
  | Nil -> Range.make_exact 0
  | Any -> Range.make_open 0
  | Factor (l,t,r) ->
     let range_l = encoder_range l in
     let range_t = encoder_range t in
     let range_r = encoder_range r in
     Range.sum [range_l; range_t; range_r]
  | Alt (Empty,Empty) -> assert false
  | Alt (c1,Empty) -> encoder_range c1
  | Alt (Empty,c2) -> encoder_range c2
  | Alt (c1,c2) ->
     let range_c1 = encoder_range c1 in
     let range_c2 = encoder_range c2 in
     Range.union range_c1 range_c2
  | Const s -> Range.make_exact (String.length s)
  | Regex _ -> Range.make_open 1
  | Expr _ -> Range.make_open 1

let rec encoder : type a. a model -> a data encoder = function
  (* assuming that the passed data matches the model *)
  | Row lm ->
     let l_enc_m = List.map encoder lm in
     (function
      | DRow ld ->
         List.fold_left2
           (fun res enc_m d ->
             res
             +. Mdl.Code.universal_int_star (data_length d)
             +. enc_m d)
           0. l_enc_m ld
      | _ -> assert false)
  | Empty ->
     (function _ -> assert false)
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
       let range_l = encoder_range l in
       let range_t = encoder_range t in
       let range_r = encoder_range r in
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
     let enc_l = encoder l in
     let enc_t = encoder t in
     let enc_r = encoder r in
     (function
      | DFactor (dl,dt,dr) ->
         let nl_nt_nr = data_length dl, data_length dt, data_length dr in
         enc_split nl_nt_nr +. enc_l dl +. enc_t dt +. enc_r dr
      | _ -> assert false)
  | Alt (c1,c2) ->
     let dl_choice1, dl_choice2 =
       match c1, c2 with
       | Empty, _ | _, Empty -> 0., 0. (* no choice to be made *)
       | _ -> Mdl.Code.usage 0.6, Mdl.Code.usage 0.4 in (* favoring fst alternative *)
     let enc_c1 = encoder c1 in
     let enc_c2 = encoder c2 in
     (function
      | DAlt (1,dc) -> dl_choice1 +. enc_c1 dc
      | DAlt (2,dc) -> dl_choice2 +. enc_c2 dc
      | _ -> assert false)
  | Const _ ->
     (function
      | DToken _ -> 0.
      | _ -> assert false)
  | Regex rm ->
     let enc_rm = dl_string_regex rm in
     (function
      | DToken s -> enc_rm s
      | _ -> assert false)
  | Expr _ -> (fun _ -> 0.) (* nothing to code, evaluated *)


(* reading *)

type 'a read =
  { env : env;
    index : index;
    data : 'a data;
    dl : dl }

let limit_dl (f_dl : 'a -> dl) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x0::_ ->
     let dl0 = f_dl x0 in
     let min_dl = !max_parse_dl_factor *. dl0 in
     List.filter (fun x -> f_dl x <= min_dl) l

let read ~(env : env) (m0 : row model) (ls : string list) : row read list result =
  Common.prof "Model.read" (fun () ->
  let| m = apply m0 env in (* reducing expressions *)
  let parses =
    let* data, () = parse RowParsing m ls in
    let dl = (* QUICK *)
      let dl_data = encoder m data in
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
             let index = Expr.make_index (bindings env) in
             let dl = dl +. Mdl.Code.universal_int_star rank in (* to penalize later parses, in case of equivalent parses *)
             { env;  index; data; dl }) in
    Result.Ok best_parses)

type reads =
  { dl_m : dl; (* DL of the model *)
    reads : row read list list; (* outer list over docs, inner list over parses, sorted in increasing DL *)
  }

(* writing *)

let write ~(env : env) (m : row model) : (string list, exn) Result.t = Common.prof "Model.write_doc" (fun () ->
  let| m' = apply m env in
  let d = generate m' in
  let ls = contents_of_row_data d in
  Result.Ok ls)
             
(* refinements *)

type refinement =
  | RCell of cell model (* cell specialization *)
  | RToken of token model (* token specialization *)

let xp_support (print : Xprint.t) (supp : int) =
  print#string " ("; print#int supp; print#string ")"

let xp_refinement (print : Xprint.t) = function
  | RCell Nil -> print#string "<span class=\"model-nil\">ε</span>"
  | RCell cell -> xp_model ~prio_ctx:2 print cell
  | RToken tok -> xp_model ~prio_ctx:2 print tok
let pp_refinement = Xprint.to_stdout xp_refinement

let rec apply_refinement : type a. refinement -> a path -> a model -> a model =
  fun rf p m ->
  match p, m, rf with
  | Col (i,p1), Row lm, _ ->
     (try Row (list_update (apply_refinement rf p1) i lm)
      with Not_found -> assert false)
  | This, Any, RCell cell -> cell
  | Left p1, Factor (l,t,r), _ -> Factor (apply_refinement rf p1 l, t, r)
  | Middle p1, Factor (l, t, r), _ -> Factor (l, apply_refinement rf p1 t, r)
  | Right p1, Factor (l, t, r), _ -> Factor (l, t, apply_refinement rf p1 r)
  | Index (1,p1), Alt (c1, c2), _ -> Alt (apply_refinement rf p1 c1, c2)
  | Index (2,p1), Alt (c1, c2), _ -> Alt (c1, apply_refinement rf p1 c2)
  | This, Const _, RToken tok -> tok
  | This, Regex _, RToken tok -> tok
  | _ -> assert false


let map_reads (f : 'a -> 'b) (reads : 'a list list) : 'b list list  =
  List.map
    (fun example_reads ->
      List.map f example_reads)
    reads

let partition_map_reads (f : 'a -> ('b,'c) Result.t) (selected_reads : 'a list list) (other_reads : 'c list list) : 'b list list * 'c list list =
  (* returns: 1) the result of applying [f] on [selected_reads] when [f] is defined, and 2) the complement part of [selected_reads] to [others] *)
  list_partition_map
    (fun example_reads ->
      let defined_example_reads, example_reads_env =
        list_partition_map f example_reads [] in
      if defined_example_reads = []
      then Result.Error example_reads_env
      else Result.Ok defined_example_reads)
    selected_reads
    other_reads

let inter_union_reads
    : type a r.
           (a read -> a data)
           -> (a read -> (r * a data) list)
           -> a read list list
           -> (r, (a read * (a data, a data) Result.t) list) Mymap.t =
  fun alt_data get_rs reads ->
  (* given a function extracting refinement information [type 'r] from each read,
     return a set of such ref-info, each mapped to the dl-shortest reads supporting it, along with new data *)
  let process_example reads =
    assert (reads <> []);
    let read0 = List.hd reads in
    let alt_read = (read0, Result.Error (alt_data read0)) in
    let refs =
      List.fold_left
        (fun refs read ->
          let refs_read = get_rs read in
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

let local_refinements
    : type a r. nb_env_paths:int -> dl_M:dl
                -> a model (* local model at some path *)
                -> a read list list (* local data with read information *)
                -> (a read -> a data) (* alternative data when a refinement does not apply *)
                -> (a read -> (r * a data) list) (* refinement information with related new local data *)
                -> (r -> alt:bool -> (a read * a data) list -> (refinement * a model) Myseq.t) (* converting refinement info, alt mode (true if partial match), support, and best reads *)
                -> (a path * refinement * int * dl) Myseq.t (* result: a sequence of path-wise refinements with estimate DL *)
  =
  fun ~nb_env_paths ~dl_M
      m selected_reads
      alt_data_of_read rs_of_read make_r_m' ->
  let encoder_m = encoder m in
  let dl_m = dl_model ~nb_env_paths m in
  let r_best_reads =
    inter_union_reads
      (fun read -> alt_data_of_read read)
      (fun read -> rs_of_read read)
      selected_reads in
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
                   let data' =
                     match d_res with
                     | Result.Ok d -> DAlt (1, d)
                     | Result.Error d -> DAlt (2, d) in
                   read, data')
                 best_reads in
  let* r, m' = make_r_m' r_info ~alt best_reads in
  let encoder_m' = encoder m' in
  let dl_m' = dl_model ~nb_env_paths m' in
  let dl' =
    dl_M -. dl_m +. dl_m'
    +. !alpha *. Mdl.sum best_reads
                   (fun (read, data') ->
                     read.dl -. encoder_m read.data +. encoder_m' data') in
  Myseq.return (This, r, supp, dl')     


let rec refinements_aux : type a. nb_env_paths:int -> dl_M:dl -> a model -> a read list list -> env list list -> (a path * refinement * int (* support *) * dl) Myseq.t =
  fun ~nb_env_paths ~dl_M m selected_reads other_reads_env ->
  if selected_reads = [] then Myseq.empty
  else
    match m with
    | Row lm ->
       Myseq.interleave
         (List.mapi
            (fun i m ->
              let m_reads =
                map_reads
                  (fun read ->
                    match read.data with
                    | DRow ld ->
                       let d_i = try List.nth ld i with _ -> assert false in
                       {read with data = d_i}
                    | _ -> assert false)
                  selected_reads in
              refinements_aux ~nb_env_paths ~dl_M m m_reads other_reads_env
              |> Myseq.map (fun (p,r,supp,dl') -> (Col (i,p), r, supp, dl')))
            lm)
    | Empty -> Myseq.empty
    | Nil -> Myseq.empty
    | Any ->
       local_refinements ~nb_env_paths ~dl_M m selected_reads
         (fun read -> DAny (contents_of_data read.data))
         (fun read ->
           (* r = [`IsNil | `Token of token model] *)
           let s = contents_of_data read.data in
           let rs = (* the nil string *)
             if s = "" then [(`IsNil, DNil)] else [] in
           let rs = (* token models *)
             if s <> ""
             then
               List.fold_left
                 (fun rs tm ->
                   let best_len, (data', (sl, sr) as best_slice) =
                     Myseq.fold_left
                       (fun (best_len, best_slide as best) (data', _ as slice) ->
                         let len = data_length data' in
                         if len > best_len
                         then (len, slice)
                         else best)
                       (0, (DToken "", ("", "")))
                       (parse TokenParsing tm s) in
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
           (* no const string to avoid unstructured constant strings like ' 4/11', must be instance of a regexp *)
           rs)
      (fun r_info ~alt best_reads ->
        let* m' =
          match r_info with
          | `IsNil ->
             let m' = Nil in
             if alt then Myseq.empty
             else Myseq.return m'
          | `Token tm ->
             let ts, l, r, c2 = (* token string set, left and right (Nil if all empty strings, Any otherwise), alternative (Nil if all alts are empty) *)
               List.fold_left
                 (fun (ts,l,r,c2) (read,data') ->
                   match data' with
                   | DFactor (DAny sl, DToken st, DAny sr)
                     | DAlt (1, DFactor (DAny sl, DToken st, DAny sr)) ->
                      (Bintree.add st ts),
                      (if sl <> "" then Any else l),
                      (if sr <> "" then Any else r),
                      c2
                   | DAlt (2, DAny sc2) ->
                      ts, l, r, (if sc2 <> "" then Any else c2)
                   | _ -> assert false)
                 (Bintree.empty, Nil, Nil, Nil) best_reads in
             let tm' = (* shortcut: replacing constant regex by a Const string *)
               match tm with
               | Regex _ when Bintree.cardinal ts = 1 -> Const (Bintree.choose ts)
               | _ -> tm in
             let m' = Factor (l, tm', r) in
             let m'= if alt then Alt (m', c2) else m' in
             Myseq.return m' in
        let r = RCell m' in
        Myseq.return (r,m'))

    | Factor (l,t,r) ->
       Myseq.concat
         [ refinements_aux ~nb_env_paths ~dl_M t
             (map_reads
                (fun read ->
                  match read.data with
                  | DFactor (_,dt,_) -> {read with data = dt}
                  | _ -> assert false)
                selected_reads)
             other_reads_env
           |> Myseq.map (fun (p,r,supp,dl') -> Middle p, r, supp, dl');
           refinements_aux ~nb_env_paths ~dl_M l
             (map_reads
                (fun read ->
                  match read.data with
                  | DFactor (dl,_,_) -> {read with data = dl}
                  | _ -> assert false)
                selected_reads)
             other_reads_env
           |> Myseq.map (fun (p,r,supp,dl') -> Left p, r, supp, dl');
           refinements_aux ~nb_env_paths ~dl_M r
             (map_reads
                (fun read ->
                  match read.data with
                  | DFactor (_,_,dr) -> {read with data = dr}
                  | _ -> assert false)
                selected_reads)
             other_reads_env
           |> Myseq.map (fun (p,r,supp,dl') -> Right p, r, supp, dl') ]
      
    | Alt (c1,c2) ->
       Myseq.concat
         [ (let sel1, other1 =
              partition_map_reads
                (fun read ->
                  match read.data with
                  | DAlt (i, dc) ->
                     if i = 1 then Result.Ok {read with data = dc} else Result.Error read.env
                  | _ -> assert false)
                selected_reads
                other_reads_env in
            refinements_aux ~nb_env_paths ~dl_M c1 sel1 other1)
           |> Myseq.map (fun (p,r,supp,dl') -> Index (1,p), r, supp, dl');
           (let sel2, other2 =
              partition_map_reads
                (fun read ->
                  match read.data with
                  | DAlt (i,dc) ->
                     if i = 2 then Result.Ok {read with data = dc} else Result.Error read.env
                  | _ -> assert false)
                selected_reads
                other_reads_env in
            refinements_aux ~nb_env_paths ~dl_M c2 sel2 other2)
           |> Myseq.map (fun (p,r,supp,dl') -> Index (2,p), r, supp, dl') ]
      
    | Const _ ->
       local_refinements ~nb_env_paths ~dl_M m selected_reads
         (* r = [`Expr of expr] *)
         (fun read -> DToken (contents_of_data read.data))
         (fun read ->
           let s = contents_of_data read.data in
           let rs =
             let es : exprset = Expr.index_lookup (`String s) read.index in
             Myseq.fold_left
               (fun rs e -> (`Expr e, DToken s) :: rs)
               [] (Expr.exprset_to_seq es) in
           rs)
         (fun r_info ~alt best_reads ->
           let m' =
             match r_info with
             | `Expr e -> Expr e in
           let m' = if alt then Alt (m',m) else m' in
           let r = RToken m' in
           Myseq.return (r,m'))
    | Regex rm ->
       let rm'_candidates =
         match rm with
         | Content -> [Word; Decimal]
         | Word -> [Letters; Digits]
         | Decimal -> [Digits]
         | Separators -> [Spaces]
         | _ -> [] in
       local_refinements ~nb_env_paths ~dl_M m selected_reads
         (* r = [`CommonStr of string | `RE of regex_model | `Expr of expr] *)
         (fun read -> DToken (contents_of_data read.data))
         (fun read ->
           let s = contents_of_data read.data in
           let rs = [] in
           let rs =
             if s <> "" && (match m with Regex _ -> true | _ -> false)
             then (`CommonStr s, DToken s)::rs
             else rs in
           let rs =
             List.fold_left
               (fun rs re' ->
                 if regexp_match_full (re_of_regex re') s
                 then (`RE re', DToken s) :: rs
                 else rs)
               rs rm'_candidates in
           let rs =
             let es : exprset = Expr.index_lookup (`String s) read.index in
             Myseq.fold_left
               (fun rs e -> (`Expr e, DToken s) :: rs)
               rs (Expr.exprset_to_seq es) in
           rs)
       (fun r_info ~alt best_reads ->
         let m' =
           match r_info with
           | `CommonStr s -> Const s
           | `RE re' -> Regex re'
           | `Expr e -> Expr e in
         let m' = if alt then Alt (m',m) else m' in
         let r = RToken m' in
         Myseq.return (r,m'))
    | Expr e -> Myseq.empty
     
let refinements ~nb_env_paths (m : row model) ?(dl_M : dl = 0.) (rsr : reads) : (row path * refinement * int (* support *) * dl * row model) Myseq.t =
  (* NOTE: dl_M does not matter for ranking because invariant of parsing and refinement *)
  let selected_reads = rsr.reads in
  let other_reads_env = [] in
  let* p, r, supp, dl' =
    refinements_aux ~nb_env_paths ~dl_M m selected_reads other_reads_env
    |> Myseq.sort (fun (p1,r1,supp1,dl1) (p2,r2,supp2,dl2) ->
           if supp1 = supp2 then dl_compare dl1 dl2
           else Stdlib.compare supp2 supp1) (* by decreasing support first *)
    |> Myseq.slice ~limit:!max_refinements in
  let m' = apply_refinement r p m in
  Myseq.return (p, r, supp, dl', m')


(* examples  / pairs *)
             
type task_model =
  { input_model : row model; (* no reference *)
    output_model : row model
  }

let init_task_model (t : Task.task) =
  { input_model = row_model0 (Task.input_row_size t);
    output_model = row_model0 (Task.output_row_size t) }

let xp_task_model (print : Xprint.t) (m : task_model) =
  xp_model print ~ctx:ctx0 m.input_model;
  print#string " ➜ ";
  xp_model print ~ctx:ctx0 m.output_model
let pp_task_model = Xprint.to_stdout xp_task_model
let string_of_task_model = Xprint.to_string xp_task_model

             
type pairs_reads = (* result of reading a list of pairs of grids *)
  { dl_mi : dl; (* input model DL *)
    dl_mo : dl; (* output model DL *)
    inputs_reads : row read list list; (* outer list over example inputs, inner list over parses *)
    reads : (row read * row read * dl) list list; (* outer list over examples, inner list over parses, sorted in increasing DL *)
  }

let read_pairs ?(env = row_data0 0) (m : task_model) (pairs : Task.pair list) : pairs_reads result =
  Common.prof "Model.read_pairs" (fun () ->
  (* takes model, input env+docs, output docs *)
  let dl_mi = dl_model ~nb_env_paths:0 m.input_model in    
  let dl_mo = dl_model ~nb_env_paths:(dl_model_env_stats m.input_model) m.output_model in
  let| inputs_reads_reads =
    pairs
    |> list_map_result
         (fun {input; output} ->
           let| input_reads =
             read ~env m.input_model input in (* no diff allowed during training *)
           let| pair_reads = 
             let+|+ ri = Result.Ok input_reads in      
             let+|+ ro = read ~env:ri.data m.output_model output in
             let dl = ri.dl +. ro.dl in
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
        | (ri,ro,dl)::_ -> (ldi +. ri.dl, ldo +. ro.dl)
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

let split_pairs_read (prs : pairs_reads) : reads * reads =
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


let apply_model ?(env = env0) (m : task_model) (row_i : string list) : ((row data * string list) list, exn) Result.t =
  Common.prof "Model.apply_model" (fun () ->
  let+|+ read_i =
    read ~env m.input_model row_i in
  let| row_o =
    write ~env:read_i.data m.output_model in
  Result.Ok [(read_i.data, row_o)])

  
type task_refinement =
  | RInit
  | Rinput of row path * refinement * int (* support *) * dl (* estimated result DL *)
  | Routput of row path * refinement * int (* support *) * dl (* estimated result DL *)

let task_refinement_support = function
  | RInit -> (-1)
  | Rinput (_,_,supp,_) -> supp
  | Routput (_,_,supp,_) -> supp             

let rec xp_task_refinement (print : Xprint.t) = function
  | RInit -> print#string "init"
  | Rinput (p,ri,supp,dl') -> xp_task_refinement_aux print " In." p ri supp dl' "i"
  | Routput (p,ro,supp,dl') -> xp_task_refinement_aux print " Out." p ro supp dl' "o"
and xp_task_refinement_aux print in_out p r supp dl' i_o =
  print#string (Printf.sprintf " / ~%.3f%s)  " dl' i_o);
  print#string in_out;
  xp_row_path print p;
  print#string " ← ";
  xp_refinement print r;
  xp_support print supp
let pp_task_refinement = Xprint.to_stdout xp_task_refinement
let string_of_task_refinement = Xprint.to_string xp_task_refinement

let apply_task_refinement (r : task_refinement) (m : task_model) : (task_refinement * task_model) result =
  match r with
  | RInit -> Result.Error (Failure "apply_refinement")
  | Rinput (p,ri,supp,dl') ->
     Result.Ok (r, {m with input_model = apply_refinement ri p m.input_model})
  | Routput (p,ro,supp,dl') ->
     Result.Ok (r, {m with output_model = apply_refinement ro p m.output_model})

let task_refinements (last_r : task_refinement) (m : task_model) (prs : pairs_reads) (dsri : reads) (dsro : reads) : (task_refinement * task_model) Myseq.t =
  Myseq.concat (* TODO: rather order by estimated dl *)
    [ (let* p, ri, suppi, dli', mi = refinements ~nb_env_paths:0 ~dl_M:prs.dl_mi m.input_model dsri in
       Myseq.return (Rinput (p,ri,suppi,dli'), {m with input_model = mi}));
      (let* p, ro, suppo, dlo', mo = refinements ~nb_env_paths:(dl_model_env_stats m.input_model) ~dl_M:prs.dl_mo m.output_model dsro in
       Myseq.return (Routput (p,ro,suppo,dlo'), {m with output_model = mo})) ]


(* learning *)

let learn_model
      ?(verbose = false)
      ?(pause = 0.)
      ~timeout
      ~init_model
      ~beam_width ~refine_degree
      (pairs : Task.pair list)
    : ((task_refinement * task_model) * (pairs_reads * reads * reads) * dl) list * bool
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
	 pp_task_refinement r; print_newline ();
         pp_task_model m; print_newline ();
	 raise exn)
    ~code:(fun (r,m) (prs,gsri,gsro) ->
	   let (lmi,lmo,lm), (ldi,ldo,ld), (_lmdi,_lmdo,lmd) =
	     norm_dl_model_data prs in
           if verbose then (
             Printf.printf "\t?? %.3f\t" lmd;
             pp_task_refinement r; print_newline ();
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
      Printf.printf "%.3f\t" dl; pp_task_refinement r; print_newline ();
      if verbose then (
        print_endline " ===> first read for first example";
        List.hd (List.hd prs.reads)
        |> (fun (read_i, read_o, dl) ->
          print_endline " --- some read ---";
          pp_data read_i.data; print_newline ();
          pp_data read_o.data; print_newline ();
          Printf.printf "\tdl=%.1f\n" dl);
        print_newline ());
        (*pp_grids_read "### OUT grids_read ###" gsro;*)
      (*Printf.printf "    l = %.1f = %.1f + %.1f = (%.1f + %.1f) + (%.1f + %.1f)\n" lmd lm ld lmi lmo ldi ldo;*)
      flush stdout;
      let refs = task_refinements r m prs gsri gsro in
      refs))

