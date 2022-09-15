(* closed and upper-open intervals over natural integers *)

type t =
  | Closed of int * int (* lower and upper bound *)
  | Open of int (* no upper bound *)

let make_closed a b = Closed (a,b)
let make_open a = Open a
let make_exact a = Closed (a,a)

let lower = function
  | Closed (a,b) -> a
  | Open a -> a

let upper = function
  | Closed (a,b) -> Some b
  | Open _ -> None

let cardinal : t -> int option = function
  | Closed (a,b) -> Some (b - a + 1)
  | Open _ -> None

let dl (x : int) (r : t) : Mdl.bits =
  match r with
  | Closed (a,b) ->
     assert (x >= a && x <= b);
     Mdl.Code.uniform (b - a + 1)
  | Open a ->
     assert (x >= a);
     Mdl.Code.universal_int_star (x - a)
            
let add r1 r2 =
  match r1, r2 with
  | Closed (a1,b1), Closed (a2,b2) -> Closed (a1+a2, b1+b2)
  | Closed (a1,_), Open a2 -> Open (a1+a2)
  | Open a1, Closed (a2,_) -> Open (a1+a2)
  | Open a1, Open a2 -> Open (a1+a2)
let sum = function
  | [] -> Open 0
  | r::lr -> List.fold_left add r lr

let sub r1 r2 =
  match r1, r2 with
  | Closed (a1,b1), Closed (a2,b2) ->
     if b1 - a2 < 0 then failwith "sub: result range is void";
     Closed (max 0 (a1 - b2), b1 - a2)
  | Closed (a1,b1), Open a2 ->
     if b1 - a2 < 0 then failwith "sub: result range is void";
     Closed (0, b1 - a2)
  | Open a1, Closed (a2,b2) -> Open (max 0 (a1 - b2))
  | Open a1, Open a2 -> Open 0

let inter r1 r2 =
  match r1, r2 with
  | Closed (a1,b1), Closed (a2,b2) -> Closed (max a1 a2, min b1 b2)
  | Closed (a1,b1), Open a2 -> Closed (max a1 a2, b1)
  | Open a1, Closed (a2,b2) -> Closed (max a1 a2, b2)
  | Open a1, Open a2 -> Open (max a1 a2)
let inter_list = function
  | [] -> failwith "inter_list: empty list"
  | r::lr -> List.fold_left inter r lr
               
                          
              
