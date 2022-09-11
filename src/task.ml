
type pair = { input : string; output : string }
type task = { train : pair list; test : pair list }

let rec task_of_json : Yojson.Safe.t -> task = function
  | `Assoc ["train", `List trains; "test", `List tests]
  | `Assoc ["test", `List tests; "train", `List trains] ->
     { train = List.map pair_of_json trains;
       test = List.map pair_of_json tests }
  | `Assoc ["train", `List trains] ->
     { train = List.map pair_of_json trains;
       test = [] }
  | _ -> invalid_arg "Invalid JSON task"
and pair_of_json : Yojson.Safe.t -> pair = function
  | `Assoc ["input", input; "output", output]
  | `Assoc ["output", output; "input", input] ->
     { input = string_of_json input;
       output = string_of_json output }
  | _ -> invalid_arg "Invalid JSON pair"
and string_of_json : Yojson.Safe.t -> string = function
  | `String s -> s
  | _ -> invalid_arg "Invalid JSON document"
               
let from_file (filename : string) : task =
  let json = Yojson.Safe.from_file filename in
  task_of_json json

