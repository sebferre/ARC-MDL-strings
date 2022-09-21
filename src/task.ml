
type pair = { input : string list; output : string list }
type task = { train : pair list; test : pair list }

let input_row_size (t : task) : int =
  match t with
  | { train = { input } :: pairs1; test = pairs2 } ->
     let size = List.length input in
     if List.for_all (fun {input} -> List.length input = size) (pairs1 @ pairs2)
     then size
     else invalid_arg "Task.input_row_size: inconsistent row size"
  | _ -> invalid_arg "Task.input_row_size: no training example"
          
let output_row_size (t : task) : int =
  match t with
  | { train = { output } :: pairs1; test = pairs2 } ->
     let size = List.length output in
     if List.for_all (fun {output} -> List.length output = size) (pairs1 @ pairs2)
     then size
     else invalid_arg "Task.output_row_size: inconsistent row size"
  | _ -> invalid_arg "Task.output_row_size: no training example"
          
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
     { input = string_list_of_json input;
       output = string_list_of_json output }
  | _ -> invalid_arg "Invalid JSON pair"
and string_list_of_json : Yojson.Safe.t -> string list = function
  | `String s -> [s]
  | `List l -> List.map string_of_json l
  | _ -> invalid_arg "Invalid JSON string list"
and string_of_json : Yojson.Safe.t -> string = function
  | `String s -> s
  | _ -> invalid_arg "Invalid JSON document"
       
let from_file (filename : string) : task =
  let json = Yojson.Safe.from_file filename in
  let task = task_of_json json in
  task

