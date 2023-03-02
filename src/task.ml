
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

let task_of_csv (ch_csv : Csv.in_channel) : task =
  (* assuming no header *)
  (* assuming the last column is the output *)
  (* assuming the rows with empty last column to be test instances *)
  (* TODO: find a way to get more information from CSV files *)
  let train, test =
    Csv.fold_right
      ~f:(fun line (train,test) ->
        match List.rev line with
        | [] -> train, test (* empty line *)
        | y::rev_xs ->
           let example = { input = List.rev rev_xs; output = [y] } in
           if y = ""
           then train, example::test
           else example::train, test)
      ch_csv ([],[]) in
  Csv.close_in ch_csv;
  { train; test }

let from_filename_contents (filename : string) (contents : string) : task =
  if Filename.check_suffix filename ".json" then
    let json = Yojson.Safe.from_string contents in
    task_of_json json
  else if Filename.check_suffix filename ".csv" then
    let ch_csv = Csv.of_string contents in
    task_of_csv ch_csv
  else failwith "Unexpected task file format"
       
let from_file (filename : string) : task =
  if Filename.check_suffix filename ".json" then
    let json = Yojson.Safe.from_file filename in
    task_of_json json
  else if Filename.check_suffix filename ".csv" then
    (try
       let ch = Stdlib.open_in filename in
       let ch_csv = Csv.of_channel ch in
       task_of_csv ch_csv
     with exn ->
       print_endline (Printexc.to_string exn);
       failwith "The file could not be open")
  else failwith "Unexpected task file format"

