
(* === command line options === *)
             
let training = ref true (* should be set to false on evaluation set *)
let start_rank = ref max_int
let timeout_build = ref 60
let timeout_prune = ref 10
let timeout_predict = ref 10
let verbose = ref 1
let viz = ref false
let pause = ref 0.

(* === printing and checking functions === *)

(* let print_l_gmd name gr = (* grid model+data DL *)
  let lm, ld, lmd = Model.dl_grid_model_data gr in
  Printf.printf "DL %s: L = %.1f + %.1f = %.1f\n" name lm ld lmd *)
		   
let print_l_md psr = (* model+data DL *)
  let (lmi,lmo,lm), (ldi,ldo,ld), (lmdi, lmdo, lmd) =
    Model.dl_model_data psr in
  Printf.printf "DL input  with Mi: L = %.1f + %.1f = %.1f\n" lmi ldi lmdi;
  Printf.printf "DL output with Mo: L = %.1f + %.1f = %.1f\n" lmo ldo lmdo;
  Printf.printf "DL input+output M: L = %.1f + %.1f = %.1f\n" lm ld lmd;
  ldo

let print_l_task_model name task model =
  Model.read_pairs model task.Task.train
  |> Result.fold
       ~ok:(fun psr -> ignore (print_l_md psr))
       ~error:(fun exn -> raise exn)

(* === monitoring learning === *)

type measures = (string * [`Tasks|`Bits|`MRR|`Seconds] * float) list

let print_measures count ms =
  List.iter
    (fun (a,t,v) ->
     match t with
     | `Tasks -> Printf.printf "%s = %.2f tasks (%.2f%%)\n" a v (100. *. v /. float count)
     | `Bits -> Printf.printf "%s = %.1f bits (%.1f bits/task)\n" a v (v /. float count)
     | `MRR -> Printf.printf "%s = %.2f\n" a (v /. float count)
     | `Seconds -> Printf.printf "%s = %.1f sec (%.1f sec/task)\n" a v (v /. float count))
    ms;
  print_newline ()

let apply_model m row =
  let res_opt =
    Common.do_timeout !timeout_predict (fun () ->
        Model.apply_model m row) in
  match res_opt with
  | Some res -> res
  | None -> Result.Error (Failure "The model could not be applied in the allocated timeout.")
  
let score_learned_model name (m : Model.task_model) (train_test : [`TRAIN of Model.pairs_reads |`TEST]) examples : float * float * float (* micro success, macro success, MRR *) =
  let _, nb_ex, nb_correct, sum_rrank =
    List.fold_left
      (fun (i,nb_ex,nb_correct,sum_rrank) {Task.input; output} ->
        Printf.printf "\n## instance %d\n" i;
        if !training then (
          match train_test with
          | `TRAIN psr ->
             print_endline "\n> Input and output best reading:";
             let reads_pair = try List.nth psr.reads (i-1) with _ -> assert false in
             Common.sub_list reads_pair 0 1
             |> List.iter (fun (gri, gro, _) ->
                    let {Model.env=envi; data=gdi; dl=dli} = gri in
                    let {Model.env=envo; data=gdo; dl=dlo} = gro in
                    print_newline ();
                    Model.pp_data gdi; Printf.printf "   (%.1f bits)\n" dli;
                    Model.pp_data gdo; Printf.printf "   (%.1f bits)\n" dlo)
                    (* TODO if !viz then
                      Grid.pp_grids [Model.grid_of_data_as_template gdi.data;
                                     Model.grid_of_data_as_template gdo.data]) *)
          | `TEST -> ()
        );
        if !verbose >= 2 then (
          print_endline "\n> Best input readings:";
          let input_reads =
            match train_test with
            | `TRAIN psr -> Result.Ok (List.nth psr.Model.inputs_reads (i-1))
            | `TEST -> Model.read
                         ~env:Model.env0 ~bindings:Model.bindings0
                         m.Model.input_model input in
          ( match input_reads with
            | Result.Ok reads ->
               List.iter
                 (fun {Model.data=gdi; dl=dli} ->
                   Model.pp_data gdi;
                   Printf.printf "  (%.1f bits)\n" dli)
                 reads
               (* TODO if !grid_viz then (
                 Grid.pp_grids
                   (List.map
                      (fun (_,gdi,_) -> Model.grid_of_data_as_template gdi.Model.data)
                      reads)) *)
            | Result.Error _ ->
               print_endline "(No input readings)"
          ));
        print_endline "\n> Output prediction from input (up to 3 trials):";
        let score, rank, label, _failed_derived_grids =
	  match apply_model m input with
	  | Result.Ok gdi_derived_s ->
             List.fold_left
               (fun (score,rank,label,failed_derived_grids) (gdi, derived) ->
                 if score=1 then
                   score, rank, label, failed_derived_grids (* already success *)
                 else if rank > 3 then
                   score, rank, label, failed_derived_grids (* at most 3 trials *)
                 else if List.mem derived failed_derived_grids then
                   score, rank, label, failed_derived_grids (* already derived that grid and it failed *)
                 else ( (* score=0 && rank <= 3 && new derived_grid *)
                   Printf.printf ">> Trial %d\n" rank;
                   if !training then (
                     Model.pp_data gdi;
                   (* TODO if !grid_viz then Grid.pp_grids [Model2.grid_of_data_as_template gdi.data] *)
                   );
	           ( if derived = output
		     then (
                       if !training then (
                         print_endline "correct output grid"
                       (* TODO if !viz then Grid.pp_grids [derived] *)
                       );
                       1, rank, "SUCCESS", failed_derived_grids
                     )
		     else (
                        (* TODO if !training then (
                          print_grid_diff ~grid:output ~derived_grid:derived diff); *)
                       0, rank+1, "FAILURE", derived::failed_derived_grids )
                   )
               ))
               (0,1,"FAILURE",[]) gdi_derived_s
	  | Result.Error exn ->
             print_endline (Printexc.to_string exn);
             0, 0, "ERROR", [] in
        let tt = match train_test with `TRAIN _ -> "TRAIN" | `TEST -> "TEST" in
        let str_rank =
          if score = 0
          then "-"
          else
            match rank with
            | 1 -> "1st"
            | 2 -> "2nd"
            | 3 -> "3rd"
            | _ -> assert false in
	Printf.printf "\n%s %s/%d: %d %s (%s)\n" tt name i score str_rank label;
	i+1,
        nb_ex+1,
        nb_correct+score,
        (if score=1 then sum_rrank +. 1. /. float rank else sum_rrank))
      (1,0,0,0.) examples in
  let micro = float nb_correct /. float nb_ex in
  let macro = if nb_correct = nb_ex then 1. else 0. in
  let mrr = sum_rrank /. float nb_ex in
  micro, macro, mrr
       
let print_learned_model ~init_model ~refine_degree name (task : Task.task) : measures =
  Common.prof "Test.print_learned_model" (fun () ->
  let runtime, res =
    Common.chrono (fun () ->
        Model.learn_model
          ~verbose:(!training && !verbose > 1)
          ~pause:(!pause)
          ~timeout_build:(!timeout_build)
          ~timeout_prune:(!timeout_prune)
          ~init_model
          ~beam_width:1 ~refine_degree
          task.train)
  in
  match res with
  | Common.Exn exn ->
      print_endline (Printexc.to_string exn);
      Printexc.print_backtrace stdout;
      let ms =
        [ "runtime-learning", `Seconds, runtime;
          "bits-train-error", `Bits, 0.; (* dummy value *)
	  "acc-train-micro", `Tasks, 0.;
	  "acc-train-macro", `Tasks, 0.;
          "acc-train-mrr", `MRR, 0.;
	  "acc-test-micro", `Tasks, 0.;
	  "acc-test-macro", `Tasks, 0.;
          "acc-test-mrr", `MRR, 0.;
        ] in
      ms    
  | Common.Val ((m_build,psr_build,timed_out_build), (m_prune,psr_prune,timed_out_prune)) ->
     if timed_out_build then print_endline "TIMEOUT";
     print_endline "\n# Learned model (decriptive, before pruning):";
     Model.pp_task_model m_build;
     print_newline ();
     let _ = print_l_md psr_build in
     print_endline "\n# Learned model (predictive, after pruning):";
     Model.pp_task_model m_prune;
     print_newline ();
     let ldo = print_l_md psr_prune in
     print_endline "\n# train input/output grids";
     let micro_train, macro_train, mrr_train =
       score_learned_model name m_prune (`TRAIN psr_prune) task.train in
     print_endline "\n# Test input/output grids";
     let micro_test, macro_test, mrr_test =
       score_learned_model name m_prune (`TEST) task.test in
     print_endline "\n# Performance measures on task";
     let ms =
       [ "runtime-learning", `Seconds, runtime;
         "bits-train-error", `Bits, ldo;
	 "acc-train-micro", `Tasks, micro_train;
	 "acc-train-macro", `Tasks, macro_train;
         "acc-train-mrr", `MRR, mrr_train;
	 "acc-test-micro", `Tasks, micro_test;
	 "acc-test-macro", `Tasks, macro_test;
         "acc-test-mrr", `MRR, mrr_test;
       ] in
     print_measures 1 ms;
     ms)
               
(* === solved/candidate training tasks === *)
		      
let flashfill_dir = "/local/ferre/data/tasks/FlashFill/"
let gulwani_dir = flashfill_dir ^ "Gulwani2011_examples/"
let gulwani_names = (* List.sort Stdlib.compare (Array.to_list (Sys.readdir gulwani_dir)) *)
  [ "example1.json";
    "example2.json";
    "example3.json";
    "example4.json";
    "example5.json";
    "example6.json";
    "example7.json";
    "example8.json";
    "example9.json";
    "example10.json";
    "example11.json";
    "example12.json";
    "example13.json";
    "example14.json" ]
let fokou_dir = flashfill_dir ^ "Fokou2022/"
let fokou_names = List.sort Stdlib.compare (Array.to_list (Sys.readdir fokou_dir))

(* === main === *)
  
let task_of_name dir name = Task.from_file (dir ^ name)

class type checker =
  object
    method process_task : string -> Task.task -> unit
    method summarize_tasks : unit
  end
					   
let main_tasks (dir : string) (names : string list) (checker : checker) : unit =
  print_endline "## options";
  Printf.printf "alpha = %.1f\n" !Model.alpha;
  Printf.printf "mode = %s\n" (if !training then "training" else "evaluation");
  Printf.printf "timeout_build = %d\n" !timeout_build;
  Printf.printf "timeout_prune = %d\n" !timeout_prune;
  Printf.printf "timeout_predict = %d\n" !timeout_predict;
  print_newline ();
  let nb_tasks = List.length names in
  let _ =
    List.fold_left
      (fun rank name ->
        if rank <= !start_rank then (
          let task = task_of_name dir name in
          print_endline "=====================================";
          Printf.printf "[-%d] Checking task %s: %d train, %d test\n"
	    rank name
	    (List.length task.train)
	    (List.length task.test);
          checker#process_task name task
        );
       rank-1)
      nb_tasks names in
  checker#summarize_tasks;
  Common.prerr_profiling ()

class checker_model ~(get_init_model : string -> Task.task -> Model.task_model) ~refine_degree : checker =
  object
    val mutable count = 0
    val mutable sum_ms = []

    method process_task name task =
      (* TODO if !viz then (
        print_endline "\n# train pairs";
        List.iter
          (fun {input; output} ->
            
          task.train ); *)
      let init_model =
        try get_init_model name task
        with Not_found -> Model.init_task_model task in
      if refine_degree <> 0 then ( (* only for learning, redundant for analyzing *)
        print_endline "\n# evaluating init_model";
        print_l_task_model name task init_model;
        print_endline "\n# learning a model for train pairs"
      );
      let ms = print_learned_model ~init_model ~refine_degree name task in
      count <- count+1;
      sum_ms <-
	if sum_ms = []
	then ms
	else
	  List.map2
	    (fun (a,t,sum_v) (a',t',v) ->
	     assert (a=a' && t=t');
	     (a,t,sum_v+.v))
	    sum_ms ms

    method summarize_tasks =
      Printf.printf "\n## performance measures averaged over %d tasks\n" count;
      print_measures count sum_ms;
      flush stdout
  end

let checker_learning = new checker_model
                         ~get_init_model:(fun name task -> Model.init_task_model task)
                         ~refine_degree:(!Model.max_refinements)

(* TODO let checker_apply = new checker_model
                         ~get_init_model:(fun name task -> List.assoc name task_model)
                         ~refine_degree:0 *)
                
let _ =
  let () = Printexc.record_backtrace true in
  let dir = ref gulwani_dir in
  let names = ref gulwani_names in
  let checker = ref checker_learning in
  Arg.parse
    ["-gulwani", Unit (fun () -> dir := gulwani_dir; training := true; names := gulwani_names), "Use Gulwani's tasks (default)";
     "-fokou", Unit (fun () -> dir := fokou_dir; training := false; names := fokou_names), "Use Fokou's tasks";
     "-all", Unit (fun () -> ()), "Use all tasks in the chosen set (default)";
     "-sample",
        Int (fun n -> Random.self_init (); names := Common.list_sample ~size:n !names),
	"Use the first N tasks in the chosen set";
     (*     "-solved", Unit (fun () -> names := solved_train_names), "Use short list of solved training tasks"; *)
     "-tasks",
     String (fun ids ->
	     let ids = String.split_on_char ',' ids in
	     names :=
	       !names
	       |> List.filter
		    (fun name ->
                      Filename.check_suffix name ".json"
		      && ids
		         |> List.exists
			      (fun id ->
                                let n_id = String.length id in
			        n_id <= String.length name && String.sub name 0 n_id = id))),
     "Use the tasks specified by their prefix (comma-separated)";
     "-r", Set_int start_rank, "Start processing with this task rank (only useful for error recovery)";
     "-learn", Unit (fun () -> checker := checker_learning), "Perform learning on chosen tasks (default)";
     (* TODO     "-apply", Unit (fun () -> checker := checker_apply), "Apply pre-defined models to the chosen tasks (Model.init_model by default)"; *)
     "-alpha", Set_float Model.alpha, "Multiplication factor over examples in DL computations (default: 10)";
     "-timeout_build", Set_int timeout_build, "Learning/building timeout per task (default: 30s)";
     "-timeout_prune", Set_int timeout_prune, "Learning/pruning timeout per task (default: 10s)";
     "-viz", Set viz, "Show train strings, as understood by the model";
     "-pause", Set_float pause, "Time delay (in seconds, default=0.0) at each step during learning, useful in combination with -viz";
     "-v", Set_int verbose, "Verbose mode";
    ]
    (fun str -> ())
    "test [-gulwani|-fokou] [-all|-sample N|-tasks ID,ID,...] [-r N] [-learn|-apply] [-alpha N] [-timeout N] [-viz [-pause T]] [-v N]";
  main_tasks !dir !names !checker
