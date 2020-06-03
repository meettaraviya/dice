open Core
open Cudd
open Wmc
open CoreGrammar
open Lexing
open Lexer
open Passes
open Parser
open Random

let rec print_pretty e =
        match e with
        | `Int(sz, v) -> string_of_int v
        | `True -> "true"
        | `False -> "false"
        | `Tup(l, r) -> Format.sprintf "(%s, %s)" (print_pretty l) (print_pretty r)
        | _ -> failwith "ouch"

let get_joint_distribution_sample parsed sampled_vars = 
  let compiled = compile_program (CoreGrammar.from_external_prog parsed) ~sampled_vars:sampled_vars in
  let zbdd = compiled.body.z in
  let p = Wmc.wmc zbdd compiled.ctx.weights in
  let table = VarState.get_table compiled.body.state in
  let probs = List.map table ~f:(fun (label, bdd) ->
      let prob = (Wmc.wmc (Bdd.dand bdd zbdd) compiled.ctx.weights) /. p in
      (label, prob)) in
  (probs, p /. !(compiled.ctx.q))


let rec loop_get_joint_distribution parsed n_samples sampled_vars =
  let probs, importance = get_joint_distribution_sample parsed sampled_vars in
  let probs_weighted = List.map probs ~f:(fun (label, p) -> (label, p *. importance)) in
  if n_samples == 1 then
    probs_weighted, importance
  else
    let probs_sum, importance_sum = loop_get_joint_distribution parsed (n_samples - 1) sampled_vars in
    let probs_sum_new = List.map2_exn probs_weighted probs_sum ~f:(fun (l1, p_weighted) (l2, p_sum) ->
      (* Format.printf "%s %s\n" (print_pretty l1) (print_pretty l2); *)
      (* assert(l1 == l2); *)
      (l1, p_weighted +. p_sum)
    )
    in probs_sum_new, importance_sum +. importance

let get_joint_distribution parsed n_samples sampled_vars = 
  let probs_sum, importance_sum = loop_get_joint_distribution parsed n_samples sampled_vars in
  let probs_normalized = List.map probs_sum ~f:(fun (l1, p) -> (l1, p /. importance_sum)) in
  probs_normalized



let rec parse_and_print lexbuf n_samples sampled_vars =
  Random.self_init ();
  let parsed = Util.parse_with_error lexbuf in
  let probs = get_joint_distribution parsed n_samples sampled_vars in
  Format.printf "Value\tProbability\n";
  List.iter probs ~f:(fun (typ, prob) ->
      if prob >. 0.0 then Format.printf "%s\t%f\n" (print_pretty typ) prob;
    )
  (* Format.printf "Final compiled size: %d\n" (VarState.state_size [compiled.body.state]) *)
  (* let prob = CoreGrammar.get_prob (CoreGrammar.from_external_prog parsed) in
   * Format.printf "prob: %f\n" prob *)

let loop filename n_samples sampled_vars =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf n_samples sampled_vars;
  In_channel.close inx

(*let () =
  Command.run (Command.basic_spec ~summary:"Parse and compute prob"
  (* doc says: The old interface for command-line specifications -- Do Not Use. *)
    Command.Spec.(empty +> anon ("filename" %: string))
    loop)*)

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)
(* 
let command =
  Command.basic
    ~summary:"Parse and compute prob"
    (* ~readme:(fun () -> "More detailed information") *)
    (Command.Param.map filename_param ~f:(fun filename->
         (fun () -> loop filename))) *)

let command =
  Command.basic
    ~summary:"Parse and compute prob"
    ~readme:(fun () -> "More detailed information")
    (let open Command.Let_syntax in
     let open Command.Param in
     let%map
       filename  = anon ("filename" %: string)
      and n_samples = anon ("number of samples" %: int)
      and sampled_vars = anon (sequence ("sampled_vars" %: string))
     in
     fun () -> loop filename n_samples sampled_vars)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command