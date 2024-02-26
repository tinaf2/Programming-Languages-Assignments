(*1*)
let rec subset a b = 
  match a with
  | [] -> true
  | x :: rest ->
    if List.mem x b then subset rest b
    else false;;

(*2*)
let equal_sets a b =
  (subset a b) && (subset b a);;

(*3*)
let rec set_union a b = 
  match a with
    | [] -> b
    | x :: rest -> x :: set_union rest b    

(*4*)
let set_all_union sets = 
  List.fold_left set_union [] sets

(*5*)
(* Writing a function `self_member s` in OCaml would be impossible due to Russell's Paradox, which arises from considering 
the set of all sets that do not contain themselves. A set cannot contain itself, because a set contains only the things it has inside of 
it, so a set and the things inside of it are logically different things. In the context of OCaml, sets are typically represented as lists. 
However, a list in OCaml cannot contain itself due to the language's strict type system. A list is defined by its elements, and these 
elements cannot be of the same type as the list itself, thus preventing a list from being an element of itself. Attempting to create a 
function that checks if a set is a member of itself would result in type errors, since OCaml's type system does not allow this kind of 
self-referential structure. This limitation reflects the logical inconsistency highlighted by Russell's Paradox, where a set being a 
member of itself leads to a contradiction. *)


(*6*)
let rec computed_fixed_point eq f x = 
  match (eq (f x) x) with
  | true -> x
  | _ -> computed_fixed_point eq f (f x)

(*7*)
let rec apply_p_times f p x = 
  match (p = 0) with
  | true -> x
  | _ -> apply_p_times f (p - 1) (f x)

let rec computed_periodic_point eq f p x =
  match (p <= 0) with
  | true -> x
  | _ -> match (eq x (apply_p_times f p x)) with
          | true -> x
          | _ -> computed_periodic_point eq f p (f x)

(*8*)
let rec whileseq s p x =
  match (p x) with
    | true -> x :: whileseq s p (s x)
    | false -> []

(*9*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let filter_blind_alleys (start_symbol, rules) =
  let init_dict = 
    List.fold_left (fun acc (lhs, _) -> if List.mem_assoc lhs acc then acc else (lhs, false) :: acc) [] rules
  in

  let rec update_dict dict =
    let updated_dict = List.map (fun (nt, can_term) ->
      let can_term_now = 
        List.exists (fun (lhs, rhs) -> lhs = nt && List.for_all (fun sym ->
          match sym with
          | N n -> (match List.assoc_opt n dict with Some b -> b | None -> false)
          | T _ -> true) rhs) rules
      in
      (nt, can_term || can_term_now)) dict
    in
    if updated_dict = dict then dict else update_dict updated_dict
  in

  let final_dict = update_dict init_dict in

  let filtered_rules = 
    List.filter (fun (lhs, rhs) ->
      (match List.assoc_opt lhs final_dict with Some b -> b | None -> false) &&
      List.for_all (function 
        | N n -> (match List.assoc_opt n final_dict with Some b -> b | None -> false) 
        | T _ -> true) rhs) rules
  in

  (start_symbol, filtered_rules)











         
                