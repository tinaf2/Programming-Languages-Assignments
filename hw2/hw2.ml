type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
  
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* 1 *)
let convert_grammar gram1 = let (start_symbol, rules) = gram1 in
    let rec production_rules nt rules = match rules with
        | [] -> []
        | (lhs, rhs)::tail -> 
            if lhs = nt then rhs :: (production_rules nt tail)
            else production_rules nt tail
    in
    (start_symbol, fun nt -> production_rules nt rules)

(* 2 *)
let rec parse_tree_leaves tree = match tree with
  | Leaf terminal -> [terminal]
  | Node (_, subtrees) -> 
      List.flatten (List.map parse_tree_leaves subtrees)

(* 3 *)

let rec make_matcher gram = let (start_symbol, prod_func) = gram in
  let rec check_rules alt_list accept frag = match alt_list with
    | [] -> None
    | rule::rest -> match (check_rule rule accept frag) with
                      | None -> check_rules rest accept frag
                      | Some x -> Some x

  and check_rule current_rule accept frag = match current_rule with
    | [] -> accept frag
    | sym::rest_syms -> match sym with
                    | N nt -> 
                      let curried_acceptor = check_rule rest_syms accept in
                        check_rules (prod_func nt) curried_acceptor frag
                    | T t -> check_sym rest_syms t accept frag
                    
  and check_sym rest_rule_syms t accept frag = match frag with
                          | [] -> None 
                          | head::fragrest -> match (head = t) with
                                           | true -> check_rule rest_rule_syms accept fragrest
                                           | false -> None
  in
  (fun accept frag -> check_rules (prod_func start_symbol) accept frag)

(* 4 *)

let parser_acceptor frag tree = 
  match frag with
    | [] -> Some tree
    | _ -> None

let rec make_parser gram = let (start_symbol, prod_func) = gram in
  let rec check_rules current_symbol alt_list accept frag subtrees = match alt_list with
    | [] -> None
    | rule::rest -> match (check_rule current_symbol rule accept frag subtrees) with
                      | None -> check_rules current_symbol rest accept frag subtrees
                      | Some x -> Some x

and check_rule current_symbol current_rule accept frag subtrees = match current_rule with
  | [] -> accept frag (Node(current_symbol, subtrees))
  | sym::rest_syms -> match sym with
                      | N nt ->
                        let curried_acceptor frag2 future_tree = check_rule current_symbol rest_syms accept frag2 (subtrees @ [future_tree]) in
                          check_rules nt (prod_func nt) curried_acceptor frag []
                      | T t -> check_sym current_symbol rest_syms t accept frag subtrees

and check_sym current_symbol rest_rule_syms t accept frag subtrees = match frag with
                            | [] -> None
                            | head::fragrest -> match (head = t) with
                                                  | true -> check_rule current_symbol rest_rule_syms accept fragrest (subtrees @ [Leaf t])
                                                  | false -> None

in
(fun frag -> check_rules start_symbol (prod_func start_symbol) parser_acceptor frag [])

