type sample_nonterminals =
  | Sentence | NP | Noun | Verb | Adjective

let prod_func = function
	| Sentence -> 
		[[N NP; N Verb; N NP]; 
		 [N NP; N Verb]]
	| NP ->
		[[N Adjective; N Noun];
		 [N Noun]]
	| Noun ->
		[[T "emacs"];
		 [T "Paul"; T "Eggert"]]
	| Verb ->
		[[T "adores"];
		 [T "loves"]]
	| Adjective ->
		[[T "smart"];
		 [T "knowledgable"];
		 [T "scholarly"]]

let sample_grammar = Sentence, prod_func

let sample_acceptor = function
	| [] -> None
	| x -> Some x

let sample_frag_1 = ["smart"; "Paul"; "Eggert"; "adores"; "emacs"]

let make_matcher_test = 
	((make_matcher sample_grammar sample_acceptor sample_frag_1)
		= Some ["emacs"])

let make_parser_test = 
	match make_parser sample_grammar sample_frag_1 with
	| Some tree -> parse_tree_leaves tree = sample_frag_1
	| _ -> false


