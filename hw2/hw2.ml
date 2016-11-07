type ('nonterminal, 'terminal) symbol = 
	| N of 'nonterminal
	| T of 'terminal;;

(*1. Write a function 'convert_grammar gram1' that returns a Homework 2-style grammar, 
which is converted from the Homework 1-style grammar gram1*)
let rec get_rule_list rules start_symbol = match rules with
	| [] -> []
	| (lh, rh)::t -> 
		if 	 lh = start_symbol  then [rh]@(get_rule_list t start_symbol)
		else get_rule_list t start_symbol;;

let convert_grammar gram1 = 
	match gram1 with
	| (start_symbol, rules) -> (start_symbol, (get_rule_list rules));;

(*helper functions for 'parse_prefix  gram'*)
let rec and_matcher lst rule accept deriv frag = match rule with
	| [] -> accept deriv frag (* all rules have been matched with the matcher *)
	| _ -> match frag with
		| [] -> None 		(*all fragment has been checked*)
		| h::t -> match rule with 
			| (N nonterm)::tail -> 	(*current symbol is nonterminal, get the matchers for this symbol*)
				(or_matcher nonterm lst (lst nonterm) (and_matcher lst tail accept) deriv frag)
			| (T term)::tail -> (*current symbol is terminal, use matcher to match and move to the rest symbols*)
				if h = term then (and_matcher lst tail accept deriv t)
				else None
		

and or_matcher start_symbol rules rhs accept deriv frag = match rhs with
		|[] -> None
		| h::t -> 
			match (and_matcher rules h accept (deriv@[ (start_symbol, h) ]) frag) with
 			| Some(d, s) -> Some(d, s)	(* current matcher works, simply return and_matcher result*)
			| None -> (or_matcher start_symbol rules t accept deriv frag) 
			(* current matcher does not work, move to next available matcher in the list*)


(*2. Write a function parse_prefix gram that returns a matcher for the grammar gram*)
let parse_prefix gram accept frag = 
	match gram with
	| (start_symbol, rules) -> or_matcher start_symbol rules (rules start_symbol) accept [] frag;;