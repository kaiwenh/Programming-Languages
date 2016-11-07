                                                                 
(*subset: check if a is a subset of b*)
let rec subset a b = match a with |[] -> true |h::t->if List.mem h b then (subset t b) else false;; 
(*equal_sets: check if two sets are equal*)
let equal_sets a b = subset a b && subset b a;;
(*set_union: make union of two lists, and the resulting list is sorted*)
let rec set_union a b =  match a with
    | [] -> List.sort compare b
    | h::t -> if List.mem h b then set_union t b
              else set_union t (h::b);;
(*set_intersection: find the intersection of sets a and b*)
let rec set_intersection a b = match a with 
	|[] -> [] 
	|h::t->if List.mem h b then h::(set_intersection t b) 
		   else set_intersection t b;;

(*set_diff: returns a list representing a−b*)
let rec set_diff a b = match a with 
	|[] -> []
	|h::t->if not (List.mem h b) then h::(set_diff t b) 
		   else set_diff t b;;

(*computed_fixed_point: returns the computed fixed point for f with respect to x, *)
let rec computed_fixed_point eq f x = if (eq (f x) x) then x  else (computed_fixed_point eq f (f x));; 

(*computed_periodic_point: returns the computed periodic point for f with period p and with respect to x*)
let rec computed_periodic_point eq f p x = match p with
	| 0 -> x
	| _ -> if eq x (f (computed_periodic_point eq f (p-1) (f x))) then x 
		   else (computed_periodic_point eq f p (f x));;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* check if subrule is good: the subrule can be derived to terminal symbols. *)
let check_subrule terminals rhs= 
	match rhs with
    | T s -> true (*all are terminal symbols*)
	| N s -> List.mem s terminals;; (* the symbol s is one terminal*)

(* check if a rule (non-terminal symbol and a list of rules) is valid: every rule in the list can reach a terminal *)
let rec check_rule terminals rule =
	match rule with
	| a::b -> 
	 	if (check_subrule terminals a) 
	 	then check_rule terminals b 
	    else false
	| [] -> true
	| _ -> false;; 


		
(* build up a set of rules which can reach terminals *)
let rec build_terminal_set terminals rules =
	match rules with
	| (a, b)::t -> 
		if (check_rule terminals b)
		then ( 
			if (List.mem a terminals) 
			then build_terminal_set terminals t 
			else build_terminal_set (a::terminals) t)
		else build_terminal_set terminals t
	| [] -> terminals;;


(* filter out blind-alley rules by checking if each rule is in the terminal set*)
let rec filter_bad_rules terminals rules = 
	match rules with 
	| [] -> []
	| (a, b)::t -> 
	    if (check_rule terminals b) 
		then (a, b)::(filter_bad_rules terminals t) 
		else filter_bad_rules terminals t;;	

(* define equal function for function 'computed fixed point'*)
let eq = fun (a, _) (b, _) -> equal_sets a b;;
(* define function to pass to computed_fixed_point *)
let f (terminals, rules) =
	((build_terminal_set terminals rules), rules);;

(* returns a copy of the grammar g with all blind-alley rules removed*)
let filter_blind_alleys  = function
	| (start_symbol, rules) -> 
	  (start_symbol, filter_bad_rules (fst(computed_fixed_point eq f ([], rules))
	) rules);; 

