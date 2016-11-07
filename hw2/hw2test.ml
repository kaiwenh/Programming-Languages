(* test cases for 'conver_grammar' *)
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"]]

let test_convert_grammar_1 = ((snd (convert_grammar (Expr, awksub_rules))) Expr = [[T"("; N Expr; T")"]; [N Num]])
let test_convert_grammar_2 = ((snd (convert_grammar (Expr, awksub_rules))) Incrop = [[T"++"]; [T"--"]])

(*my test cases for 'parse_prefix'*)

let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None


type my_type =
  | Struct | S1 | T1 | S2 | T2 | D | E | F

let my_grammar = (Struct,
  function | Struct -> [[N S1; N T1]]
    | S1 -> [[N D; N S2]; [N S2]]
    | T1 -> [[N T2; N S1]; [N T2]; [N T2; N S1; N E]; [N T2; N E]]
    | S2 -> [[T"They"]; [T"I"]; [T"you"]; [T"taxi"]; [T"apple"]; [T"airport"]; [T"there"]]
    | T2 -> [[T"take"]; [T"come"]; [T"talk"]]
    | D -> [[T"the"]]
    | E -> [[N F; N S1]]
    | F -> [[T"from"]; [T"to"]])

let test_1 = ((parse_prefix my_grammar accept_empty_suffix ["They"; "come"; "from"; "there"])
  = Some
 ([(Struct, [N S1; N T1]); (S1, [N S2]); (S2, [T "They"]);
   (T1, [N T2; N E]); (T2, [T "come"]); (E, [N F; N S1]); (F, [T "from"]);
   (S1, [N S2]); (S2, [T "there"])], []))

let test_2 = ((parse_prefix my_grammar accept_empty_suffix ["I"; "take"; "taxi"; "to"; "the"; "airport"]) 
 = Some
 ([(Struct, [N S1; N T1]); (S1, [N S2]); (S2, [T "I"]);
   (T1, [N T2; N S1; N E]); (T2, [T "take"]); (S1, [N S2]);
   (S2, [T "taxi"]); (E, [N F; N S1]); (F, [T "to"]);
   (S1, [N D; N S2]); (D, [T "the"]); (S2, [T "airport"])], []))