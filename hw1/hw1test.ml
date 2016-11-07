let my_subset_test0 = subset [1;1] [1;2;3]
let my_subset_test1 = not (subset [4;3] [1;2;3])
let my_subset_test2 = subset [3;1] [4;1;3]

let my_equal_sets_test0 = not (equal_sets [1;3] [3;2])
let my_equal_sets_test1 = equal_sets [1;3;4] [3;1;3;4;4]

let my_set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [4;5;3] [1;2;3]) [1;2;3;4;5]
let my_set_union_test2 = equal_sets (set_union [2;3] []) [2;3]

let my_set_intersection_test0 =
  equal_sets (set_intersection [3;2;1;1] [1;2;3]) [1;2;3]
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;4;2] [1;2;3]) [2;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [1;1;1;1] [3;1;2;4]) [1]

let my_set_diff_test0 = equal_sets (set_diff [1;1] [1;4;3;1]) []
let my_set_diff_test1 = equal_sets (set_diff [2;3;4;1] [1;3]) [2;4]
let my_set_diff_test2 = equal_sets (set_diff [3;3] []) [3]
let my_set_diff_test3 = equal_sets (set_diff [] [3]) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x *. 4.) 1. = infinity
 

let my_computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x * x - 1) 2 0 = 0

type my_nonterminals =  
 | Apple | Banana | Cat | A | B | C 

let myrules = 
  [
   Apple, [T"1"];
   Apple, [N Banana];
   Banana, [N Banana; N Cat];
   Cat, [N Apple; T"b"];
   A, [N Apple; N Banana; T"cc"];
   A, [N Apple];
   A, [T"dd"; T"ee"];
   A, [N Banana; N Cat];
   B, [N A; N Apple; N Banana];
   B, [N C];
   B, [N Apple; T"b"];
   C, [N A; N Cat];
   C, [T"vv"];
   C, [T"p"; N B];

  ]


  let mygrammar0 = Apple, myrules

  let mygrammar1 = Banana, List.tl myrules

  let myrule_test0 = filter_blind_alleys mygrammar0 
  = (Apple,
   [Apple, [T"1"];
   Cat, [N Apple; T "b"];
   A, [N Apple];
   A, [T"dd"; T "ee"];
   B, [N C];
   B, [N Apple; T"b"];
   C, [N A; N Cat];
   C, [T"vv"];
   C, [T"p"; N B]])
  let myrule_test1 = filter_blind_alleys mygrammar1 
  = (Banana,  
   [A, [T"dd"; T "ee"];
   B, [N C];
   C, [T"vv"];
   C, [T"p"; N B]])
 

 

