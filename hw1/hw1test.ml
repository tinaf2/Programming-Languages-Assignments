let subset_test0 = subset [] []
let subset_test1 = not (subset [1] [])
let subset_test2 = subset [2; 3] [1; 2; 3; 4]
let subset_test3 = not (subset [5; 6] [1; 2; 3; 4])

let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [2; 2; 4] [4; 2]
let equal_sets_test2 = not (equal_sets [1; 2] [])
let equal_sets_test3 = not (equal_sets [1; 2; 3] [4; 5; 6])
let equal_sets_test4 = equal_sets [3; 3; 3; 3] [3]

let set_union_test0 = equal_sets (set_union [1; 2] [3; 4]) [1; 2; 3; 4]
let set_union_test1 = equal_sets (set_union [5; 6] [5; 7; 8]) [5; 6; 7; 8]
let set_union_test2 = equal_sets (set_union [9; 10] []) [9; 10]
let set_union_test3 = equal_sets (set_union [14; 15] [16; 15; 14]) [14; 15; 16]
let set_union_test4 = equal_sets (set_union [17; 18; 19] [19; 20]) [17; 18; 19; 20]

let set_all_union_test0 = equal_sets (set_all_union [[1; 2]; [3; 4]]) [1; 2; 3; 4]
let set_all_union_test1 = equal_sets (set_all_union [[]; [5; 6]; []; [7]]) [5; 6; 7]
let set_all_union_test2 = equal_sets (set_all_union [[8; 9]; [10; 9]; [11]]) [8; 9; 10; 11]

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 4) 550 = 0

let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x / 3) 0 5 = 5

let whileseq_test0 =
  whileseq (( * ) 2) ((>) 100) 1 = [1; 2; 4; 8; 16; 32; 64];;

type transport_nonterminals =
  | Motorcycle | Bicycle | Car | Truck | Plane | Helicopter

let transport_grammar =
  Motorcycle,
  [Truck, [T"ZZZ"];
   Helicopter, [];
   Car, [T"khrgh"];
   Plane, [T"aooogah!"];
   Bicycle, [N Helicopter];
   Bicycle, [N Car];
   Bicycle, [N Plane];
   Motorcycle, [N Truck];
   Motorcycle, [N Bicycle; T"or"; N Motorcycle]]


let transport_test0 =
  filter_blind_alleys transport_grammar = transport_grammar

let transport_test1 =
  filter_blind_alleys (Bicycle, List.tl (List.tl (List.tl (snd transport_grammar)))) =
    (Bicycle,
     [Plane, [T "aooogah!"];
      Bicycle, [N Plane]]) 

      
