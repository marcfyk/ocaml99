open OUnit2
open Ocaml99.Exercises

let tests_last =
  "last"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (last []))
       ; ("singleton list" >:: fun _ -> assert_equal (Some 1) (last [ 1 ]))
       ; ("list with more than 1 element"
          >:: fun _ -> assert_equal (Some 2) (last [ 1; 3; 2 ]))
       ]
;;

let tests_last_two =
  "last_two"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (last_two []))
       ; ("singleton list" >:: fun _ -> assert_equal None (last_two [ 1 ]))
       ; ("list of 2 elements" >:: fun _ -> assert_equal (Some (2, 1)) (last_two [ 2; 1 ]))
       ; ("list with more than 2 elements"
          >:: fun _ -> assert_equal (Some (3, 2)) (last_two [ 1; 3; 2 ]))
       ]
;;

let tests_nth =
  "nth"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (nth [] 0))
       ; ("index == length of list" >:: fun _ -> assert_equal None (nth [ 1 ] 1))
       ; ("index > length of list" >:: fun _ -> assert_equal None (nth [ 1 ] 2))
       ; ("index < 0" >:: fun _ -> assert_equal None (nth [ 1 ] (-2)))
       ; ("0 <= index < length of list"
          >:: fun _ -> assert_equal (Some 3) (nth [ 1; 3; 2 ] 1))
       ]
;;

let tests_length =
  "length"
  >::: [ ("empty list" >:: fun _ -> assert_equal 0 (length []))
       ; ("non empty list" >:: fun _ -> assert_equal 3 (length [ 1; 3; 2 ]))
       ]
;;

let tests_reverse =
  "rev"
  >::: [ ("empty list" >:: fun _ -> assert_equal [] (rev []))
       ; ("non empty list" >:: fun _ -> assert_equal [ 2; 3; 1 ] (rev [ 1; 3; 2 ]))
       ]
;;

let tests_is_palindrome =
  "is_palindrome"
  >::: [ ("empty list" >:: fun _ -> assert_equal true (is_palindrome []))
       ; ("non empty and is a palindrome"
          >:: fun _ -> assert_equal true (is_palindrome [ 1; 2; 3; 2; 1 ]))
       ; ("non empty and not a palindrome"
          >:: fun _ -> assert_equal false (is_palindrome [ 1; 2; 2; 3 ]))
       ]
;;

let () =
  run_test_tt_main tests_last;
  run_test_tt_main tests_last_two;
  run_test_tt_main tests_nth;
  run_test_tt_main tests_length;
  run_test_tt_main tests_reverse;
  run_test_tt_main tests_is_palindrome
;;
