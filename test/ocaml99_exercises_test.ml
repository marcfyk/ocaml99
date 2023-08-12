open OUnit2
open Ocaml99.Exercises

let tests_last =
  "last"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (last []))
       ; ("singleton list" >:: fun _ -> assert_equal (Some "a") (last [ "a" ]))
       ; ("list with more than 1 element"
          >:: fun _ -> assert_equal (Some "d") (last [ "a"; "b"; "c"; "d" ]))
       ]
;;

let tests_last_two =
  "last_two"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (last_two []))
       ; ("singleton list" >:: fun _ -> assert_equal None (last_two [ "a" ]))
       ; ("list of 2 elements"
          >:: fun _ -> assert_equal (Some ("a", "b")) (last_two [ "a"; "b" ]))
       ; ("list with more than 2 elements"
          >:: fun _ -> assert_equal (Some ("c", "d")) (last_two [ "a"; "b"; "c"; "d" ]))
       ]
;;

let tests_nth =
  "nth"
  >::: [ ("empty list" >:: fun _ -> assert_equal None (nth [] 0))
       ; ("index == length of list" >:: fun _ -> assert_equal None (nth [ "a" ] 1))
       ; ("index > length of list" >:: fun _ -> assert_equal None (nth [ "a" ] 2))
       ; ("index < 0" >:: fun _ -> assert_equal None (nth [ "a" ] (-2)))
       ; ("0 <= index < length of list"
          >:: fun _ -> assert_equal (Some "c") (nth [ "a"; "b"; "c"; "d"; "e" ] 2))
       ]
;;

let tests_length =
  "length"
  >::: [ ("empty list" >:: fun _ -> assert_equal 0 (length []))
       ; ("non empty list" >:: fun _ -> assert_equal 3 (length [ "a"; "b"; "c" ]))
       ]
;;

let tests_reverse =
  "rev"
  >::: [ ("empty list" >:: fun _ -> assert_equal [] (rev []))
       ; ("non empty list"
          >:: fun _ -> assert_equal [ "c"; "b"; "a" ] (rev [ "a"; "b"; "c" ]))
       ]
;;

let tests_is_palindrome =
  "is_palindrome"
  >::: [ ("empty list" >:: fun _ -> assert_equal true (is_palindrome []))
       ; ("non empty and is a palindrome"
          >:: fun _ -> assert_equal true (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]))
       ; ("non empty and not a palindrome"
          >:: fun _ -> assert_equal false (is_palindrome [ "a"; "b" ]))
       ]
;;

let tests_flatten =
  "flatten"
  >::: [ ("empty list" >:: fun _ -> assert_equal [] (flatten []))
       ; ("non empty list"
          >:: fun _ ->
          assert_equal
            [ "a"; "b"; "c"; "d"; "e" ]
            (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]))
       ]
;;

let tests_compress =
  "compress"
  >::: [ ("empty list" >:: fun _ -> assert_equal [] (compress []))
       ; ("list with no consecutive elements"
          >:: fun _ -> assert_equal [ "a"; "b"; "c" ] (compress [ "a"; "b"; "c" ]))
       ; ("list with consecutive elements"
          >:: fun _ ->
          assert_equal
            [ "a"; "b"; "c"; "a"; "d"; "e" ]
            (compress
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ]
;;

let tests_pack =
  "pack"
  >::: [ ("empty list" >:: fun _ -> assert_equal [] (pack []))
       ; ("list with no consecutive duplicates"
          >:: fun _ -> assert_equal [ [ "a" ]; [ "b" ]; [ "c" ] ] (pack [ "a"; "b"; "c" ])
         )
       ; ("list with consecutive duplicates"
          >:: fun _ ->
          assert_equal
            [ [ "a"; "a"; "a"; "a" ]
            ; [ "b" ]
            ; [ "c"; "c" ]
            ; [ "a"; "a" ]
            ; [ "d"; "d" ]
            ; [ "e"; "e"; "e"; "e" ]
            ]
            (pack
               [ "a"
               ; "a"
               ; "a"
               ; "a"
               ; "b"
               ; "c"
               ; "c"
               ; "a"
               ; "a"
               ; "d"
               ; "d"
               ; "e"
               ; "e"
               ; "e"
               ; "e"
               ]))
       ]
;;

let () =
  run_test_tt_main tests_last;
  run_test_tt_main tests_last_two;
  run_test_tt_main tests_nth;
  run_test_tt_main tests_length;
  run_test_tt_main tests_reverse;
  run_test_tt_main tests_is_palindrome;
  run_test_tt_main tests_flatten;
  run_test_tt_main tests_compress;
  run_test_tt_main tests_pack
;;
