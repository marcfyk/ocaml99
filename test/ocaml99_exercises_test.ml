open OUnit2
open Ocaml99.Exercises
open Diff

let tests_last =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_option (fun x -> x))) in
  "last"
  >::: [ ("empty list" >:: fun _ -> assert_eq None (last []))
       ; ("singleton list" >:: fun _ -> assert_eq (Some "a") (last [ "a" ]))
       ; ("list with more than 1 element"
          >:: fun _ -> assert_eq (Some "d") (last [ "a"; "b"; "c"; "d" ]))
       ]
;;

let tests_last_two =
  let assert_eq =
    assert_equal
      ~pp_diff:(diff_simple (string_of_option (string_of_pair (fun x -> x) (fun x -> x))))
  in
  "last_two"
  >::: [ ("empty list" >:: fun _ -> assert_eq None (last_two []))
       ; ("singleton list" >:: fun _ -> assert_eq None (last_two [ "a" ]))
       ; ("list of 2 elements"
          >:: fun _ -> assert_eq (Some ("a", "b")) (last_two [ "a"; "b" ]))
       ; ("list with more than 2 elements"
          >:: fun _ -> assert_eq (Some ("c", "d")) (last_two [ "a"; "b"; "c"; "d" ]))
       ]
;;

let tests_nth =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_option (fun x -> x))) in
  "nth"
  >::: [ ("empty list" >:: fun _ -> assert_eq None (nth [] 0))
       ; ("index == length of list" >:: fun _ -> assert_eq None (nth [ "a" ] 1))
       ; ("index > length of list" >:: fun _ -> assert_eq None (nth [ "a" ] 2))
       ; ("index < 0" >:: fun _ -> assert_eq None (nth [ "a" ] (-2)))
       ; ("0 <= index < length of list"
          >:: fun _ -> assert_eq (Some "c") (nth [ "a"; "b"; "c"; "d"; "e" ] 2))
       ]
;;

let tests_length =
  let assert_eq = assert_equal ~pp_diff:(diff_simple string_of_int) in
  "length"
  >::: [ ("empty list" >:: fun _ -> assert_eq 0 (length []))
       ; ("non empty list" >:: fun _ -> assert_eq 3 (length [ "a"; "b"; "c" ]))
       ]
;;

let tests_reverse =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "rev"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (rev []))
       ; ("non empty list"
          >:: fun _ -> assert_eq [ "c"; "b"; "a" ] (rev [ "a"; "b"; "c" ]))
       ]
;;

let tests_is_palindrome =
  let assert_eq = assert_equal ~pp_diff:(diff_simple string_of_bool) in
  "is_palindrome"
  >::: [ ("empty list" >:: fun _ -> assert_eq true (is_palindrome []))
       ; ("non empty and is a palindrome"
          >:: fun _ -> assert_eq true (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]))
       ; ("non empty and not a palindrome"
          >:: fun _ -> assert_eq false (is_palindrome [ "a"; "b" ]))
       ]
;;

let tests_flatten =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "flatten"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (flatten []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ "a"; "b"; "c"; "d"; "e" ]
            (flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]))
       ]
;;

let tests_compress =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "compress"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (compress []))
       ; ("list with no consecutive elements"
          >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (compress [ "a"; "b"; "c" ]))
       ; ("list with consecutive elements"
          >:: fun _ ->
          assert_eq
            [ "a"; "b"; "c"; "a"; "d"; "e" ]
            (compress
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ]
;;

let tests_pack =
  let string_of_string_list_list = string_of_list (string_of_list (fun x -> x)) in
  let assert_eq = assert_equal ~pp_diff:(diff_simple string_of_string_list_list) in
  "pack"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (pack []))
       ; ("list with no consecutive duplicates"
          >:: fun _ -> assert_eq [ [ "a" ]; [ "b" ]; [ "c" ] ] (pack [ "a"; "b"; "c" ]))
       ; ("list with consecutive duplicates"
          >:: fun _ ->
          assert_eq
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

let tests_encode =
  let string_of_tuple (x, y) = "(" ^ string_of_int x ^ "," ^ y ^ ")" in
  let string_of_tuple_list xs = xs |> List.map string_of_tuple |> String.concat "; " in
  let diff = diff_simple string_of_tuple_list in
  let assert_eq = assert_equal ~pp_diff:diff in
  "encode"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (encode []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]
            (encode
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ]
;;

let tests_encode_modified =
  let string_of_rle = function
    | One a -> "One " ^ a
    | Many (a, b) -> "Many (" ^ string_of_int a ^ ", " ^ b ^ ")"
  in
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list string_of_rle)) in
  "encode_modified"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (encode_modified []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ Many (4, "a")
            ; One "b"
            ; Many (2, "c")
            ; Many (2, "a")
            ; One "d"
            ; Many (4, "e")
            ]
            (encode_modified
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ]
;;

let tests_decode =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "decode"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (decode []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
            (decode
               [ Many (4, "a")
               ; One "b"
               ; Many (2, "c")
               ; Many (2, "a")
               ; One "d"
               ; Many (4, "e")
               ]))
       ]
;;

let tests_encode_direct =
  let string_of_rle = function
    | One a -> "One " ^ a
    | Many (a, b) -> "Many (" ^ string_of_int a ^ ", " ^ b ^ ")"
  in
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list string_of_rle)) in
  "encode_direct"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (encode_direct []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ Many (4, "a")
            ; One "b"
            ; Many (2, "c")
            ; Many (2, "a")
            ; One "d"
            ; Many (4, "e")
            ]
            (encode_direct
               [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]))
       ]
;;

let tests_duplicate =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "duplicate"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (duplicate []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]
            (duplicate [ "a"; "b"; "c"; "c"; "d" ]))
       ]
;;

let tests_replicate =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "replicate"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (replicate [] 1))
       ; ("n = 0" >:: fun _ -> assert_eq [] (replicate [ "a"; "b"; "c" ] 0))
       ; ("n = 1" >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (replicate [ "a"; "b"; "c" ] 1))
       ; ("n > 1"
          >:: fun _ ->
          assert_eq
            [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]
            (replicate [ "a"; "b"; "c" ] 3))
       ]
;;

let tests_drop =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "drop"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (drop [] 1))
       ; ("n <= 0" >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (drop [ "a"; "b"; "c" ] 0))
       ; ("n = 1" >:: fun _ -> assert_eq [] (drop [ "a"; "b"; "c" ] 1))
       ; ("n > 1"
          >:: fun _ ->
          assert_eq
            [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]
            (drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3))
       ]
;;

let tests_split =
  let string_of_string_list = string_of_list (fun x -> x) in
  let string_of_tuple = string_of_pair string_of_string_list string_of_string_list in
  let diff = diff_simple string_of_tuple in
  let assert_eq = assert_equal ~pp_diff:diff in
  "split"
  >::: [ ("empty list" >:: fun _ -> assert_eq ([], []) (split [] 3))
       ; ("n < 0"
          >:: fun _ -> assert_eq ([], [ "a"; "b"; "c" ]) (split [ "a"; "b"; "c" ] (-1)))
       ; ("n = 0"
          >:: fun _ -> assert_eq ([], [ "a"; "b"; "c" ]) (split [ "a"; "b"; "c" ] 0))
       ; ("n > 0"
          >:: fun _ ->
          assert_eq
            ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
            (split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3))
       ; ("n > length of list"
          >:: fun _ ->
          assert_eq ([ "a"; "b"; "c"; "d" ], []) (split [ "a"; "b"; "c"; "d" ] 5))
       ]
;;

let tests_slice =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "slice"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (slice [] 1 2))
       ; ("i < 0"
          >:: fun _ ->
          assert_equal [ "a"; "b"; "c" ] (slice [ "a"; "b"; "c"; "d"; "e" ] (-1) 2))
       ; ("k < 0" >:: fun _ -> assert_eq [] (slice [ "a"; "b"; "c"; "d"; "e" ] 1 (-2)))
       ; ("i > length of list"
          >:: fun _ -> assert_eq [] (slice [ "a"; "b"; "c"; "d"; "e" ] 10 2))
       ; ("k > length of list"
          >:: fun _ ->
          assert_eq [ "b"; "c"; "d"; "e" ] (slice [ "a"; "b"; "c"; "d"; "e" ] 1 10))
       ; ("(i, k) > 0 and (i, k) < length of list"
          >:: fun _ ->
          assert_eq
            [ "c"; "d"; "e"; "f"; "g" ]
            (slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6))
       ]
;;

let tests_rotate =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "rotate"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (rotate [] 1))
       ; ("n = 0" >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (rotate [ "a"; "b"; "c" ] 0))
       ; ("n > 0"
          >:: fun _ ->
          assert_eq
            [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
            (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3))
       ; ("n > 0 and n > length of list"
          >:: fun _ ->
          assert_eq
            [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
            (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 11))
       ; ("n < 0"
          >:: fun _ ->
          assert_eq
            [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
            (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-5)))
       ; ("n < 0 and |n| > length of list"
          >:: fun _ ->
          assert_eq
            [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]
            (rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-13)))
       ]
;;

let tests_remove_at =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "remove_at"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (remove_at 1 []))
       ; ("k < 0"
          >:: fun _ ->
          assert_eq [ "a"; "b"; "c"; "d" ] (remove_at (-1) [ "a"; "b"; "c"; "d" ]))
       ; ("k = length of list"
          >:: fun _ ->
          assert_eq [ "a"; "b"; "c"; "d" ] (remove_at 4 [ "a"; "b"; "c"; "d" ]))
       ; ("k > length of list"
          >:: fun _ ->
          assert_eq [ "a"; "b"; "c"; "d" ] (remove_at 5 [ "a"; "b"; "c"; "d" ]))
       ; ("k >= 0 and k < length of list"
          >:: fun _ -> assert_eq [ "a"; "c"; "d" ] (remove_at 1 [ "a"; "b"; "c"; "d" ]))
       ]
;;

let tests_insert_at =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "insert_at"
  >::: [ ("empty list" >:: fun _ -> assert_eq [ "a" ] (insert_at "a" 0 []))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ "a"; "alfa"; "b"; "c"; "d" ]
            (insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ]))
       ; ("index < 0"
          >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (insert_at "a" (-1) [ "b"; "c" ]))
       ; ("index = length of list"
          >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (insert_at "c" 2 [ "a"; "b" ]))
       ; ("index > length of list"
          >:: fun _ -> assert_eq [ "a"; "b"; "c" ] (insert_at "c" 3 [ "a"; "b" ]))
       ]
;;

let tests_range =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list string_of_int)) in
  "range"
  >::: [ ("start > end" >:: fun _ -> assert_eq [] (range 1 0))
       ; ("start = end" >:: fun _ -> assert_eq [ 1 ] (range 1 1))
       ; ("start < end" >:: fun _ -> assert_eq [ 4; 5; 6; 7; 8; 9 ] (range 4 9))
       ]
;;

let tests_rand_select =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list (fun x -> x))) in
  "rand_select"
  >::: [ ("empty list" >:: fun _ -> assert_eq [] (rand_select [] 0))
       ; ("non empty list"
          >:: fun _ ->
          assert_eq
            [ "g"; "d"; "b" ]
            (rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3))
       ; ("n > length of list" >:: fun _ -> assert_eq [ "a" ] (rand_select [ "a" ] 2))
       ]
;;

let tests_lotto_select =
  let assert_eq = assert_equal ~pp_diff:(diff_simple (string_of_list string_of_int)) in
  "lotto_select"
  >::: [ ("no elements" >:: fun _ -> assert_eq [] (lotto_select 0 10))
       ; ("non empty list"
          >:: fun _ -> assert_eq [ 29; 4; 20; 35; 24; 19 ] (lotto_select 6 49))
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
  run_test_tt_main tests_pack;
  run_test_tt_main tests_encode;
  run_test_tt_main tests_encode_modified;
  run_test_tt_main tests_decode;
  run_test_tt_main tests_encode_direct;
  run_test_tt_main tests_duplicate;
  run_test_tt_main tests_replicate;
  run_test_tt_main tests_drop;
  run_test_tt_main tests_split;
  run_test_tt_main tests_slice;
  run_test_tt_main tests_rotate;
  run_test_tt_main tests_remove_at;
  run_test_tt_main tests_insert_at;
  run_test_tt_main tests_range;
  run_test_tt_main tests_rand_select;
  run_test_tt_main tests_lotto_select
;;
