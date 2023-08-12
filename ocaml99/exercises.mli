(** Tail of a List
    Beginner difficulty

    Write a function last : 'a list -> 'a option that returns the last element of a list *)
val last : 'a list -> 'a option

(** Last Two Elements of a List
    Beginner difficulty

    Find the last but one (last and penultimate) elements of a list. *)
val last_two : 'a list -> ('a * 'a) option

(** N'th Element of a List
    Beginner difficulty

    Find the N'th element of a list.

    Remark: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds.*)
val nth : 'a list -> int -> 'a option

(** Find the number of elements of a list.

    OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.*)
val length : 'a list -> int

(** Reverse a list.

    OCaml standard library has List.rev but we ask that you reimplement it.*)
val rev : 'a list -> 'a list

(** Find out whether a list is a palindrome.

    Hint: A palindrome is its own reverse.*)
val is_palindrome : 'a list -> bool
