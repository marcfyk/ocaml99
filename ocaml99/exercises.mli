(** Tail of a List
    Beginner difficulty

    Write a function last : 'a list -> 'a option that returns the last element
    of a list. *)
val last : 'a list -> 'a option

(** Last Two Elements of a List
    Beginner difficulty

    Find the last but one (last and penultimate) elements of a list. *)
val last_two : 'a list -> ('a * 'a) option

(** N'th Element of a List
    Beginner difficulty

    Find the N'th element of a list.

    Remark: OCaml has List.nth which numbers elements from 0 and raises an
    exception if the index is out of bounds. *)
val nth : 'a list -> int -> 'a option

(** Length of a List
    Beginner difficulty

    Find the number of elements of a list.

    OCaml standard library has List.length but we ask that you reimplement it.
    Bonus for a tail recursive solution. *)
val length : 'a list -> int

(** Reverse a List
    Beginner difficulty

    Reverse a list.

    OCaml standard library has List.rev but we ask that you reimplement it. *)
val rev : 'a list -> 'a list

(** Palindrome
    Beginner difficulty

    Find out whether a list is a palindrome.

    Hint: A palindrome is its own reverse. *)
val is_palindrome : 'a list -> bool

type 'a node =
  | One of 'a
  | Many of 'a node list

(** Flatten a List
    Intermediate difficulty

    Flatten a nested list structure. *)
val flatten : 'a node list -> 'a list

(** Eliminate Duplicates
    Intermediate difficulty

    Eliminate consecutive duplicates of list elements. *)
val compress : 'a list -> 'a list

(** Pack Consecutive Duplicates
    Intermediate difficulty

    Pack consecutive duplicates of list elements into sublists. *)
val pack : 'a list -> 'a list list

(** Run-Length Encoding
    Beginner difficulty

    If you need so, refresh your memory about run-length encoding. *)
val encode : 'a list -> (int * 'a) list

type 'a rle =
  | One of 'a
  | Many of int * 'a

(** Modified Run-Length Encoding
    Beginner difficulty

    Modify the result of the previous problem in such a way that if an element
    has no duplicates it is simply copied into the result list.
    Only elements with duplicates are transferred as (N E) lists.

    Since OCaml lists are homogeneous, one needs to define a type to hold both
    single elements and sub-lists. *)
val encode_modified : 'a list -> 'a rle list

(** Decode a Run-Length Encoded List
    Intermediate difficulty

    Given a run-length code list generated as specified in the previous problem,
    construct its uncompressed version. *)
val decode : 'a rle list -> 'a list

(** Run-Length Encoding of a List (Direct Solution)
    Intermediate difficulty

    Implement the so-called run-length encoding data compression method directly.
    I.e. don't explicitly create the sublists containing the duplicates,
    as in problem "Pack consecutive duplicates of list elements into sublists",
    but only count them. As in problem "Modified run-length encoding",
    simplify the result list by replacing the singleton lists (1 X) by X. *)
val encode_direct : 'a list -> 'a rle list

(** Duplicate the Elements of a List
    Beginner difficulty

    Duplicate the elements of a list. *)
val duplicate : 'a list -> 'a list

(** Replicate the Elements of a List a Given Number of Times
    Intermediate difficulty

    Replicate the elements of a list a given number of times. *)
val replicate : 'a list -> int -> 'a list

(** Drop Every N'th Element From a List
    Intermediate difficulty

    Drop every N'th element from a list. *)
val drop : 'a list -> int -> 'a list

(** Split a List Into Two Parts; The Length of the First Part Is Given
    Beginner difficulty

    Split a list into two parts; the length of the first part is given.

    If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty. *)
val split : 'a list -> int -> 'a list * 'a list
