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
