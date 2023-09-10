val string_of_list : ('a -> string) -> 'a list -> string
val string_of_pair : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
val string_of_option : ('a -> string) -> 'a option -> string
val diff : ('a -> string) -> ('b -> string) -> Format.formatter -> 'a * 'b -> unit
val diff_simple : ('a -> string) -> Format.formatter -> 'a * 'a -> unit
