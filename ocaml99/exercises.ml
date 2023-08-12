let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs
;;

let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs
;;

let rec nth xs n =
  match xs, n with
  | _ when n < 0 -> None
  | [], _ -> None
  | x :: _, 0 -> Some x
  | _ :: xs, m -> nth xs (m - 1)
;;
