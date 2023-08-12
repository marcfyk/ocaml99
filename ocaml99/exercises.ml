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

let length xs =
  let rec length' acc = function
    | [] -> acc
    | _ :: xs -> length' (acc + 1) xs
  in
  length' 0 xs
;;

let rev xs =
  let rec rev' acc = function
    | [] -> acc
    | x :: xs -> rev' (x :: acc) xs
  in
  rev' [] xs
;;

let is_palindrome xs = rev xs = xs
