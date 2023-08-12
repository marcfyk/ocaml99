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

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten nodes =
  let rec flatten' acc = function
    | [] -> acc
    | One x :: xs -> flatten' (x :: acc) xs
    | Many x :: xs -> flatten' (flatten' acc x) xs
  in
  nodes |> flatten' [] |> rev
;;

let compress xs =
  let rec compress' acc xs =
    match acc, xs with
    | _, [] -> acc
    | [], x :: xs -> compress' (x :: acc) xs
    | y :: _, x :: xs -> if y == x then compress' acc xs else compress' (x :: acc) xs
  in
  compress' [] xs |> rev
;;

let pack xs =
  let rec pack' acc xs =
    match acc, xs with
    | _, [] -> acc
    | [], xs -> pack' ([] :: acc) xs
    | [] :: ys, x :: xs -> pack' ([ x ] :: ys) xs
    | (z :: _ as zs) :: ys, x :: xs ->
      if z == x then pack' ((x :: zs) :: ys) xs else pack' ([ x ] :: acc) xs
  in
  pack' [] xs |> rev
;;
