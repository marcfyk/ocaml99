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
    | (y' :: _ as ys') :: ys, x :: xs ->
      if y' == x then pack' ((x :: ys') :: ys) xs else pack' ([ x ] :: acc) xs
  in
  pack' [] xs |> rev
;;

let encode xs =
  let rec encode' acc xs =
    match acc, xs with
    | _, [] -> acc
    | [], x :: xs -> encode' ((1, x) :: acc) xs
    | (count, y) :: ys, x :: xs ->
      if y == x then encode' ((count + 1, y) :: ys) xs else encode' ((1, x) :: acc) xs
  in
  encode' [] xs |> rev
;;

type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_modified xs =
  let rec encode_modified' acc xs =
    match acc, xs with
    | _, [] -> acc
    | [], x :: xs -> encode_modified' (One x :: acc) xs
    | One y :: ys, x :: xs ->
      if y == x
      then encode_modified' (Many (2, y) :: ys) xs
      else encode_modified' (One x :: acc) xs
    | Many (count, y) :: ys, x :: xs ->
      if y == x
      then encode_modified' (Many (count + 1, y) :: ys) xs
      else encode_modified' (One x :: acc) xs
  in
  encode_modified' [] xs |> rev
;;

let decode xs =
  let rec decode' acc = function
    | [] -> acc
    | One x :: xs -> decode' (x :: acc) xs
    | Many (1, x) :: xs -> decode' (x :: acc) xs
    | Many (count, x) :: xs -> decode' (x :: acc) (Many (count - 1, x) :: xs)
  in
  decode' [] xs |> rev
;;

let encode_direct xs =
  let rec encode_direct' acc xs =
    match acc, xs with
    | _, [] -> acc
    | [], x :: xs -> encode_direct' [ One x ] xs
    | One y :: ys, x :: xs ->
      if y == x
      then encode_direct' (Many (2, y) :: ys) xs
      else encode_direct' (One x :: acc) xs
    | Many (count, y) :: ys, x :: xs ->
      if y == x
      then encode_direct' (Many (count + 1, y) :: ys) xs
      else encode_direct' (One x :: acc) xs
  in
  encode_direct' [] xs |> rev
;;
