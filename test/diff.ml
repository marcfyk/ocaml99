let string_of_list f xs = xs |> List.map f |> String.concat "; "
let string_of_pair f g (x, y) = "(" ^ String.concat ", " [ f x; g y ] ^ ")"

let string_of_option f = function
  | None -> "None"
  | Some x -> "Some " ^ f x
;;

let diff f g formatter (xs, ys) =
  let xs' = f xs in
  let ys' = g ys in
  Format.fprintf formatter "\nexpected: %s\nactual: %s" xs' ys'
;;

let diff_simple f = diff f f
