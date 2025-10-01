let abs x =
  if x >= 0 then x
  else (-x)

(* Part 1 *)

let rev_tup (a, b) = (b, a)

let rev_triple (a, b, c) = (c, b, a)

let is_odd x = x mod 2 <> 0

let is_older (y1, m1, d1) (y2, m2, d2) =
  if y1 <> y2 then y1 < y2
  else if m1 <> m2 then m1 < m2
  else d1 < d2

let to_us_format (y, m, d) = (m, d, y)

(* Part 2 *)

let rec pow x p =
  if p = 0 then 1
  else x * pow x (p - 1)

let rec fac n =
  if n = 1 then 1
  else n * fac (n - 1)

(* Part 3 *)

let rec get_nth (idx, lst) =
  match (idx, lst) with
  | 0, h :: _ -> h
  | n, _ :: t -> get_nth (n - 1, t)
  | _, [] -> failwith "Index out of bounds"

let larger lst1 lst2 =
  let len1 = List.length lst1 in
  let len2 = List.length lst2 in
  if len1 > len2 then lst1
  else if len2 > len1 then lst2
  else []

let sum lst1 lst2 =
  let rec list_sum lst =
    match lst with
    | [] -> 0
    | h :: t -> h + list_sum t
  in
  list_sum lst1 + list_sum lst2





