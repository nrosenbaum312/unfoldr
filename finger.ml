let divisible_by = fun value divisor ->
  value mod divisor = 0

let rec coins = fun amount ->
  if amount = 0 then 0
  else if (divisible_by amount 10) then 1 + (coins (amount - 10))
  else if (divisible_by amount 5) then 1 + (coins (amount - 5))
  else 1 + (coins (amount - 1))

let max = fun a b ->
  if a > b then a else b

let min = fun a b ->
  if a > b then b else a

let rec maximum_table_area = fun side1 side2 side3 ->
  let pair1 = (max side1 side2) in
  let pair2 = (max side2 side3) in
  let pair3 = (max side3 side1) in
  (max pair1 pair2) * (min pair1 (min pair2 pair3))

let rec exists = fun bools ->
  begin match bools with
    | []      -> false
    | x::tail -> x || (exists tail)
  end

let rec all = fun bools ->
  begin match bools with
    | []      -> true
    | x::tail -> x && (all tail)
  end
  
let rec append = fun l1 l2 ->
  begin match l1 with
    | []      -> l2
    | x::tail -> x::(append tail l2)
  end
  
let rec contains_int = fun l name ->
  begin match l with
    | []      -> false
    | x::tail -> x = name || (contains_int tail name)
  end

let rec in_both = fun names1 names2 ->
  begin match names1 with
    | []      -> []
    | x::tail -> if (contains_int names2 x) then x::(in_both tail names2)
      else (in_both tail names2)
  end

let rec merge = fun l1 l2 ->
  begin match (l1, l2) with
    | ([], [])               -> []
    | (x::tail, [])          -> x::(merge tail l2)
    | ([], x::tail)          -> x::(merge l1 tail)
    | (x1::tail1, x2::tail2) -> x1::(x2::(merge tail1 tail2))
  end

let rec is_sorted = fun l ->
  begin match l with
    | []         -> true
    | [x]        -> true
    | x::y::tail -> x <= y && (is_sorted (y::tail))
  end

let rec merge_sorted = fun l1 l2 ->
  begin match (l1, l2) with
    | ([], [])               -> []
    | (x::tail, [])          -> x::(merge_sorted tail l2)
    | ([], x::tail)          -> x::(merge_sorted l1 tail)
    | (x1::tail1, x2::tail2) -> if x2 > x1 then x1::(merge_sorted tail1 l2)
      else x2::(merge_sorted l1 tail2)
  end

let rec is_prefix_of = fun l1 l2 ->
  begin match (l1, l2) with
    | ([], [])               -> true
    | (x1::tail1, [])        -> false
    | ([], x2::tail2)        -> true
    | (x1::tail1, x2::tail2) -> x1 = x2 && (is_prefix_of tail1 tail2)
  end

let rec sublist = fun l1 l2 ->
  begin match l2 with
    | []      -> l1 = []
    | x::tail -> (is_prefix_of l1 l2) || (sublist l1 tail)
  end

let evaluate = fun num inc fn arg ->
  if num >= 0 then inc + (fn arg)
  else if num = (-999) then 0
  else (fn arg)

let rec sum_rainfall = fun readings ->
  begin match readings with
    | []      -> 0
    | x::tail -> (evaluate x x sum_rainfall tail)
  end

let rec length = fun l ->
  begin match l with
    | [] -> 0
    | x::tail -> (evaluate x 1 length tail)
  end

let rainfall = fun readings ->
  begin match readings with
    | []   -> -1
    | _::_ -> let sum = (sum_rainfall readings) in 
      let len = (length readings) in 
      if len > 0 then sum / len else -1
  end

let rec append_lists = fun l1 l2 ->
  begin match l1 with
    | []      -> l2
    | x::tail -> x::(append_lists tail l2)
  end

let rec append_ints = fun l1 l2 ->
  begin match l1 with
    | []      -> l2
    | x::tail -> x::(append_ints tail l2)
  end

let rec distribute_into = fun l num ->
  begin match l with
    | []      -> [[num]]
    | [x]     -> ((num::x)::[])
    | x::tail -> (append_lists ((num::x)::[]) (distribute_into tail num))
  end

let rec filter_out = fun l value ->
  begin match l with
    | []  -> []
    | x::tail -> (append_ints 
                    (if x = value then [] else x::[]) 
                    (filter_out tail value)
                  )
  end
  
let rec map_perms = fun iterated whole ->
  begin match iterated with
    | []      -> []
    | x::tail -> let filtered = (filter_out whole x) in 
      (append_lists 
          (distribute_into (map_perms filtered filtered) x) 
          (map_perms tail whole)
      )
  end 
  
let rec permutations = fun l ->
  begin match l with
    | [] -> [[]]
    | _::_ -> (map_perms l l)
  end

let rec map_insert hd l =
  begin match l with
  | [] -> []
  | y :: ys -> (hd :: y) :: map_insert hd ys
  end

let rec insert_all_positions x lst =
  begin match lst with
  | [] -> [[x]]
  | hd :: tl ->
      (x :: lst) ::
      map_insert hd (insert_all_positions x tl)
  end

let rec flatten lsts =
  begin match lsts with
  | [] -> []
  | l :: ls -> l @ flatten ls
  end

let rec map_permutations hd l =
  begin match l with
  | [] -> []
  | x :: xs -> insert_all_positions hd x :: map_permutations hd xs
  end

let rec permutations lst =
  begin match lst with
  | [] -> [[]]
  | hd :: tl ->
    flatten (map_permutations hd (permutations tl))
  end