// 42.3
let rec allSubsets n k =
    match n, k with
    | n, k when n = k -> set [set [1 .. n]]
    | _, 0 -> set [Set.empty]
    | n, k -> Set.union (allSubsets (n-1) k) (Set.map (Set.add n) (allSubsets (n-1) (k-1)))
    
