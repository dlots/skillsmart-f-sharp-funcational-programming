// 16.1
let notDivisible(n, m) = m % n = 0

// 16.2
let prime = function
| n when n < 2 -> false
| n when n = 2 -> true
| n -> not (List.exists (fun elem -> (notDivisible(elem, n))) [2 .. (n-1)])
