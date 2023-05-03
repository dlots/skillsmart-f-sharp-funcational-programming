// 40.1
let rec sum (p, xs) =
    let rec iteration (p, xs, sum_accumulator) =
        match xs with
        | [] -> sum_accumulator
        | head :: tail when p(head) -> iteration(p, tail, sum_accumulator + head)
        | _ :: tail -> iteration(p, tail, sum_accumulator)
    iteration(p, xs, 0)

// 40.2.1
let rec count (xs, n) =
    let rec iteration (xs, n, count_accumulator) =
        match xs with
        | head :: tail when head < n -> iteration(tail, n, count_accumulator)
        | head :: tail when head = n -> iteration(tail, n, count_accumulator + 1)
        | _ -> count_accumulator
    iteration(xs, n, 0)

// // 40.2.2
let rec insert (xs, n) =
    match xs with
    | head :: tail when head < n -> head :: insert (tail, n)
    | _ -> n :: xs
    
// // 40.2.3
let rec intersect (xs1, xs2) =
    let rec iteration(xs1, xs2, intersection) =
       match xs1, xs2 with
       | head1 :: tail1, head2 :: tail2 when head1 = head2 -> iteration(tail1, tail2, intersection @ [head1])
       | head1 :: tail1, head2 :: _ when head1 < head2 -> iteration(tail1, xs2, intersection)
       | head1 :: _, head2 :: tail2 when head1 > head2 -> iteration(xs1, tail2, intersection)
       | _ -> intersection
    iteration(xs1, xs2, [])

// // 40.2.4
let rec plus (xs1, xs2) =
    let rec iteration(xs1, xs2, result) =
       match xs1, xs2 with
       | head1 :: tail1, head2 :: tail2 when head1 = head2 -> iteration(tail1, tail2, result @ [head1; head1])
       | head1 :: tail1, head2 :: _ when head1 < head2 -> iteration(tail1, xs2, result @ [head1])
       | head1 :: _, head2 :: tail2 when head1 > head2 -> iteration(xs1, tail2, result @ [head2])
       | [], _ -> result @ xs2
       | _, [] -> result @ xs1
       | _, _ -> []
    iteration(xs1, xs2, [])
    
// // 40.2.5
let rec minus (xs1, xs2) =
    let rec iteration(xs1, xs2, xor) =
        match xs1, xs2 with
        | _, [] -> xor @ xs1
        | head1 :: tail1, head2 :: tail2 when head1 = head2 -> iteration(tail1, tail2, xor)
        | head1 :: tail1, head2 :: _ when head1 < head2 -> iteration(tail1, xs2, xor @ [head1])
        | head1 :: _, head2 :: tail2 when head1 > head2 -> iteration(xs1, tail2, xor)
        | _, _ -> xor
    iteration(xs1, xs2, [])

// // 40.3.1
let rec smallest xs =
    let rec iteration (xs, current_smallest) =
        match xs with
        | head :: tail when Option.isNone current_smallest -> iteration (tail, Some head)
        | head :: tail when head < Option.get current_smallest -> iteration (tail, Some head)
        | [] -> current_smallest
        | _ :: tail -> iteration(tail, current_smallest)
    iteration(xs, None)
    
// // 40.3.2
let rec delete (n, xs) =
    let rec iteration (n, xs, left) =
        match xs with
        | [] -> left
        | head :: tail when head = n -> left @ tail
        | head :: tail -> iteration(n, tail, left @ [head])
    iteration(n, xs, [])

// // 40.3.3
let rec sort xs =
    let rec iteration (sorted, xs) =
        match xs with
        | [] -> sorted
        | _ ->
            let smallest = Option.get (smallest xs)
            iteration(sorted @ [smallest], delete (smallest, xs))
    iteration([], xs)
    
// // 40.4
let rec revrev lst =
    let rec reverse lst = 
        match lst with
        | [] -> []
        | head :: tail -> reverse tail @ [head]
    match lst with
    | [] -> []
    | x :: xs -> revrev xs @ [reverse x]
    
