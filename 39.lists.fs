// 39.1
let rec rmodd = function
    | _ :: head :: tail -> [head] @ rmodd tail
    | _ -> []

// 39.2
let rec del_even = function
    | head :: tail when head % 2 = 0 -> del_even tail
    | head :: tail when head % 2 > 0 -> [head] @ del_even tail
    | _ -> []

// 39.3
let rec multiplicity x = function
    | head :: tail when head = x -> 1 + multiplicity x tail
    | head :: tail when head <> x -> multiplicity x tail
    | _ -> 0

// 39.4
let rec split = function
    | head1 :: head2 :: tail ->
        let x, y = split tail
        ([head1] @ x, [head2] @ y)
    | head :: tail ->
        let x, y = split tail
        ([head] @ x, y)
    | _ -> ([], [])

// 39.5
let rec zip = function
    | head1 :: tail1, head2 :: tail2 -> [(head1, head2)] @ zip (tail1, tail2)
    | [], [] -> []
    | _ -> invalidArg "list1, list2" "lists not equal in size"
