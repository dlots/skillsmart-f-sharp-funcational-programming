// 41.4.1
let list_filter f xs = List.foldBack (fun x acc -> if f x then x :: acc else acc) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun acc x -> if p x then acc + x else acc) 0 xs

// 41.4.3
let revrev list_of_lists =
    let rev acc lst = (List.fold (fun head tail -> tail::head) [] lst) :: acc
    List.fold rev [] list_of_lists
