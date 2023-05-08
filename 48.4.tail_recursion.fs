// 48.4.1
let rec fibo1 n n1 n2 =
    match n with
    | 0 -> n2
    | 1 -> n1
    | _ -> fibo1 (n-1) (n1+n2) n1

// 48.4.2
let rec fibo2 n c =
    match n with
    | 0 | 1 -> c n
    | _ -> fibo2 (n-1) (fun a -> fibo2 (n-2) (fun b -> c (a+b)))

// 48.4.3
let rec bigList n k =
    let rec f n acc =
        match n with
        | 0 -> acc
        | _ -> f (n-1) (1::acc)
    f n []
