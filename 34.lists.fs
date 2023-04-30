// 34.1
let upto n =
    let rec iter(i, l) =
        match i with 
        | i when i = 0 -> l
        | _ -> iter(i-1, i :: l)
    iter (n, [])
        

// 34.2
let rec dnto n = 
    let rec iter(i, l) =
        match i with 
        | i when i > n -> l
        | _ -> iter(i+1, i :: l)
    iter (1, [])
    
// 34.3
let rec evenn n =
    let rec iter(i, l) =
        match i with 
        | i when i < 0 -> l
        | _ -> iter(i-2, i :: l)
    iter (n*2-2, [])
