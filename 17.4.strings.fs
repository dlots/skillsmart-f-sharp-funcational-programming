// 17.1
let rec pow(s, n) =
    match n with
    | 0 -> ""
    | _ -> pow(s, n-1) + s

// 17.2
let rec isIthChar(s: string, n, c) = s.[n] = c

// 17.3
let rec occFromIth(s, n, c) =
    match n with 
    | i when i >= String.length s -> int 0
    | i when s.[i] = c -> occFromIth(s, n+1, c) + 1
    | _ -> occFromIth(s, n+1, c)
