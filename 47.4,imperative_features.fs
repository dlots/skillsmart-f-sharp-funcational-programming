// 47.4.1
let f n =
    let mutable factorial = 1
    let mutable counter = 1
    while counter <= n do
        factorial <- factorial * counter
        counter <- counter + 1
    factorial

// 47.4.2
let fibo n =
    if n = 0 then
        0
    else
        let mutable previous = 0
        let mutable current = 1
        let mutable counter = 1
        while counter < n do
            let next = previous + current
            previous <- current
            current <- next
            counter <- counter + 1
        current
