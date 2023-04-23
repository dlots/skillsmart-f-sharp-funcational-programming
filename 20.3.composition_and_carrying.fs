// 20.3.1
let vat n x = x / 100. * float (100 + n)

// 20.3.2
let unvat n x = x / float (100 + n) * 100.

// 20.3.3
let rec min f =
    let rec check = function
        | (f, n) when f(n) = 0 -> n
        | (f, n) -> check(f, n+1)
    check(f, 1)
