// 50.2.1
let fac_seq = seq {
    let rec f x a =
        if x <= 1 then a
        else f (x - 1) (a * x)
    for i in Seq.initInfinite id do
        yield (f i 1)
}

// 50.2.2
let seq_seq = seq {
    for i in Seq.initInfinite id do
        if i % 2 = 0 then yield i/2
        else yield (-1) * (i + 1)/2
}
