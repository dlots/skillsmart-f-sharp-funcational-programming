// 49.5.1
let even_seq = Seq.initInfinite (fun i -> (i+1)*2)

// 49.5.2
let fac_seq = Seq.unfold (fun (index, fac) -> Some((index, fac), (index+1, fac*(index+1)))) (0, 1) |> Seq.map snd

// 49.5.3
let seq_seq = Seq.initInfinite(fun i -> (i + (i%2)) / 2 * pown -1 (i%2))
