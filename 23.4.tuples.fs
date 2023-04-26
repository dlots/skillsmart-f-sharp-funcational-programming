// 23.4.1
let (.+.) x y =
    let total (gold, silver, bronze) = (gold * 20 + silver) * 12 + bronze
    let new_value = total x + total y
    let bronze = new_value % 12
    let all_silver = new_value / 12
    let silver = all_silver % 20
    let gold = (all_silver - silver) / 20
    (gold, silver, bronze)
    
let (.-.) x y =
    let total (gold, silver, bronze) = (gold * 20 + silver) * 12 + bronze
    let new_value = total x - total y
    let bronze = new_value % 12
    let all_silver = new_value / 12
    let silver = all_silver % 20
    let gold = all_silver / 20
    (gold, silver, bronze)

// 23.4.2
let (.+) x y =
    let a, b = x
    let c, d = y
    (a + c, b + d)
    
let (.-) x y =
    let a, b = y
    x .+ (-a, -b)
    
let (.*) x y =
    let a, b = x
    let c, d = y
    (a * c - b * d, b * c + a * d)
    
let (./) x y =
    let a, b = y
    x .* (a / (a*a + b*b), -b / (a*a + b*b))
    
