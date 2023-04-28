type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y =
    match x, y with
    | { f = AM }, { f = PM} -> false
    | { f = PM }, { f = AM } -> true
    | _, _ -> x > y
