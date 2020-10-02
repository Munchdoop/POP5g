//5g1 a
let a = Array2D.init 2 3 (fun x y -> (x,y))
let b = Array2D.init 7 9 (fun x y -> (x,y))
let transposeArr (array2d:'a [,]) : 'a [,] =
    let newArray = Array2D.init (array2d |> Array2D.length2) (array2d |> Array2D.length1) (fun r c -> array2d.[c,r])
    newArray
printfn "%A" b
printfn "%A" (transposeArr(b))