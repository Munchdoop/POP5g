let squares (n:int) :int [] = 
    let arr = Array.init n (fun x -> x*x)
    arr
printfn"%A"(squares(10))    


//unit fejl 
let alpha : 'a [,] = Array2D.init 2 2 (fun r c -> alpha[r].[c] )
let transposeArr (alpha :'a [,]) : 'a [,] =
    let newarray =  Array2D.init 2 3  (fun r c -> alpha.[c,r])
    for r in[0..Array2D.length1 alpha] do
        for c in[0..Array2D.length2 alpha - 1] do
            printf "%2d " (alpha.[r].[c])


//andet attempt 
let a = Array2D.init 5 5 (fun r c -> (r+1) * (c+1))
let prA (a : int[,]) =
    for r in[0..Array2D.length1 a - 1] do
        for c in[0..Array2D.length2 a - 1] do
            printf "%2d " (a.[r,c])
        printf "\n"
do prA a

//tætteste på at virke 
let arrayOfArrays = [| [|1;2 |] ;  [|3;4 |]] |]
let transposeArr (array2d:'a [,]) : 'a [,] =
    let newArray = Array2D.init 2 3  (fun r c -> array2d.[c,r])
    newArray
printfn"%A" (transposeArr(arrayOfArrays))


let a = Array2D.init 3 2 (fun x y -> (x,y))
printfn "%A" a


let arrayOfArrays = [| [| 1; 0 ; 3|]; [|0; 1 ; 2|] |]
let twoDimensionalArray = Array2D.init 3 2 (fun i j -> arrayOfArrays.[i].[j])

//virker man svare ikke på opgaven 
let transposeArr (arrayOfArrays) = 
    let arrayOfArrays = [| [| 1; 2; 3 |]; [|4; 5; 6 |] |]
    let array2d = Array2D.init 2 3 (fun row column -> arrayOfArrays.[row].[column])     
    let newArray = Array2D.init (array2d |> Array2D.length2) (array2d |> Array2D.length1) (fun r c -> array2d.[c,r])
    newArray



//matrix syntaks virker ikke 
let transpose (matrix: 'a [,]) : 'a [,]=
    if matrix.Length = 0 then failwith "Invalid matrix"  
    Array.init matrix.[0].Length (fun i -> 
        Array.init matrix.Length (fun j -> 
            matrix.[j].[i]))
