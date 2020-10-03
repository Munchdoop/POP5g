//5g0(a)
///<summary>This function takes an a' list list as an argument and returns a true statement if all lists contain at least one element and are of the same size.</summary>
///<param name="llst">A multidimensinal list.</param>
///<returns>A boolean true or false statement.</returns>
let isTable (llst:'a list list) : bool =
    let mutable isValidTable : bool = true
    let lenFirstColumn : int = llst.Head.Length
    //printfn "Length of inital element: %i" len
    if lenFirstColumn = 0 then
        //printfn "Initial element is empty, breaking"
        isValidTable <- false
        isValidTable
    else
        //printfn "Length is not empty, proceeding"
        let mutable llstCopy = llst
        let mutable i = 1
        let lenRows = llst.Length
        while (i < lenRows) && isValidTable do
            //printfn "Loop iteration: %i" i
            llstCopy <- List.tail llstCopy
            if llstCopy.Head.Length <> lenFirstColumn then
                //printfn "Found inequality, breaking"
                isValidTable <- false
            i <- i + 1
        isValidTable
//printfn "This is the result of the function isTable: %A" (isTable([[1;2;3];[4;5;6]]))

//5g0(b)
///<summary>This function takes an 'a list list and returns the head in each embedded list.</summary>
///<param name = "llst">This a multidimensional 'a list.</param>
///<returns>Returns a list containing the heads of each of the list in the list.</returns>
let firstColumn (llst:'a list list) : 'a list =
    let mutable lstRes : 'a list = [] // Resultant 'a list
    let llstHasEmpty : bool list = List.map (fun (x: 'a list) -> x.IsEmpty) llst // Checks if llst has empty elements
    if List.contains true llstHasEmpty then
        lstRes <- []
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Head) llst
    lstRes
//printfn "this is the result of the function firstColumn: %A" (firstColumn([[1;2;3];[4;5;6]]))

//5g0(c)
///<summary>This function takes a 'a list list and returns the tail in each embedded list.</summmary>
///<param name = "llst">This a multidimensional a' list.</param>
///<returns>Returns a list list containing the tails of each of the list in the list.</returns>
let dropFirstColumn (llst:'a list list) : 'a list list =
    let mutable lstRes : 'a list list = [[]]
    let llstHasEmpty = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    if List.contains (true) (llstHasEmpty) then
        lstRes <- [[]]
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Tail) llst
    lstRes
//printfn "this is the result of the function dropFirstColumn: %A" (dropFirstColumn([[1;2;3];[4;5;6]]))

//5g0(d)
///<summary>transposeLstLst takes a multidimensional list and transposes it by using the funtions above.</summary>
///<param name = "llst">This a multidimensional list.</param>
///<returns>The function returns the transposed list.</returns>
let transposeLstLst (llst :'a list list) : 'a list list =
    let mutable a: 'a list list = llst
    let mutable b = []
    while List.forall (fun (x:'a list) -> x.Length > 0) a do
        b <- b @ [firstColumn a]
        a <- dropFirstColumn a
    b
//printfn "this is the result of the function transposeLstLst: %A" (transposeLstLst([[1;2;3];[4;5;6]]))

let InputList : 'a list list = [[1;2;3];[]]
printfn "InputList <- %A" InputList
printfn "This is the result of the function isTable: %A" (isTable(InputList))
printfn "this is the result of the function firstColumn: %A" (firstColumn(InputList))
printfn "this is the result of the function dropFirstColumn: %A" (dropFirstColumn(InputList))
printfn "this is the result of the function transposeLstLst: %A" (transposeLstLst(InputList))