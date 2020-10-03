//5g0(a)
///<summary>This function takes an a' list list as an argument and returns a true statement if all lists contain at least one element and are of the same size.</summary>
///<param name="llst">A multidimensinal list.</param>
///<returns>A boolean true or false statement.</returns>
let isTable (llst:'a list list) : bool =
    let mutable isValidTable : bool = true
    let lenFirstColumn : int = llst.Head.Length
    (* WB: 1 *)
    if lenFirstColumn = 0 then
        isValidTable <- false
        isValidTable
    else
        let mutable llstCopy = llst
        let mutable i = 1
        let lenRows = llst.Length
        while (i < lenRows) && isValidTable do
            llstCopy <- List.tail llstCopy
            (* WB: 2 *)
            if llstCopy.Head.Length <> lenFirstColumn then
                isValidTable <- false
            i <- i + 1
        isValidTable

//5g0(b)
///<summary>This function takes an 'a list list and returns the head in each embedded list.</summary>
///<param name = "llst">This a multidimensional 'a list.</param>
///<returns>Returns a list containing the heads of each of the list in the list.</returns>
let firstColumn (llst:'a list list) : 'a list =
    let mutable lstRes : 'a list = [] // Resultant 'a list
    let llstHasEmpty : bool list = List.map (fun (x: 'a list) -> x.IsEmpty) llst // Checks if llst has empty elements
    (* WB: 1 *)
    if List.contains true llstHasEmpty then
        lstRes <- []
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Head) llst
    lstRes

//5g0(c)
///<summary>This function takes a 'a list list and returns the tail in each embedded list.</summmary>
///<param name = "llst">This a multidimensional a' list.</param>
///<returns>Returns a list list containing the tails of each of the list in the list.</returns>
let dropFirstColumn (llst:'a list list) : 'a list list =
    let mutable lstRes : 'a list list = [[]]
    let llstHasEmpty = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    (* WB: 1 *)
    if List.contains (true) (llstHasEmpty) then
        lstRes <- [[]]
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Tail) llst
    lstRes

//5g0(d)
///<summary>transposeLstLst takes a multidimensional list and transposes it by using the funtions above.</summary>
///<param name = "llst">This a multidimensional list.</param>
///<returns>The function returns the transposed list.</returns>
let transposeLstLst (llst :'a list list) : 'a list list =
    let mutable a : 'a list list = llst
    let mutable b = []
    (* WB: 1 *)
    while List.forall (fun (x:'a list) -> x.Length > 0) a do
        b <- b @ [firstColumn a]
        a <- dropFirstColumn a
    b

(*
let InputList = [[1;2;3];[4;5;6]]
printfn "InputList <- %A" InputList
printfn "This is the result of the function isTable: %A" (isTable(InputList))
printfn "this is the result of the function firstColumn: %A" (firstColumn(InputList))
printfn "this is the result of the function dropFirstColumn: %A" (dropFirstColumn(InputList))
printfn "this is the result of the function transposeLstLst: %A" (transposeLstLst(InputList))
*)

printfn "White-box testing of isTable.fsx"
printfn "  Unit: isTable"
printfn "    Branch: 1a - %b" (isTable [[]] = false)
printfn "    Branch: 2a - %b" (isTable [[1;2];[1;2;2]] = false)
printfn "    Branch: 2b - %b" (isTable [[1;2];[1;2]] = true)

printfn "White-box testing of firstColumn.fsx"
printfn "  Unit: firstColumn"
printfn "    Branch: 1a - %b" (firstColumn [[]] = [])
printfn "    Branch: 1b - %b" (firstColumn [[1;2];[1;2]] = [1;1])

printfn "White-box testing of dropFirstColumn.fsx"
printfn "  Unit: dropFirstColumn"
printfn "    Branch: 1a - %b" (dropFirstColumn [[]] = [[]])
printfn "    Branch: 1b - %b" (dropFirstColumn [[1;2];[1;2]] = [[2];[2]])

printfn "White-box testing of transposeLstLst.fsx"
printfn "  Unit: transposeLstLst"
printfn "    Branch: 1a - %b" (transposeLstLst [[]] = [[]])
printfn "    Branch: 1b - %b" (transposeLstLst [[1;2];[1;2]] = [[1;1];[2;2]])