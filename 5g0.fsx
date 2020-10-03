/// 5g0(a)
/// <summary>Takes a two-dimensional list and returns <c>true</c> if all inner list elements contain at least one element and are of the same size.</summary>
/// <param name="llst">A two-dimensional list.</param>
/// <returns>A <typeparamref name="boolean"/> statement.</returns>
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

/// 5g0(b)
/// <summary>Takes a two-dimensional list and returns the head of each embedded list element.</summary>
/// <param name="llst">A two-dimensional list.</param>
/// <returns>A one-dimensional list containing the head of each list element in <paramref name="llst"/>.</returns>
let firstColumn (llst:'a list list) : 'a list =
    let mutable lstRes : 'a list = [] // Resultant 'a list
    let llstHasEmpty : bool list = List.map (fun (x: 'a list) -> x.IsEmpty) llst // Checks if llst has empty elements
    (* WB: 1 *)
    if List.contains true llstHasEmpty then
        lstRes <- []
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Head) llst
    lstRes

/// 5g0(c)
/// <summary>Takes a two-dimensional list and returns the tail of each embedded list element.</summary>
/// <param name="llst">A two-dimensional list.</param>
/// <returns>A two-dimensional list containing the tail of each list element in <paramref name="llst"/>.</returns>
let dropFirstColumn (llst:'a list list) : 'a list list =
    let mutable lstRes : 'a list list = [[]]
    let llstHasEmpty = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    (* WB: 1 *)
    if List.contains (true) (llstHasEmpty) then
        lstRes <- [[]]
    else
        lstRes <- List.map (fun (x : 'a list) -> x.Tail) llst
    lstRes

/// 5g0(d)
/// <summary>Transposes a two-dimensional list by using the funtions <c>firstColumn</c> and <c>dropFirstColumn</c>.</summary>
/// <param name="llst">A two-dimensional list.</param>
/// <returns>The transposed two-dimensional list of <paramref name="llst"/>.</returns>
/// <seealso cref="firstColumn : 'a list list -> 'a list"/>
/// <seealso cref="DropFirstColumn : 'a list list -> 'a list list"/>
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

// BEGIN WHITEBOX TESTING

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

// END WHITEBOX TESTING
*)