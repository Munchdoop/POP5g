//5g0(a)
///<summary>This function takes an a' list list as an argument and returns a true statement if all lists contain at least one element and are of the same size.</summary>
///<param name="llst">A multidimensinal list.</param>
///<returns>A boolean true or false statement.</returns>
let isTable (llst:'a list list) : bool =
    let mutable valid : bool = true
    let len : int = llst.Head.Length
    //printfn "Length of inital element: %i" len
    if len = 0 then
        valid <- false
        //printfn "Initial element is empty, breaking"
        valid
    else
        //printfn "Length is not empty, proceeding"
        let mutable i = 1
        let lenA = llst.Length
        let mutable llst2 = llst
        while (i < lenA) && valid do
            //printfn "Loop iteration: %i" i
            llst2 <- List.tail llst2
            if llst2.Head.Length <> len then
                //printfn "Found inequality, breaking"
                valid <- false
            i <- i + 1
        valid
printfn "this is the result of the function isTable: %A" (isTable([[1;2;3];[4;5;6]]))

//5g0(b)
///<summary>This function takes an 'a list list and returns the head in each embedded list.</summary>
///<param name = "llst">This a multidimensional a' list.</param>
///<returns>Returns a list containing the heads of each of the list in the list.</returns>
let firstColumn (llst:'a list list) : 'a list =
    let mutable llst2 : 'a list = []
    let llst3 = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    if List.contains (true) (llst3) then
        llst2 <- []
    else
        llst2 <- List.map (fun (x : 'a list) -> x.Head) llst
    llst2
printfn "this is the result of the function firstColumn: %A" (firstColumn([[1;2;3];[4;5;6]]))

//5g0(c)
///<summary>This function takes a 'a list list and returns the tail in each embedded list.</summmary>
///<param name = "llst">This a multidimensional a' list.</param>
///<returns>Returns a list list containing the tails of each of the list in the list.</returns>
let dropFirstColumn (llst:'a list list) : 'a list list =
    let mutable llst2 : 'a list list = [[]]
    let llst3 = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    if List.contains (true) (llst3) then
        llst2 <- [[]]
    else
        llst2 <- List.map (fun (x : 'a list) -> x.Tail) llst
    llst2
printfn "this is the result of the function dropFirstColumn: %A" (dropFirstColumn([[1;2;3];[4;5;6]]))

//5g0(d)
///<summary>transposeLstLst takes a multidimensional list and transposes it by using the funtions above.</summary>
///<param name = "llst">This a multidimensional list.</param>
///<returns>The function returns the transposed list.</returns>
let transposeLstLst (llst :'a list list) : 'a list list =
    let mutable (a: 'a list list) = llst
    let mutable b = []
    while List.forall (fun (x:'a list) -> x.Length > 0) a do
        b <- b @ [firstColumn a]
        a <- dropFirstColumn a
    b
printfn "this is the result of the function transposeLstLst: %A" (transposeLstLst([[1;2;3];[4;5;6]]))