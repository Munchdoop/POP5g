
let isTable (llst: 'a list list) : bool =
    if List.length llst < 2 then false
    else
        if List.length llst.[0] = 0 || List.length llst.[1] = 0 then false 
        else   
            if List.length llst.[0] = List.length llst.[1] then true
            else false

let trying = [[];[]]
printfn "%A" (List.length trying)
printfn "%A" (isTable(trying))

(*
let isTable (llst: 'a list) : bool =
    if List.isEmpty llst then false
    else true 
 
let trying = []
printfn "%A" (isTable(trying))
*)

let isTable (llst:'a list list) : bool =
    let mutable valid : bool = true
    let len : int = llst.Head.Length
    printfn "Length of inital element: %i" len
    if len = 0 then
        valid <- false
        printfn "Initial element is empty, breaking"
        valid
    else
        printfn "Length is not empty, proceeding"
        (*for elm in llst do
            printfn "%i > %i" elm.Length len
            if elm.Length <> len then
                valid <- false
        valid*)
        let mutable i = 1
        let lenA = llst.Length
        let mutable llst2 = llst
        while (i < lenA) && valid do
            printfn "Loop iteration: %i" i
            llst2 <- List.tail llst2
            if llst2.Head.Length <> len then
                printfn "Found inequality, breaking"
                valid <- false
            i <- i + 1
        valid

let isTable2 (llst:'a list list) : bool =
    let mutable valid : bool = true
    let len : int = llst.Head.Length
    printfn "Length of inital element: %i" len
    if len = 0 then
        valid <- false
        printfn "Initial element is empty, breaking"
        valid
    else
        printfn "Length is not empty, proceeding"
        for elm in llst do
            printfn "Loop iteration: %A" elm
            //printfn "%i > %i" elm.Length len
            if elm.Length <> len then
                valid <- false
        valid

(*let firstColumn (llst:'a list list) : 'a list =
    let mutable llst2 : 'a list = llst.Head
    let len : int = llst.Length
    let mutable i = 1
    let mutable counter = llst.Head.Head
    //let row = llst.Item 0 in printfn "%A" row
    while (i < len) do
        printfn "%A" llst2.Tail
        llst2 <- List.append 
        i <- i + 1
    llst2*)

let firstColumn (llst:'a list list) : 'a list =
    let mutable newList = []
    let mutable counter = 1
    for elm in llst do
        newList <- newList @ elm
        for elm2 in elm do
        counter <- counter + 1
    newList

let myTable = [[1;2;3];[4;5;6];[7;8;9];[10;11;12]]
firstColumn myTable
printfn "%b" (isTable myTable)
printfn "%b" (isTable2 myTable)