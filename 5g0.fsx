

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
//alternative løsning - lidt dårligere køretid
(*
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
*)

//5g0(b)
let firstColumn (llst:'a list list) : 'a list =
    let mutable llst2 : 'a list = []
    let llst3 = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    if List.contains (true) (llst3) then
        llst2 <- []
    else 
        llst2 <- List.map (fun (x : 'a list) -> x.Head) llst
    llst2


//5g0c
let dropFirstColumn (llst:'a list list) : 'a list list =
    let mutable llst2 : 'a list list = [[]]
    let llst3 = List.map (fun (x: 'a list) -> x.IsEmpty) llst
    if List.contains (true) (llst3) then
        llst2 <- [[]]
    else 
        llst2 <- List.map (fun (x : 'a list) -> x.Tail) llst
    llst2

//5g0d
let transposeLstLst (llst :'a list list) : 'a list list =
    let mutable (a: 'a list list) = llst
    let mutable b = []
    while List.forall (fun (x:'a list) -> x.Length > 0) a do
        b <- b @ [firstColumn a]
        a <- dropFirstColumn a
    b

let myTable = [[1;2;3;4];[4;5;6;4];[7;8;9;4]]
transposeLstLst myTable