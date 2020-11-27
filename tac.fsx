namespace InOut

module tac =
    [<EntryPoint>]
    let main (args: string array) = 
        let myList = Array.toList args 
        let res = RNW.tac myList
        printfn "%s" (Option.get(res))
        0
        