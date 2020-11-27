namespace InOut

module cat =
    [<EntryPoint>]
    let main (args:string array) =
        let myList = Array.toList args
        let res = RNW.cat myList
        printfn "%s" (Option.get(res))
        (*
        for a in myList do
            let data = System.IO.File.OpenText(a)
            printfn "%s" (data.ReadToEnd())
        *)
        //printfn "%A" myList
        0