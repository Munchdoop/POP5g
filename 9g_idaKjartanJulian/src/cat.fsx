namespace InOut

///<summary> takes user-given files and prints them in terminal </summary>
///<param name="args"> the files the user wants to read </param>
///<returns> if file exist then it prints the contents to screen and 0 </returns>
module cat =
    [<EntryPoint>]
    let main (args:string array) =
        try
            let myList = Array.toList args
            let res = RNW.cat myList
            printfn "%s" (Option.get(res))
            printfn "%i" 0
            0
        with
            | _ ->
                printfn "%i" 1
                1
