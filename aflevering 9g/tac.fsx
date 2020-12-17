namespace InOut

///<summary> takes user-given files and prints them in terminal but in reverse </summary>
///<param name="args"> the files the user wants to read </param>
///<returns> if file exist then it prints the contents in reverse to screen and 0 </returns>
module tac =
    [<EntryPoint>]
    let main (args: string array) =
        try
            let myList = Array.toList args
            let reverse = List.rev myList
            let res = RNW.tac reverse
            printfn "%s" (Option.get(res))
            printfn "%i" 0
            0
        with
            | _ ->
                printfn "%i" 1
                1
