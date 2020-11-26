namespace InOut
open System

module RNW =
    let readFile (filename:string) : string option =
        try 
            let ff = IO.File.OpenText(filename)
            Some (ff.ReadToEnd())
        with 
            |_ ->   None


    let cat (filenames:string list) : string option =
        //let concat (acc:string) (elm:string) = acc + elm
        let rec catHelper (s:string list) : string option =
            match s with
                | [] -> None
                | [x] ->
                    let myX = readFile(x)
                    match myX with
                        | None -> None
                        | Some y -> Some y
                //| x::xs ->
                //    x |> readFile |> Option.bind (Option.get(catHelper xs))
                | x::xs ->
                    let myX = readFile(x)
                    match myX with
                        | None -> None
                        | Some y -> Some (y + Option.get(catHelper xs))
        catHelper filenames



//tac & cat 
//useful functions:
//list.rev
//list.spilt
//list.map 
//Option.get()
//list.fold