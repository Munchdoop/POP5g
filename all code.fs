namespace InOut
open System
open System.Globalization

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
                | [] -> 
                    failwith "Error: file does not exist"
                    None
                | [x] ->
                    let myX = readFile(x)
                    match myX with
                        | None -> 
                            failwith "Error: file does not exist"
                            None
                        | Some y -> Some y
                //| x::xs ->
                //    x |> readFile |> Option.bind (Option.get(catHelper xs))
                | x::xs ->
                    let myX = readFile(x)
                    match myX with
                        | None -> 
                            failwith "Error: file does not exist"
                            None
                        | Some y -> Some (y + Option.get(catHelper xs))
        catHelper filenames
    
    let rev str = //this is a help-function to tac
        StringInfo.ParseCombiningCharacters(str) 
        |> Array.rev
        |> Seq.map (fun i -> StringInfo.GetNextTextElement(str, i))
        |> String.concat ""

    let tac (filenames:string list) : string option =
        let rec tachelper (str:string list) : string option =
            match str with
                | [] -> 
                    failwith "Error: file does not exist"
                    None 
                | [x] -> 
                    let thisX = readFile(x)
                    match thisX with 
                        | None -> 
                            failwith "Error: file does not exist"
                            None
                        | Some y -> Some (rev y) 
                | x :: xs -> 
                    let thisX = readFile(x)
                    match thisX with 
                        | None -> 
                            failwith "Error: file does not exist" 
                            None 
                        | Some y -> 
                            Some (Option.get(tachelper xs) + (rev y)) //reverses the order of txt files
        tachelper filenames 



//tac & cat 
//useful functions:
//list.rev
//list.spilt
//list.map 
//Option.get()
//list.fold

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

namespace InOut

module tac =
    [<EntryPoint>]
    let main (args: string array) = 
        let myList = Array.toList args 
        let res = RNW.tac myList
        printfn "%s" (Option.get(res))
        0
        