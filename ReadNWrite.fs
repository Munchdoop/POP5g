open System 
let readFile (filename:string) : string option =
    try 
        let ff = IO.File.OpenText(filename)
        Some (ff.ReadToEnd())
    with 
        |_ ->   None


let cat (filenames:string list) : string option =
    let rec catHelper (x:string list) : string =
        match x with
            | x::xs ->
                let x1 = readFile(x)
                match x1 with
                    | Some y -> y :: (catHelper xs)
                    | None -> string -1
            | x ->
                let x1 = readFile(x)
                match x1 with
                    | None -> None
                    | Some y -> y
    let res = Some catHelper (filenames)
    res


//tac & cat 
//useful functions:
//list.rev
//list.spilt
//list.map 
//Option.get()
//list.fold