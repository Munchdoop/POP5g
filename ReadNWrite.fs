open System 
let readFile (filename:string) : string option =
    try 
        let ff = IO.File.OpenText(filename)
        Some (ff.ReadToEnd())
    with 
        |_ ->   None


let rec cat (filenames:string list) : string option =
    match filenames with
        | x :: xs -> 
            let x1 = readFile(x)
            match x1 with
                | None -> None
                | Some y -> Some cat(xs)

//cat 
//list.map .value
//list.fold

//tac
//useful functions:
//list.rev
//list.spilt
//list.map 