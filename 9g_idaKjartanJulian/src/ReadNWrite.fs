namespace InOut
open System
open System.Globalization

module RNW =
    ///<summary> reads the file and saves it as an option-type
    ///<param name="filename"> takes a user-given filename </param>
    ///<returns> a option-type or None </returns>
    let readFile (filename:string) : string option =
        try
            let ff = IO.File.OpenText(filename)
            Some (ff.ReadToEnd())
        with
            |_ ->   None

    ///<summary> takes user-given files and prints them in terminal </summary>
    ///<param name="args"> the files the user wants to read </param>
    ///<returns> if file exist then it prints the contents to screen and 0 </returns>
    let cat (filenames:string list) : string option =
        let rec catHelper (s:string list) (acc:string) : string option =
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
                        | Some y -> Some (acc+y)
                | x::xs ->
                    let content = readFile(x)
                    match content with
                        | None ->
                            failwith "Error: file does not exist"
                            None
                        | Some content -> catHelper xs (acc+content)
        catHelper filenames ""

    ///<summary> takes a string and reverses the order of the letters </summary>
    ///<param name="str"> the string that is to be reversed </param>
    ///<returns> returns the now reversed string </returns>
    let rev (str:string) : string = //this is a help-function to tac
        StringInfo.ParseCombiningCharacters(str)
        |> Array.rev
        |> Seq.map (fun i -> StringInfo.GetNextTextElement(str, i))
        |> String.concat ""

    ///<summary> takes user-given files and prints them in terminal but in reverse </summary>
    ///<param name="args"> the files the user wants to read </param>
    ///<returns> if file exist then it prints the contents in reverse to screen and 0 </returns>
    let tac (filenames:string list) : string option =
        let rec tacHelper (s:string list) (acc:string) : string option =
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
                        | Some y -> Some (acc + rev y)
                | x::xs ->
                    let content = readFile(x)
                    match content with
                        | None ->
                            failwith "Error: file does not exist"
                            None
                        | Some content -> tacHelper xs (rev content + acc)
        tacHelper filenames ""