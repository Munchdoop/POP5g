open System
open System.Globalization

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

module cat =
    [<EntryPoint>]
    let main (args:string array) =
        try
            let myList = Array.toList args
            let res = cat myList
            printfn "%s" (Option.get(res))
            printfn "%i" 0
            0
        with
            | _ ->
                printfn "%i" 1
                1

    (*
    //type State = (Item option)
    
    let mutable itemArray = Array2D.create h w ((None) : State)

    member this.SetWall (w : Wall, coords : (int*int)) =
        (fun (i,j) -> itemArray.[i,j] <- (Some (w :> Item))) coords
    
    member this.CreateRoom(w:int, h:int) = //add startPost(x,y)
        let mutable wallList = []
        for i=0 to h do
            wallList <- Wall(i,0) :: wallList
            wallList <- Wall(i, h) :: wallList
        for j=0 to w do
            wallList <- Wall(0, j) :: wallList
            wallList <- Wall(w, j) :: wallList
        wallList
    
    // BufferCanvas
    member this.BufferCanvas (c:Canvas) =
        for i=0 to h-1 do
            for j=0 to w-1 do
                match itemArray.[i,j] with
                    | Some w when w.Flag = true -> 
                        itemArray.[i,j] <- None
                        level.Set(i,j,'.',Color.White,Color.Black)
                    | Some w -> w.RenderOn(level)
                    | _ -> level.Set(i,j,'.',Color.White,Color.Black)
    *)

//for wall in wallList do