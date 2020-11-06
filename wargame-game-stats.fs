//test and stats
///<summary> counts wins, ties and average game length. </summary>
///<param name = "n"> an int </param>
///<returns> returns a string containing wins, ties and average game length.</param>

let duration f = 
    let timer = new System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
    returnValue

let test (n:int) =
    let mutable winsPlayer1 = 0 
    let mutable winsPlayer2 = 0
    let mutable ties = 0
    let mutable accumulatedPlays = 0 
    for i = 1 to n do 
        let res = Wargame.Game.game [] Wargame.Game.player1 Wargame.Game.player2 0
        accumulatedPlays <- snd res + accumulatedPlays
        match fst res with
            | r when r = 1 ->
                winsPlayer1 <- winsPlayer1 + 1  
                winsPlayer1
            | r when r = 2 ->
                winsPlayer2 <- winsPlayer2 + 1
                winsPlayer2
            | r when r = 0 ->
                ties <- ties + 1 
                ties
            | _ -> 5
            |> ignore
    let average = accumulatedPlays / n 
    printfn "Wins player1: %A, wins player2: %A, ties: %A, average: %A" winsPlayer1 winsPlayer2 ties average

printfn "%A" (duration (fun() -> test 10000))