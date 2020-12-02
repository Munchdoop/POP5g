//timer function
/// <summary>A function wrapper which starts a timer when an input function <paramref name="f"/> executes,
/// and prints the elapsed time to the console after function execution is completed.</summary>
/// <param name="f">A function.</param>
/// <returns>The return value of <paramref name="f"/>.</returns>
let duration f = 
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let returnValue = f()
    timer.Stop()
    let ts = timer.Elapsed
    printfn "\aElapsed time: %02i:%02i:%02i.%02i"  ts.Hours ts.Minutes ts.Seconds (ts.Milliseconds / 10)
    returnValue

//test and stats
/// <summary> counts wins, ties and average game length. </summary>
/// <param name = "n"> an int </param>
/// <returns> returns a string containing wins, ties and average game length.</param>
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

printf "Enter number of tests to execute: "
let n = 10000
printfn "%A" (duration (fun() -> test n))