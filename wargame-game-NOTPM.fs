namespace Wargame

type player = deck

module GameImperative =

    let deckplay = Shuffle.newdeck()
    let players = Shuffle.deal(deckplay)
    let player1 = fst players
    let player2 = snd players

    let duration f = 
        let timer = new System.Diagnostics.Stopwatch()
        timer.Start()
        let returnValue = f()
        printfn "Elapsed Time: %i" timer.ElapsedMilliseconds
        returnValue
    
    //getCard function 
    ///<summary>Takes a player type and returns a tuple containing the head and the tail of players as an option.</summary>
    ///<param name="play">A type player, see also player.</param>
    ///<returns>A tuple containing the head and tail of <paramref name="player"/> as an option.</returns>
    let getCard (play : player ) : (card * player) option =
        match play with
        | [] -> None
        | x :: xs -> Some(x,xs)
    
    //addCards function
    ///<summary>This function takes a player type and returns a list of integers</summary>
    ///<param name = "board"> a player type </param>
    ///<param name = "decks"> a deck type </param>
    ///<returns> a deck with board appended and shuffled </returns>
    let addCards (board : player) (decks : deck) : player =
        let shuffle1 = Shuffle.shuffle(board)
        let a = decks @ shuffle1
        a
    
    //game function
    ///<summary> a recursive function that plays a game of the card game war </summary>
    ///<param name="board"> a int list </param>
    ///<param name="player1"> a player which is a int list </param>
    ///<param name="player2"> a player which is a int list </param>
    ///<param name="count"> an int </param>
    ///<returns> returns the result of the war game represented as either 0, 1 or 2 and number of plays </returns>
    let rec game (board:card list) (player1:player) (player2:player) (count : int) : int*int =
        let draw1 = getCard(player1)
        let draw2 = getCard(player2)
        if fst draw1.Value > fst draw2.Value then //tjekker om player1 har vundet runden
            let boardAppend = fst draw1.Value :: fst draw2.Value :: board //bordet der kan vindes
            let player1Update = snd draw1.Value
            let player1Next = addCards boardAppend player1Update
            let player2Next = snd draw2.Value
            let count0 = count + 1 
            //printfn "%A %A" player1Next player2Next
            if List.isEmpty player2Next then 
                (count0, 1) 
            //tjekker om player2 har vundet spillet
            else game [] player1Next player2Next count0 //hvis ingen vinder spillet så ny runde
        elif fst draw1.Value < fst draw2.Value then //player two wins round
            let boardAppend = fst draw1.Value :: fst draw2.Value :: board
            let player2Update = snd draw2.Value
            let player1Next = snd draw1.Value
            let player2Next = addCards boardAppend player2Update
            let count0 = count + 1 
            //printfn "%A %A" player1Next player2Next 
            if List.isEmpty player1Next then 
                (count0, 2)
            else game [] player1Next player2Next count0
        elif fst draw1.Value = fst draw2.Value then //war - but doesnt work
            let boardUpdate = fst draw1.Value :: fst draw2.Value :: board
            let player1Update = snd draw1.Value
            let player2Update = snd draw2.Value
            let count0 = count + 1 
            //printfn "%A %A" player1Update player2Update 
            if getCard player1Update = None then 
                (count0, 2)
            elif getCard player2Update = None then 
                (count0,1)
            else
                let cardFaceDown1 = getCard(player1Update) //kortet som ikke tælles med
                let cardFaceDown2 = getCard(player2Update) //kortet som ikke tælles med billedside ned
                let boardUpdate0 = fst cardFaceDown1.Value :: fst cardFaceDown2.Value :: boardUpdate
                let player1Update1 = snd cardFaceDown1.Value
                let player2Update1 = snd cardFaceDown2.Value
                //printfn "%A %A" player1Update1 player2Update1 
                if List.isEmpty player1Update1 then
                    (count0,2)
                elif List.isEmpty player2Update1 then
                    (count0,1)
                elif List.isEmpty player2Update1 && List.isEmpty player1Update1 then
                    printfn "%d" count0
                    printfn "%A %A" player1Update1 player2Update1 
                    (count0,0)
                else
                    game boardUpdate0 player1Update1 player2Update1 count0
        else (count, 0)
    
    //test and stats
    ///<summary> counts wins, ties and average game length. </summary>
    ///<param name = "n"> an int </param>
    ///<returns> returns a string containing wins, ties and average game length. </param>
    let test (n:int) =
        let mutable winsPlayer1 = 0 
        let mutable winsPlayer2 = 0
        let mutable ties = 0
        let mutable accumulatedPlays = 0 
        for i = 1 to n do 
            let res = game [] player1 player2 0
            accumulatedPlays <- fst res + accumulatedPlays
            if snd res = 1 then
                winsPlayer1 <- winsPlayer1 + 1  
                winsPlayer1
            elif snd res = 2 then
                winsPlayer2 <- winsPlayer2 + 1
                winsPlayer2
            elif snd res = 0 then 
                ties <- ties + 1 
                ties
            else
                5
            |> ignore
        let average = accumulatedPlays / n 
        printfn "Wins player1: %A, wins player2: %A, ties: %A, average: %A" winsPlayer1 winsPlayer2 ties average
    printfn "%A" (duration (fun() -> test 10000))