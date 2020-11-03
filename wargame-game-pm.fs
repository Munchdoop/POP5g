module Wargame

type card = int
type deck = card list
type player = deck

let deckplay = Wargame_shuffle.newdeck()

let players = Wargame_shuffle.deal(deckplay)

let player1 = fst players

let player2 = snd players

let getCard (play : player ) : (card * player) option =
    match play with
    | [] -> None
    | x :: xs -> Some(x,xs)

let addCards (board : player) (decks : deck) : player =
    let shuffle1 = Wargame_shuffle.shuffle(board)
    let a = decks @ shuffle1
    a

let rec game (board:deck) (player1:player) (player2:player) (acc:int) : int*int =
    match getCard player1, getCard player2 with
        | None, None -> 0, acc
        | Some p1Card, None -> 1, acc
        | None, Some p2Card -> 2, acc
        | Some (p1Card, p1Deck), Some (p2Card, p2Deck) ->
            let cardPile : deck = p1Card :: p2Card :: board
            match p1Card, p2Card with
                | p1Card, p2Card when p1Card > p2Card -> 
                    game [] (addCards p1Deck cardPile) p2Deck (acc+1)
                | p1Card, p2Card when p1Card < p2Card ->
                    game [] p1Deck (addCards p2Deck cardPile) (acc+1)
                | p1Card, p2Card when p1Card = p2Card ->
                    game cardPile p1Deck p2Deck (acc+1)
                | _ -> -1, acc

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
    let average = accumulatedPlays / n 
    printfn "Wins player1: %A, wins player2: %A, ties: %A, average: %A" winsPlayer1 winsPlayer2 ties average
printfn "%A" (test 10000)