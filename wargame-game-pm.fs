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

let f = game [] player1 player2 0
printfn "%A" f