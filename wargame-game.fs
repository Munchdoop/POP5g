type card = int
type deck = card list 
type player = deck 

let deckplay = Wargame_shuffle.newdeck()

let players = Wargame_shuffle.deal(deckplay)

let player1 = fst players

let player2 = snd players 

//printfn "%A %A %A %A" deckplay players player1 player2 //test

let getCard (play : player ) : (card * player) option =
    match play with 
    | [] -> None 
    | x :: xs -> Some(x,xs)

let a = getCard(player1)
printfn "%A" a

let addCards (plays : player) (decks : deck) : player =
    let shuffle1 = Wargame_shuffle.shuffle(plays)
    let a = decks @ shuffle1
    a 

let b = addCards [1;2] [3;4;5]
printfn "%A" b



(*skal være tre outcome, a > b, a = b, a < b
skal også have en stop funktion som slutter når en player rammer 0
sørg for både at implementere vindere og ties *)

