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
let c = fst a.Value
//printfn "%A" a 
//printfn "%A" c

let addCards (plays : player) (decks : deck) : player =
    let shuffle1 = Wargame_shuffle.shuffle(plays)
    let a = decks @ shuffle1
    a 

let b = addCards [1;2] [3;4;5]
printfn "%A" b


let rec game (board:card list) (player1:player) (player2:player) : int =
    let draw1 = getCard(player1)
    let draw2 = getCard(player2)
    //printfn "%A %A" a b //test  
    if fst draw1.Value > fst draw2.Value then //tested and works 
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board
        let boardShuffle = Wargame_shuffle.shuffle(boardAppend)
        let player1Update = snd draw1.Value
        let player1Next = addCards boardShuffle player1Update
        let player2Next = snd draw2.Value
        printfn "player1" //test   
        if List.isEmpty player1 then 2 
        elif List.isEmpty player2 then 1 
        else game [] player1Next player2Next
    elif fst draw1.Value < fst draw2.Value then //tested and works
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board
        let boardShuffle = Wargame_shuffle.shuffle(boardAppend)
        let player2Update = snd draw2.Value
        let player2Next = addCards boardShuffle player2Update
        let player1Next = snd draw1.Value 
        printfn "player2" //test 
        if List.isEmpty player1 then 2 
        elif List.isEmpty player2 then 1 
        else game [] player1Next player2Next
    elif fst draw1.Value = fst draw2.Value then //war - but doesnt work 
        let c = fst draw1.Value :: board 
        let d = fst draw2.Value :: c
        let cardFaceDown1 = getCard(player1)
        let cardFaceDown2 = getCard (player2)
        let facedownappend1 = fst cardFaceDown1.Value :: d 
        let facedownappend2 = fst cardFaceDown2.Value :: facedownappend1
        let newWar1 = getCard(player1)
        let newWar2 = getCard(player2)
        if fst newWar1.Value > fst newWar2.Value then 4 
        elif fst newWar1.Value < fst newWar2.Value then 5
        else 3       
    else 0 //gameend tie not implemented

(*let krig (play1: player) (play2:player) =
    let board = []
    let a = getCard(play1)
    let b = getCard(play2)
    let c = fst a.Value :: board 
    let d = fst b.Value :: c
    d
*)
//let g = krig player1 player2 
//printf "%A" g
let f = game [] player1 player2
printfn "%A" f


(*skal være tre outcome, a > b, a = b, a < b
skal også have en stop funktion som slutter når en player rammer 0
sørg for både at implementere vindere og ties *)

