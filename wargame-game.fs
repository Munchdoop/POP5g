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

let rec game (board:card list) (player1:player) (player2:player) (count : int) : int =
    let draw1 = getCard(player1)
    let draw2 = getCard(player2)
    if fst draw1.Value > fst draw2.Value then //tjekker om player1 har vundet runden
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board //bordet der kan vindes
        let player1Update = snd draw1.Value
        let player1Next = addCards boardAppend player1Update
        let player2Next = snd draw2.Value
        let count0 = count + 1 
        if List.isEmpty player2Next then 
            printfn "%d" count0 
            1 
        //tjekker om player2 har vundet spillet
        else game [] player1Next player2Next count0 //hvis ingen vinder spillet så ny runde
    elif fst draw1.Value < fst draw2.Value then //player two wins round
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board
        let player2Update = snd draw2.Value
        let player1Next = snd draw1.Value
        let player2Next = addCards boardAppend player2Update
        let count0 = count + 1 
        if List.isEmpty player1Next then 
            printfn "%d" count0 
            2
        else game [] player1Next player2Next count0
    elif fst draw1.Value = fst draw2.Value then //war - but doesnt work
        let boardUpdate = fst draw1.Value :: fst draw2.Value :: board
        let player1Update = snd draw1.Value
        let player2Update = snd draw2.Value
        let count0 = count + 1 
        if getCard(player1Update) = None then 
            printfn "%d" count0 
            2
        elif getCard(player2Update) = None then 
            printfn "%d" count0 
            1
        else
            let cardFaceDown1 = getCard(player1Update) //kortet som ikke tælles med
            let cardFaceDown2 = getCard(player2Update) //kortet som ikke tælles med billedside ned
            let boardUpdate0 = fst cardFaceDown1.Value :: fst cardFaceDown2.Value :: boardUpdate
            let player1Update1 = snd cardFaceDown1.Value
            let player2Update1 = snd cardFaceDown2.Value
            if List.isEmpty player1Update1 then
                printfn "%d" count0 
                2
            elif List.isEmpty player2Update1 then
                printfn "%d" count0 
                1
            elif List.isEmpty player2Update1 && List.isEmpty player1Update1 then
                printfn "%d" count0 
                0
            else
                game boardUpdate0 player1Update1 player2Update1 count0
    else 0
         //gameend tie not implemented

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
let f = game [] player1 player2 0
printfn "%A" f


(*skal være tre outcome, a > b, a = b, a < b
skal også have en stop funktion som slutter når en player rammer 0
sørg for både at implementere vindere og ties *)

