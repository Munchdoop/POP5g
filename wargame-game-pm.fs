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

let game : int =
    let rec gameHelper (board:card list) (player1:player) (player2:player) =
        let challenge = getCard player1, getCard player2
        match challenge with
            | x, y when fst x.Value > fst y.Value ->
                let boardAppend = fst x.Value :: fst y.Value :: board
                let player1Update = snd x.Value
                let player1Next = addCards boardAppend player1Update
                let player2Next = snd y.Value
                match player2Next with
                    | [] -> 1
                    | _ -> gameHelper [] player1Next player2Next
            | x, y when fst x.Value < fst y.Value ->
                2
            | x, y when fst x.Value = fst y.Value ->
                0
            | _ -> -1
    gameHelper [] player1 player2

let f = game
printfn "%A" f