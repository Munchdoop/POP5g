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

let boardUpdate (player1) (player2) (winner)=
    let boardAppend = challenge

let game : int =
    let rec gameHelper (board:card list) (player1:player) (player2:player) =
        let challenge = getCard player1, getCard player2
        let (a, b) = fst challenge, snd challenge
        printfn "CHALLENGE: %A vs. %A" (fst a.Value) (fst b.Value) 
        match challenge with
            | x, y when fst x.Value > fst y.Value ->
                let boardAppend = fst x.Value :: fst y.Value :: board
                let player1Update = snd x.Value
                let player1Next = addCards boardAppend player1Update
                let player2Next = snd y.Value
                printfn "p1 has %i cards remaining, p2 has %i cards remaining" (player1Next.Length) (player2Next.Length)
                match player2Next with
                    | [] -> 1
                    | _ -> 
                        gameHelper [] player1Next player2Next
            | x, y when fst x.Value < fst y.Value ->
                let boardAppend = fst x.Value :: fst y.Value :: board
                let player2Update = snd y.Value
                let player1Next = snd x.Value
                let player2Next = addCards boardAppend player2Update
                printfn "p1 has %i cards remaining, p2 has %i cards remaining" (player1Next.Length) (player2Next.Length)
                match player1Next with
                    | [] -> 2
                    | _ -> 
                        gameHelper [] player1Next player2Next
            | x, y when fst x.Value = fst y.Value ->
                let boardUpdate = fst x.Value :: fst y.Value :: board
                let player1Update = snd x.Value
                let player2Update = snd y.Value
                match x, y with
                    | x1, y1 when x1 = None -> 2
                    | x1, y1 when y1 = None -> 1
                    | _ ->
                        let cardFaceDown1 = getCard(player1Update)
                        let cardFaceDown2 = getCard(player2Update)
                        let boardupdate0 = fst cardFaceDown1.Value :: fst cardFaceDown2.Value :: boardUpdate
                        //printfn "BOARDUPDATE: %A" boardupdate0
                        let player1Update1 = snd cardFaceDown1.Value
                        let plyaer2Update1 = snd cardFaceDown2.Value
                        match player1Update1, plyaer2Update1 with
                            | c, d when c = [] -> 2
                            | c, d when d = [] -> 1
                            | c, d when c = [] && d = [] -> 0
                            | _ -> gameHelper boardupdate0 player1Update1 plyaer2Update1
            | _ -> -1
    gameHelper [] player1 player2

let f = game
printfn "%A" f