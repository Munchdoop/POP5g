///<summary =""></summary>
type card = int
type deck = card list
type player = deck

let deckplay = Wargame_shuffle.newdeck()
let players = Wargame_shuffle.deal(deckplay)
let player1 = fst players
let player2 = snd players

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
    let shuffle1 = Wargame_shuffle.shuffle(board)
    let a = decks @ shuffle1
    a

//game function
///<summary> a recursive function that plays a game of the card game war </summary>
///<param name="board"> a int list </param>
///<param name="player1"> a player which is a int list </param>
///<param name="player2"> a player which is a int list </param>
///<returns> returns the result of the war game represented as either 0, 1 or 2 </returns>
let rec game (board:card list) (player1:player) (player2:player) : int =
    let draw1 = getCard(player1)
    let draw2 = getCard(player2)
    if fst draw1.Value > fst draw2.Value then //checks is player1 wins round
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board
        let player1Update = snd draw1.Value
        let player1Next = addCards boardAppend player1Update
        let player2Next = snd draw2.Value
        if List.isEmpty player2Next then 
            1 
        //checks if player1 won game
        else game [] player1Next player2Next  //if no winner 
    elif fst draw1.Value < fst draw2.Value then //player two wins round
        let boardAppend = fst draw1.Value :: fst draw2.Value :: board
        let player2Update = snd draw2.Value
        let player1Next = snd draw1.Value
        let player2Next = addCards boardAppend player2Update
        if List.isEmpty player1Next then 
            2
        else game [] player1Next player2Next
    elif fst draw1.Value = fst draw2.Value then //war
        let boardUpdate = fst draw1.Value :: fst draw2.Value :: board
        let player1Update = snd draw1.Value
        let player2Update = snd draw2.Value
        if getCard player1Update = None then 
            2
        elif getCard player2Update = None then 
            1
        else
            let cardFaceDown1 = getCard(player1Update) 
            let cardFaceDown2 = getCard(player2Update)
            let boardUpdate0 = fst cardFaceDown1.Value :: fst cardFaceDown2.Value :: boardUpdate
            let player1Update1 = snd cardFaceDown1.Value
            let player2Update1 = snd cardFaceDown2.Value
            if List.isEmpty player1Update1 then
                2
            elif List.isEmpty player2Update1 then
                1
            elif List.isEmpty player2Update1 && List.isEmpty player1Update1 then
                0
            else
                game boardUpdate0 player1Update1 player2Update1 
    else 0

let f = game [] player1 player2
printfn "%A" f