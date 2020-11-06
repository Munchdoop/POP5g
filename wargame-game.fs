namespace Wargame

type player = deck

module Game =
    let deckplay = Shuffle.newdeck()
    let players = Shuffle.deal(deckplay)
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
        let shuffle1 = Shuffle.shuffle(board)
        let a = decks @ shuffle1
        a

    //game function
    ///<summary> a recursive function that plays a game of the card game war </summary>
    ///<param name="board"> an int list </param>
    ///<param name="player1"> a player which is a int list </param>
    ///<param name="player2"> a player which is a int list </param>
    ///<returns> returns the result of the war game represented as either 0, 1 or 2 </returns>
    let rec game (board:deck) (player1:player) (player2:player) (acc:int) : int*int =
        match getCard player1, getCard player2 with
            | None, None -> 0, acc
            | Some p1Card, None -> 1, acc
            | None, Some p2Card -> 2, acc
            | Some (p1Card, p1Deck), Some (p2Card, p2Deck) ->
                let cardPile : deck = p1Card :: p2Card :: board
                match p1Card, p2Card with
                    | p1Card, p2Card when p1Card > p2Card -> 
                        game [] (addCards cardPile p1Deck) p2Deck (acc+1)
                    | p1Card, p2Card when p1Card < p2Card ->
                        game [] p1Deck (addCards cardPile p2Deck) (acc+1)
                    | p1Card, p2Card when p1Card = p2Card ->
                        match getCard p1Deck, getCard p2Deck with 
                            | None, None ->
                                game (cardPile) [] [] (acc+1)
                            | Some (p1CardWar, p1CardDeckWar), None ->
                                game (p1CardWar :: cardPile) p1CardDeckWar [] (acc+1)
                            | None, Some (p2CardWar, p2CardDeckWar) ->
                                game (p2CardWar :: p2CardWar :: cardPile) [] p2CardDeckWar (acc+1)
                            | Some (p1CardWar, p1CardDeckWar), Some (p2CardWar, p2CardDeckWar) ->
                                game (p1CardWar:: p2CardWar :: cardPile) p1CardDeckWar p2CardDeckWar (acc+1)
                    | _ -> -1, acc