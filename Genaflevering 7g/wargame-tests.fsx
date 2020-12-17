let shuffleprint =
    let res =  Wargame.Shuffle.newdeck()
    printfn "Shuffle and newdeck: %A" res
    res

let dealprint = 
    let res = Wargame.Shuffle.deal(shuffleprint)
    printfn "deal function: %A" res
    res

let getcardprint =
    let player1 = fst dealprint
    let res = Wargame.Game.getCard player1
    printfn "getcard: %A" res
    res

let addcardsprint =
    let board = [1;5;7]
    let player1 = fst dealprint
    let res = Wargame.Game.addCards board player1
    printfn "add cards: %A" res 
    res

let gameprint =
    let player1 = fst dealprint
    let player2 = snd dealprint
    let res = Wargame.Game.game [] player1 player2 0
    printfn "game result: %A" res 
    res