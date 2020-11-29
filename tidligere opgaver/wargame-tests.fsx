let shuffleprint =
    let res =  Wargame.Shuffle.newdeck()
    printfn "Shuffle and newdeck: %A" res

let deck1 = Wargame.Shuffle.newdeck()
let dealprint = 
    let res = Wargame.Shuffle.deal(deck1)
    printfn "deal function: %A" res

(*
let deal1 = Wargame.Shuffle.deal(deck1)
let getcardprint =
    let res = Wargame.Game.getCard deal1
    printfn "getcard: %A" res

let addcardsprint =
    let board = [1;5;7]
    let res = Wargame.Game.addCards board deal1
    printfn "add cards: %A" res *)