module Wargame_shuffle

//sørg for libary virker 
type card = int
type deck = card list 

//deal-funktion
let deal (d: deck) : deck*deck =
    let rec stack dl (xs, ys) =
        match dl with
        | x::y::rest -> stack rest (x::xs, y::ys)
        | [x] -> (x::xs, ys)
        | _ -> (xs, ys)
    stack d ([], [])

let a = deal([4;9;2;5;3])
let b = deal([])
let c = deal([2])

let rand : int -> int = let rnd = System.Random ()
                        in fun n -> rnd.Next (0,n)
//shuffle funktion
let shuffle (de:deck) : deck =
    let a = List.mapi (fun x y -> rand(52), y) de
    let b = List.sortBy fst a
    List.map snd b


//newdeck - skal lave et dæk af 4 serier med 2-14 tal (et reelt kortdæk)
let newdeck() :deck = 
    let dec = [2;3;4;5;6;7;8;9;10;11;12;13;14]
    let deckappend1 = dec @ dec @ dec @ dec
    let shuffle3 = shuffle(deckappend1)
    shuffle3 

newdeck()