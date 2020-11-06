namespace Wargame

type card = int
type deck = card list 

//deal-function
///<summary> takes a deck and divides it into two new decks one int at a time.</summary>
///<param name="d"> a deck type, which is a card list. </param> 
/// <returns> The function returns a tuple which contains two stacks. </returns>
let deal (d: deck) : deck*deck =
    let rec stack dl (xs, ys) : deck*deck =
        match dl with
        | x::y::rest -> stack rest (x::xs, y::ys)
        | [x] -> (x::xs, ys)
        | _ -> (xs, ys)
    stack d ([], [])

let rand : int -> int = let rnd = System.Random()
                        in fun n -> rnd.Next (0,n)

//shuffle function
///<summary> shuffles the deck by assigning random indices and sorting them. </summary>
///<param name="de"> a deck, which is a card list. </param>
///<returns> Returns the now shuffles deck, without the assigned indices. </returns>
let shuffle (de:deck) : deck =
    let a = List.mapi (fun i y -> rand(52), y) de
    let b = List.sortBy fst a
    List.map snd b

//newdeck()
///<summary> a unit type that makes a random shuffled card deck containing 52 cards. </summary>
///<returns> returns a shuffled card deck. </returns>
let newdeck() :deck = 
    let dec = [2;3;4;5;6;7;8;9;10;11;12;13;14]
    let deckappend1 = dec @ dec @ dec @ dec
    let shuffle3 = shuffle(deckappend1)
    shuffle3 