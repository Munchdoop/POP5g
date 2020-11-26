namespace Wargame

type card = int
type deck = card list 

module Shuffle =
    //deal-function
    /// <summary>Takes a <typeparamref name="deck"/> and divides it into two new lists,
    /// alternating between one <typeparamref name="card"/> at a time per <typeparamref name="deck"/>.</summary>
    /// <param name="d"> a <typeparamref name="deck"/>, which is a <typeparamref name="card list"/>.</param> 
    /// <returns>A tuple which contains two separate <typeparamref name="deck"/> instances.</returns>
    let deal (d: deck) : deck*deck =
        let rec stack dl (xs, ys) : deck*deck =
            match dl with
            | x::y::rest -> stack rest (x::xs, y::ys)
            | [x] -> (x::xs, ys)
            | _ -> (xs, ys)
        stack d ([], [])
    
    //random function
    /// <summary>Generates a random integer between 0 and <paramref name="n"/>.</summary>
    /// <param name="n"> an <c>int</c>.</param>
    /// <returns>A random integer between 0 and <paramref name="n"/>.</returns>
    let rand : int -> int = let rnd = System.Random()
                            in fun n -> rnd.Next (0,n)

    //shuffle function
    /// <summary>Shuffles an input <typeparamref name="deck"/> by assigning random indices and sorting through them.</summary>
    /// <param name="de"> a <typeparamref name="deck"/>, which is a <typeparamref name="card list"/>.</param> 
    /// <returns> Returns the now shuffled <paramref name="de"/> (without the assigned indices). </returns>
    let shuffle (de:deck) : deck =
        let a = List.mapi (fun i y -> rand(52), y) de
        let b = List.sortBy fst a
        List.map snd b

    //newdeck()
    /// <summary>Creates a random shuffled <typeparamref name="deck"/> containing 52 cards.</summary>
    /// <returns>A shuffled <typeparamref name="deck"/>.</returns>
    let newdeck() :deck = 
        let dec = [2;3;4;5;6;7;8;9;10;11;12;13;14]
        let deckappend1 = dec @ dec @ dec @ dec
        let shuffle3 = shuffle(deckappend1)
        shuffle3 