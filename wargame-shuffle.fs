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
printf "%A" a
printf "%A" b
printf "%A" c

//randomfunktion - makes random list 
let randlist (n:int) =
    let rnd = System.Random()
    List.init n (fun _ -> rnd.Next (2,15))

let deck1 = randlist 10
printfn "%A" deck1

//shuffle funktion, uses perfect shuffle (see faro shuffle)
let shuffle (de:deck) : deck =
    let divide = (List.length de) / 2
    let a = List.mapi (fun x y -> (if x < divide then x * 2 else ((x-divide)*2)+1), y) de
    let b = List.sortBy fst a
    List.map snd b

let f = shuffle(deck1)

let rand : int -> int = let rnd = System . Random ()
                        in fun n -> rnd . Next (0,n)


//newdeck - skal lave et dæk af 4 serier med 2-14 tal (et reelt kortdæk)
let dec = [2;3;4;5;6;7;8;9;10;11;12;13;14]
let newdeck() :deck = 
    let dec2 = shuffle(dec)
    let dec3 = List.append dec dec  
    let sh = shuffle(dec3)
    let sh1 = List.append sh sh 
    let sh2 = shuffle(sh1)
    sh2 

newdeck()
(*
let a = newdeck()
let newdeck() : deck =
    let a = []:int list
    a :: rand(15)  
    a 

newdeck()
*)