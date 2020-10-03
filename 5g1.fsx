//5g1 a
///<summary> transposeArr takes an array and transposes it by switching column and row </summary>
///<param name = "array2d"> This is the array which has to be transposed </param>
///<returns> The function returns the transposed array </returns>
let a = Array2D.init 2 3 (fun x y -> (x,y))
let transposeArr (array2d:'a [,]) : 'a [,] =
    let newArray = Array2D.init (array2d |> Array2D.length2) (array2d |> Array2D.length1) (fun r c -> array2d.[c,r])
    newArray
printfn "array before transposing: %A" a
printfn "array after transposing: %A" (transposeArr(a))

//5g1c
(*
Advantages with 5g1a
der er indeks som gør det lettere at skære de rigtige elementer ud.
I lister har hver element i lsiten kun vidne om hvad der er før og efter. der er ikke indeks
Det vil sige for at fx finde længden på en liste skal man loope igennem hele listen.
Ved arrrays skal man kende størrelsen og helst kun bruge en type i sin array
mens lister er gode til forskellige typer i en liste

Advantages with 5g0d
Du kan lave mere komplicieret strukturer, her kan man teknisk set lave en dynamisk liste.
En liste er dog immutable, derfor er det sværere at lave om i listen og den elementer/rækkefølge
Arrays er derimod mutable og elementer kan dirkete laves om.
*)
//5g1d
(*
imperative programmering - arrays: Vi kan ikke lave arrays i en funktion, emn skal istedet defininere dem udenfor
Det gør at vores arrays-funktion ikke er stateless, den har brug for den angivet funktion for at bruge den næste.
Laver mest manipulatiopn af strukturer. 

funktionelt programmering - liste: har kan hver funktion bruges i sig selv. De kan bruges i hver rækkefølge de har lyst
funktionernes rækkefølge er ikke vigtig som det er ved imperative programmering. Bruges mest til at lave funktioner
som er data kollektioner.
*)