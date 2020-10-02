
let isTable (llst: 'a list list) : bool =
    if List.length llst < 2 then false
    else
        if List.length llst.[0] = 0 || List.length llst.[1] = 0 then false 
        else   
            if List.length llst.[0] = List.length llst.[1] then true
            else false

let trying = [[];[]]
printfn "%A" (List.length trying)
printfn "%A" (isTable(trying))

(*
let isTable (llst: 'a list) : bool =
    if List.isEmpty llst then false
    else true 
 
let trying = []
printfn "%A" (isTable(trying))
*)

