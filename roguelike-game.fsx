open Roguelike
#r "roguelike-game.dll"

let game = Roguelike.World(20,20)
while not (game.GetExit()) do
    game.Play() 
    printfn "Enter a command"
printfn "Press any key to reset console..."
let x = System.Console.ReadKey()
System.Console.ResetColor()