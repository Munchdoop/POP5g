namespace Roguelike
//open RoguelikeLib
//#r "roguelike-game.dll"

module Game =

    let game = Library.World(20,20)

    //let myList = game.CreateRoom(10,10)
    //for elm in myList do game.AddItem(elm)
    game.Play()

    printfn "Press any key to reset console..."
    let x = System.Console.ReadKey()
    System.Console.ResetColor()