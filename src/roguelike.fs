module Roguelike

type Color = System.ConsoleColor

///<summary>A global message string which is updated by other objects</summary>
let mutable msg = ""

///<summary>Canvas displays a graphical representation of the game world.</summary>
///<typeparam name="rows">The number of rows to be displayed.</param>
///<typeparam name="cols">The number of columns to be displayed.</param>
///<typeparam name="field">An internal field which represents the canvas elements as a 2d array.</typeparam>
///<typeparam name="Set">A class method which takes an x and y value and sets the character, foreground, and background colour at that position in the inernal field array.</typeparam>
///<typeparam name="Show">A class method which refreshes the canvas and displays the internal field aray graphically.</typeparam>
///<typeparam name="ResetField">A class method which resets all values of the internal field array to the default state.</typeparam>
///<typeparam name="AppendMessage">A class method which appends a string to the global msg string.</typeparam>
///<typeparam name="ResetMessage">A class method which resets the global msg string to the default empty state.</typeparam>
///<typeparam name="DisplayMessage">A class method which displays the current global msg string under the game space.</typeparam>
type Canvas(dims:int*int) = // Canvas renders the map
    let (rows, cols) = dims
    let mutable field = Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.Set(cx:int, cy:int, cc:char, cfg:Color, cbg:Color) =
        field.[cx,cy] <- (cc,cfg,cbg)
    member this.SetOnOccupied(cx:int, cy:int, cc:char, cfg:Color) =
        let (_,_,obg) = field.[cx,cy]
        in this.Set(cx,cy,cc,cfg,obg)
    member this.Show () =
        // Clear the console first
        System.Console.Clear()
        // Loop through field and assign character/foreground/background based on stored values.
        for i = 0 to rows-1 do
            for j = 0 to cols-1 do
                let (c, fg, bg) = field.[i,j]
                match c with
                | x when x <> '.' -> // A non-default field
                    System.Console.ForegroundColor <- fg
                    System.Console.BackgroundColor <- bg
                | _ -> // A default field
                    System.Console.ForegroundColor <- Color.White
                    System.Console.BackgroundColor <- Color.Black
                printf " %c " c
            printf "\n"
        System.Console.ResetColor()
        this.ResetField() // Must always reset field to avoid displaying old/deleted objects
    member this.ResetField() =
        field <- Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.AppendMessage(s:string) = msg <- msg + "\n" + s
    member this.ResetMessage() = msg <- ""
    member this.DisplayMessage() = 
        printfn "%s" msg
        this.ResetMessage()

///<summary>Enitity is abstract class, wich other classes inherits from.</summary>
///<param nanme="_x">Mutable field which sets the x-coordinate when an entity is instantiated.</paramname>
///<param nanme="_y">Mutable field which sets the y-coordinate when an entity is instantiated.</paramname>
///<param nanme="x">Property which holds the x-coordinate of the entity and is equal to _x.</paramname>
///<param nanme="y">Property which holds the y-coordinate of the entity and is equal to _y.</paramname>
///<param name="c">Property which holds the char of the entity and is set by cInit on instantion.</paramname>
///<param name="fg">Property which holds the foreground-color of the entity and is set by fgInit on instantiation.</paramname>
///<param name="bg">Property which holds the background-color of the entity and is set by bgInit on instantiaiton.</paramname>
///<param name="RenderOn">Abstract method which takes a Canvas and is resposible for rendering entity on the canvas.</paramname>

[<AbstractClass>]
type Entity(xInit:int, yInit:int, cInit:char, fgInit:Color, bgInit:Color) =
    let mutable _x = xInit
    let mutable _y = yInit
    member val x = _x with get,set
    member val y = _y with get,set
    member val c = cInit with get
    member val fg = fgInit with get
    member val bg = bgInit with get
    abstract member RenderOn : Canvas -> unit
    default this.RenderOn (canvas:Canvas) =
        canvas.Set(this.x,this.y,this.c,this.fg,this.bg)

///<summary>Player is the class which contains the properties and methods for the playable character.</param>
///<param name="hp">The field health of the player character which starts at 10.</param>
///<param name="pos">The characters current position stored as a tuple.</param>
///<param name="HitPoints">The HitPoints property which is set equal to the field hp</param>
///<param name="Damage">Damage method, which subtracts the amount of damage given to the player.</param>
///<param name="Heal">Heal method, which heals the player by adding hitpoints to the player.</param>
///<param name="MoveTo">Moves the player character by updating the x-/y-coordinates.</param>
///<param name="RenderOn">Renders the player on, by updating the tile with the player character but maintaining the background color.</param>
type Player(xInit, yInit) =
    inherit Entity(xInit, yInit, '@', Color.Yellow, Color.Black)
    let mutable hp = 10
    member this.pos = (this.x,this.y)
    member this.HitPoints = hp
    member this.IsDead = hp <= 0
    member this.Damage(dmg:int) = 
        msg <- msg + "Lost " + string dmg + " hp.\n"
        hp <- (hp - dmg)
    member this.Heal(h:int) = 
        msg <- msg + "Healed " + string h + " hp.\n"
        hp <- (hp + h)
    member this.MoveTo(dx:int,dy:int) = //direction-x, direction-y
        this.x <- dx // -1 to move left, 1 to move right
        this.y <- dy // -1 to move up, 1 to move down
    override this.RenderOn (canvas:Canvas) =
        canvas.SetOnOccupied(this.x,this.y,this.c,this.fg)

///<summary> A class that inherits from Entity and sets some abstract members for Item subclasses </summary>
///<param name="InteractWith"> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
///<param name="pos"> the position of the water as a tuple</param>
///<param name="Flag">a bool that determines if an item is used </param>
[<AbstractClass>]
type Item(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    abstract member InteractWith : Player -> unit
    abstract member FullyOccupy : bool
    member val pos = (xInit,yInit)
    member val Flag : bool = false with get,set

///<summary> A class that defines water, which has healing abilities </summary>
///<param name="pos"> the position of the water as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
type Water(xInit, yInit) =
    inherit Item(xInit, yInit, '~', Color.White, Color.Blue)
    member this.pos = (this.x,this.y)
    override this.InteractWith(player:Player) =
        msg <- msg + "You stepped on water.\n"
        player.Heal(2)
    override this.FullyOccupy = false

///<summary> A class that defines wall, which acts as a block for the player </summary>
///<param name="pos"> the position of the wall as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>   
type Wall(xInit, yInit) =
    inherit Item(xInit, yInit, '|',Color.White,Color.White)
    member this.pos = (this.x,this.y)
    override this.InteractWith(_) = msg <- "You hit a wall.\n"
    override this.FullyOccupy = true

///<summary> A class that defines fire, which has hurting abilities </summary>
///<param name="pos"> the position of the fire as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
type Fire (xInit, yInit)=
    inherit Item(xInit, yInit, '^',Color.DarkYellow,Color.Yellow)
    let mutable fireLeft = 5
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You stepped on fire!\n"
        p.Damage(1)
        fireLeft <- fireLeft - 1
        if fireLeft <= 0 then
            this.Flag <- true 
        else this.Flag <- false
    override this.FullyOccupy = false   

///<summary> A class that defines FleshEatingPlant, which has hurting abilities </summary>
///<param name="pos"> the position of the FleshEatingPlant as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
type FleshEatingPlant (xInit, yInit) =
    inherit Item (xInit, yInit, 'F', Color.White, Color.Green)
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You were bit by a flesh eating plant!\n"
        p.Damage(5)
    override this.FullyOccupy = true

///<summary> A class that defines lava, which has hurting abilities </summary>
///<param name="pos"> the position of the lava as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
type Lava (xInit,yInit) =
    inherit Item(xInit,yInit,'Ø', Color.Red, Color.DarkRed)
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You stepped on lava!!!\n"
        p.Damage(9) //Lava kills (or almost kills) player - very dangerous
    override this.FullyOccupy = false

///<summary> A class that defines Pot, which has healing abilities </summary>
///<param name="pos"> the position of the Pot as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
type Pot (xInit, yInit)=
    inherit Item (xInit, yInit, 'U', Color.White, Color.DarkMagenta)
    let mutable potHealth = 1
    override this.InteractWith (p:Player) =
        p.Heal(2)
        potHealth <- potHealth - 1
        if potHealth <= 0 then
            this.Flag <- true
            msg <- "You broke a pot and feel refreshed."
    override this.FullyOccupy = true

///<summary> A class that defines Exit, which has winning abilities </summary>
///<param name="pos"> the position of the FleshEatingPlant as a tuple</param>
///<param name=InteractWith> What the class does to the player </param>
///<param name="FullyOccupy"> tells is occupies a whole field or not </param>
///<param name="CanExit"> write to world if player has more than 5 HP </param>
///<param name="SetCanExit"> a bool that decides if the player can exit </param>
///<param name="HasExited"> triggers the winscreen </param>
type Exit (xInit, yInit) =
    inherit Item(xInit,yInit,'D', Color.White, Color.Green)
    let mutable _canExit:bool = false
    let mutable _hasExited:bool = false
    override this.FullyOccupy = true
    member this.CanExit = _canExit
    member this.SetCanExit(b:bool) = _canExit <- b
    member this.HasExited = _hasExited
    override this.InteractWith (p:Player) =
        if p.HitPoints >= 5 && not this.CanExit then
            msg <- "You have opened the exit -- go forth and claim victory!"
            this.SetCanExit(true)
        else if this.CanExit then
            msg <- "You exit the dungeon!"
            _hasExited <- true
        else msg <- "You need at least 5 HP to open the exit..."


// Begin hardcoded level lists
let wallList = [(0,0);(1,0);(2,0);(3,0);(4,0);(5,0);(6,0);(7,0);(8,0);(9,0);(10,0);(11,0);
        (0,1);(6,1);(11,1);
        (0,2);(3,2);(4,2);(6,2);(8,2);(9,2);(11,2);
        (0,3);(2,3);(4,3);(8,3);(11,3);
        (0,4);(4,4);(5,4);(6,4);(7,4);(8,4);(10,4);(11,4);
        (0,5);(1,5);(3,5);(4,5);(8,5);(11,5);
        (0,6);(4,6);(5,6);(7,6);(8,6);(9,6);(11,6);
        (0,7);(4,7);(9,7);(11,7);
        (0,8);(4,8);(6,8);(7,8);(9,8);(11,8);
        (0,9);(2,9);(3,9);(7,9);(11,9);
        (0,10);(5,10);(11,10);
        (0,11);(1,11);(2,11);(3,11);(4,11);(5,11);(6,11);(7,11);(8,11);(9,11);(10,11);(11,11)]
let fireList = [(3,1);(4,1);(5,1);(6,6)]
let waterList = [(1,7);(1,8);(2,2);(2,6);(2,7);(3,4);(9,9);(10,9);(10,10)]
let lavaList = [(1,3);(5,8);(7,1);(8,1);(9,1);(10,1)]
let plantList = [(1,6);(2,8);(3,3);(3,6);(3,7);(9,10)]
let potList = [(7,5)]
// End hardcoded level lists

/// <summary>A class that represents the game world, and tracks all objects within it,
/// particularly <typeparamref name="Player"/> and <typeparamref name="Exit"/> which
/// the class monitors to make decisions on the game state.</summary>
/// <typeparam name="PlayerExited">Returns true or false depending on whether the player 
///has exited the dungeon.</typeparam>
/// <typeparam name="AddItem">A method which appends an item to the private field <paramref name="_itemList"/>.</typeparam>
/// <typeparam name="BufferCanvas">A method which deletes items flagged for deletion, and calls
/// the RenderOn() method for each item in <paramref name="_itemList"/>.</typeparam>
/// <typeparam name="DoInteractWith">A method which determines whether a player can move onto a
/// tile, and executes all the relevant InteractWith() methods in each object.</typeparam>
/// <typeparam name="IO">A method which requests user input for further use.</typeparam>
/// <typeparam name="Play">A method which initialises the level, defines the game loop, calls the various 
/// class methods as needed, and closes the program upon game over.</typeparam>
type World() =
    let mutable _level = Canvas(12,12)
    let mutable _itemList = []
    let player = Player(1,1)
    let exit = Exit (5,5)
    member this.PlayerExited = exit.HasExited
    member this.AddItem (item:Item) = // Append an iem to the _itemList
        _itemList <- item :: _itemList
    member this.BufferCanvas (canvas:Canvas) = // Remove deprecated objects and render all remaining objects on a canvas
        _itemList <- List.filter (fun (x:Item) -> not x.Flag) _itemList
        for item in _itemList do
            item.RenderOn(canvas)
    member this.DoInteractWith(dpos:int*int) = // Trigger player/object interactions
        let dx,dy = dpos
        let collideList : list<Item> = List.filter (fun (x:Item) -> x.pos = dpos) _itemList // Populate a list with all the Items to be interacted with
        if List.exists (fun (x:Item) -> x.FullyOccupy = true) collideList then player.MoveTo(player.x,player.y) // Move the player to their current spot
        else player.MoveTo(dx,dy) // Move player to the new position
        for item in collideList do // Trigger all Item interactions
            item.InteractWith(player)
    member this.IO() =
        match System.Console.ReadKey().Key with
        | System.ConsoleKey.UpArrow -> this.DoInteractWith((player.x-1),(player.y))
        | System.ConsoleKey.DownArrow -> this.DoInteractWith(player.x+1,player.y)
        | System.ConsoleKey.LeftArrow -> this.DoInteractWith(player.x,player.y-1)
        | System.ConsoleKey.RightArrow -> this.DoInteractWith(player.x,player.y+1)
        | System.ConsoleKey.Q -> player.Damage(10) // Suicide button for testing purposes
        | _ -> player.MoveTo(player.x,player.y)
    member this.Play() =
        // Initialise hardcoded level
        for elm in wallList do
            this.AddItem (Wall((snd elm),(fst elm)) :> Item)
        for elm in fireList do
            this.AddItem (Fire((snd elm),(fst elm)) :> Item)
        for elm in waterList do
            this.AddItem (Water((snd elm),(fst elm)) :> Item)
        for elm in lavaList do
            this.AddItem (Lava((snd elm),(fst elm)) :> Item)
        for elm in plantList do
            this.AddItem (FleshEatingPlant((snd elm),(fst elm)) :> Item)
        for elm in potList do
            this.AddItem (Pot((snd elm),(fst elm)) :> Item)
        this.AddItem(exit)
        // End initialise
       
        // Main loop
        while not player.IsDead && not this.PlayerExited do
            // Buffer and display graphical elements
            this.BufferCanvas(_level)
            player.RenderOn(_level)
            _level.Show()
            _level.AppendMessage("_____________\n| Health: " + string (player.HitPoints) + " |\n-------------")
            _level.DisplayMessage()
            // Request user input
            this.IO()
        // End main loop
        
        // Display won/lost messages
        if this.PlayerExited then
            _level.AppendMessage("A winner is you!")
        else _level.AppendMessage("The dungeon claims another life.")

        // Closing message
        _level.AppendMessage("Press any key to terminate the program...")
        _level.DisplayMessage()
        System.Console.ReadKey() |> ignore
        System.Console.ResetColor()