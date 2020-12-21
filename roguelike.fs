module Roguelike

type Color = System.ConsoleColor
type Tile = char*Color*Color

type Canvas(dims:int*int) = // Canvas renders the map
    let (rows, cols) = dims
    let field = Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.Set(cx:int, cy:int, cc:char, cfg:Color, cbg:Color) =
        field.[cx,cy] <- (cc,cfg,cbg)
    member this.SetOnOccupied(cx:int, cy:int, cc:char, cfg:Color) =
        let (_,_,obg) = field.[cx,cy]
        in this.Set(cx,cy,cc,cfg,obg)
    member this.Show () =
        System.Console.Clear()
        System.Console.ResetColor()
        for i = 0 to rows-1 do
            for j = 0 to cols-1 do
                let (c, fg, bg) = field.[i,j]
                System.Console.ForegroundColor <- fg
                System.Console.BackgroundColor <- bg
                printf "%c" c
            printf "\n"
     member this.Coord (x:int,y:int) = field.[x,y]

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

type Player(xInit, yInit) =
    inherit Entity(xInit, yInit, '@', Color.Yellow, Color.Black)
    let mutable hp = 10
    member this.HitPoints = hp
    member this.IsDead = hp <= 0
    member this.Damage(dmg:int) = hp <- (hp - dmg)
    member this.Heal(h:int) = hp <- (hp + h)
    member this.MoveTo(dx:int,dy:int) = //direction-x, direction-y
        //if
        //this.RenderOn (this.x,this.y,".",Color.White, Color.Black)
        this.x <- this.x + dx // -1 to move left, 1 to move right
        this.y <- this.y + dy // -1 to move up, 1 to move down
    override this.RenderOn (canvas:Canvas) =
        canvas.SetOnOccupied(this.x,this.y,this.c,this.fg)


[<AbstractClass>]
type Item(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    abstract member InteractWith : Player -> unit

    member this.FullyOccupy : bool = false

type Water(xInit, yInit) =
    inherit Item(xInit, yInit, '~', Color.White, Color.Blue)
    override this.InteractWith(player:Player) = player.Heal(2)
    member this.FullyOccupy = false

type Wall(xInit, yInit) =
    inherit Item(xInit, yInit, '|',Color.White,Color.White)
    override this.InteractWith(_) = ()
    member this.FullyOccupy = true

type Fire (xInit, yInit)=
    inherit Item(xInit, yInit, '^',Color.White,Color.Red)
    let mutable fireLeft = 5
    override this.InteractWith (p:Player) =
        p.Damage(1)
        fireLeft <- fireLeft - 1
    member this.FullyOccupy = false
    //fire needs to be deleted after 5 touches

type FleshEatingPlant (xInit, yInit) =
    inherit Item (xInit, yInit, 'F', Color.White, Color.Green)
    override this.InteractWith (p:Player) = p.Damage(5)
    member this.FullyOccupy = true
(*
type Monster(xInit,yInit) =
    inherit Item(xInit,yInit,'Ø', Color.Green, Color.Black)
    let mutable hp = 5
    member this.HP = hp
    member this.Killed = hp <= 0
    //if killed then removed
    override this.InteractWith (p:Player) =
        p.Damage(2)
        this.Damage(2) //monster takes damage also
    member this.FullyOccupy = false

type Exit (xInit, yInit) =
    inherit Item(xInit,yInit,'D', Color.White, Color.Green)
    /member this.FullyOccupy =
        override this.InteractWith (p:Player) =
        if p.HitPoints >= 5 then
            false
        else
            true
    //change to true if player has 5 health

*)

type Pot (xInit, yInit)=
    inherit Item (xInit, yInit, 'U', Color.White, Color.DarkMagenta)
    let mutable potHealth = 1
    override this.InteractWith (p:Player) =
        p.Heal(2)
        potHealth <- potHealth - 1
        //pot needs to be deleted after one use




let io (p:Player) =
    //printfn "Enter a command: "
    match System.Console.ReadKey().Key with
    | System.ConsoleKey.UpArrow ->
        //printfn "Moving player up"
        p.MoveTo(-1,0)
    | System.ConsoleKey.DownArrow -> p.MoveTo(1,0)
    | System.ConsoleKey.LeftArrow -> p.MoveTo(0,-1)
    | System.ConsoleKey.RightArrow -> p.MoveTo(0,1)
    | System.ConsoleKey.Q -> p.Damage(10)
    | _ -> p.MoveTo(0,0)

type World(h:int, w:int) =
    let mutable level = Canvas(h,w)
    //let mutable itemList = Array2D.init h w (fun y x -> Wall(x,y))
    let mutable exit:bool = false
    let player = Player(2,2)
    member this.Exit = exit
    member this.GetExit() = this.Exit
    member this.AddItem (item:Item) =
        level.Set(item.x,item.y,item.c,item.fg,item.bg)
        item.RenderOn(level)
    member this.CreateRoom(w:int, h:int) = //add startPost(x,y)
        let mutable wallList = []
        for i=0 to h do
            wallList <- Wall(i,0) :: wallList
            wallList <- Wall(i, h) :: wallList
        for j=0 to w do
            wallList <- Wall(0, j) :: wallList
            wallList <- Wall(w, j) :: wallList
        wallList
    member this.Play() =
        printfn "Creating room"
        let x = System.Console.ReadKey()
        let myRoom = this.CreateRoom(5,5)
        printfn "Looping room"
        let x = System.Console.ReadKey()
        for elm in myRoom do
            this.AddItem (elm)
        printfn "Main loop"
        while not player.IsDead do
            player.RenderOn(level)
        let x = System.Console.ReadKey()
        System.Console.ResetColor()