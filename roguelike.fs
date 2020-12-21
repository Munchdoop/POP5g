module Roguelike

type Color = System.ConsoleColor

type Canvas(dims:int*int) =
    let (rows, cols) = dims
    let field = Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.Set(cx:int, cy:int, cc:char, cfg:Color, cbg:Color) =
        field.[cx,cy] <- (cc,cfg,cbg)
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
    member this.MoveTo(dx:int,dy:int) =
        this.x <- this.x + dx
        this.y <- this.y + dy

[<AbstractClass>]
type Item(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    abstract member InteractWith : Player -> unit
    member this.FullyOccupy : bool = false

type Water(xInit, yInit) =
    inherit Item(xInit, yInit, ' ', Color.White, Color.Blue)
    override this.InteractWith(player:Player) = player.Heal(2)
    member this.FullyOccupy = false

type Wall(xInit, yInit) = 
    inherit Item(xInit, yInit, ' ',Color.White,Color.White)
    override this.InteractWith(_) = ()
    member this.FullyOccupy = true

type Fire (xInit, yInit)=
    inherit Item(xInit, yInit, ' ',Color.White,Color.Red)
    let mutable fireLeft = 5
    override this.InteractWith (p:Player) =
        p.Damage 2
        fireLeft <- fireLeft - 1
    member this.FullyOccupy = false

type FleshEatingPlant (xInit, yInit) = 
    inherit Item (xInit, yInit, 'F', Color.White, Color.Green)
    override this.InteractWith (p:Player) = p.Damage 5
    member this.FullyOccupy = false

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
    | System.ConsoleKey.UpArrow -> p.MoveTo(-1,0)
    | System.ConsoleKey.DownArrow -> p.MoveTo(1,0)
    | System.ConsoleKey.LeftArrow -> p.MoveTo(0,-1)
    | System.ConsoleKey.RightArrow -> p.MoveTo(0,1)
    | System.ConsoleKey.Q -> p.Damage(10)
    | _ -> p.MoveTo(0,0)

type World(w:int, h:int) =
    let mutable level = Canvas(w,h)
    let mutable exit:bool = false
    let player = Player(2,2)
    member this.Exit = exit
    member this.GetExit() = this.Exit
    member this.AddItem (item:Item) =
        level.Set(item.x,item.y,item.c,item.fg,item.bg)
    (*member this.CreateRoom(w:int, h:int) =
        let mutable wallList = []
        for i=0 to h do
            wallList <- Wall(i,0) :: wallList
            wallList <- Wall(i, h) :: wallList
        for j=0 to w do
            wallList <- Wall(0, j) :: wallList
            wallList <- Wall(w, j) :: wallList
        wallList*)
    member this.Play() = 
        level.Show()
        player.RenderOn(level)
        while not player.IsDead do
            io player
        printfn "Press any key to reset console..."
        let x = System.Console.ReadKey()
        System.Console.ResetColor()