module Roguelike

type Color = System.ConsoleColor
type Coordinate = int*int
type Tile = char*Color*Color

let mutable msg = ""

type Canvas(dims:int*int) = // Canvas renders the map
    let (rows, cols) = dims
    let mutable field = Array2D.create rows cols ('.', Color.White, Color.Black)
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
                match c with
                | x when x <> '.' ->
                    System.Console.ForegroundColor <- fg
                    System.Console.BackgroundColor <- bg
                | _ -> 
                    System.Console.ForegroundColor <- Color.White
                    System.Console.BackgroundColor <- Color.Black
                printf "%c" c
            printf "\n"
    member this.ResetField() =
        field <- Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.SetMessage(s:string) = msg <- s
    member this.DisplayMessage() = printfn "%s" msg
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
    member this.pos = (this.x,this.y)
    member this.HitPoints = hp
    member this.IsDead = hp <= 0
    member this.Damage(dmg:int) = 
        msg <- msg + "Lost " + string dmg + " hp\n"
        hp <- (hp - dmg)
    member this.Heal(h:int) = 
        msg <- msg + "Healed " + string h + " hp\n"
        hp <- (hp + h)
    member this.MoveTo(dx:int,dy:int) = //direction-x, direction-y
        this.x <- dx // -1 to move left, 1 to move right
        this.y <- dy // -1 to move up, 1 to move down
    override this.RenderOn (canvas:Canvas) =
        canvas.SetOnOccupied(this.x,this.y,this.c,this.fg)

[<AbstractClass>]
type Item(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    abstract member InteractWith : Player -> unit
    abstract member FullyOccupy : bool
    member val pos = (xInit,yInit)
    member val Flag : bool = false with get,set
        
type Water(xInit, yInit) =
    inherit Item(xInit, yInit, '~', Color.White, Color.Blue)
    member this.pos = (this.x,this.y)
    override this.InteractWith(player:Player) =
        msg <- msg + "You stepped on water\n"
        player.Heal(2)
    override this.FullyOccupy = false

type Wall(xInit, yInit) =
    inherit Item(xInit, yInit, '|',Color.White,Color.White)
    member this.pos = (this.x,this.y)
    override this.InteractWith(_) = msg <- "You hit a wall\n"
    override this.FullyOccupy = true

type Fire (xInit, yInit)=
    inherit Item(xInit, yInit, '^',Color.White,Color.Red)
    let mutable fireLeft = 5
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You stepped on fire\n"
        p.Damage(1)
        fireLeft <- fireLeft - 1
        if fireLeft <= 0 then
            this.Flag <- true 
        else this.Flag <- false
    override this.FullyOccupy = false   

type FleshEatingPlant (xInit, yInit) =
    inherit Item (xInit, yInit, 'F', Color.White, Color.Green)
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You hit a plant\n"
        p.Damage(5)
    override this.FullyOccupy = true

type Lava (xInit,yInit) =
    inherit Item(xInit,yInit,'Ø', Color.Green, Color.Black)
    member this.pos = (this.x,this.y)
    override this.InteractWith (p:Player) =
        msg <- msg + "You stepped on lava\n"
        p.Damage(10) //Lava kills (or almost kills) player - very dangerous
    override this.FullyOccupy = false

(*
type Exit (xInit, yInit) =
    inherit Item(xInit,yInit,'D', Color.White, Color.Green)
    member this.FullyOccupy = true
    override this.InteractWith (p:Player) =
        if p.HitPoints >= 5 then
            this.Flag = true 
        else
            this.Flag = false
    //change to true if player has 5 health
*)

type Pot (xInit, yInit)=
    inherit Item (xInit, yInit, 'U', Color.White, Color.DarkMagenta)
    let mutable potHealth = 1
    override this.InteractWith (p:Player) =
        p.Heal(2)
        potHealth <- potHealth - 1
        if potHealth <= 0 then
            this.Flag <- true
        else 
            this.Flag <- false  
    override this.FullyOccupy = true


(*let io (p:Player) =
    //printfn "Enter a command: "
    match System.Console.ReadKey().Key with
    | System.ConsoleKey.UpArrow -> (-1,0)
    | System.ConsoleKey.DownArrow -> (1,0)
    | System.ConsoleKey.LeftArrow -> (0,-1)
    | System.ConsoleKey.RightArrow -> (0,1)
    | System.ConsoleKey.Q -> p.Damage(10)
    | _ -> p.MoveTo(0,0)
*)
//type State = (Item option)

type World(h:int, w:int) =
    let mutable _level = Canvas(h,w)
    //let mutable itemArray = Array2D.create h w ((None) : State)
    let mutable _itemList = []
    let mutable exit:bool = false
    let player = Player(2,2)
    member this.Exit = exit
    member this.GetExit() = this.Exit
    member this.AddItem (item:Item) = 
        _itemList <- item :: _itemList
    member this.BufferCanvas (canvas:Canvas) =
        _itemList <- List.filter (fun (x:Item) -> not x.Flag) _itemList
        for item in _itemList do
            item.RenderOn(canvas)
    member this.DoInteractWith(dpos:int*int) =
        let dx,dy = dpos
        let collideList : list<Item> = List.filter (fun (x:Item) -> x.pos = dpos) _itemList
        if List.exists (fun (x:Item) -> x.FullyOccupy = true) collideList then player.MoveTo(player.x,player.y)
        else player.MoveTo(dx,dy)
        for item in collideList do
            item.InteractWith(player)

    member this.Play() =
        this.AddItem(Wall(2,1))
        this.AddItem(Fire(3,1))
        //this.SetWall(myTestWall, (myTestWall.x,myTestWall.y))
        //let y = Option.get(itemArray.[0,0])
        //y.RenderOn(level)
        while not player.IsDead do
            _level.ResetField()
            this.BufferCanvas (_level)
            player.RenderOn(_level)
            _level.Show()
            _level.DisplayMessage()
            _level.SetMessage("")
            match System.Console.ReadKey().Key with
            | System.ConsoleKey.UpArrow -> this.DoInteractWith((player.x-1),(player.y))
            | System.ConsoleKey.DownArrow -> this.DoInteractWith(player.x+1,player.y)
            | System.ConsoleKey.LeftArrow -> this.DoInteractWith(player.x,player.y-1)
            | System.ConsoleKey.RightArrow -> this.DoInteractWith(player.x,player.y+1)
            | System.ConsoleKey.Q -> player.Damage(10)
            | _ -> player.MoveTo(0,0)
        System.Console.ReadKey()
        System.Console.ResetColor()

    (*
    member this.SetWall (w : Wall, coords : (int*int)) =
        (fun (i,j) -> itemArray.[i,j] <- (Some (w :> Item))) coords
    
    member this.CreateRoom(w:int, h:int) = //add startPost(x,y)
        let mutable wallList = []
        for i=0 to h do
            wallList <- Wall(i,0) :: wallList
            wallList <- Wall(i, h) :: wallList
        for j=0 to w do
            wallList <- Wall(0, j) :: wallList
            wallList <- Wall(w, j) :: wallList
        wallList
    
    // BufferCanvas
    member this.BufferCanvas (c:Canvas) =
        for i=0 to h-1 do
            for j=0 to w-1 do
                match itemArray.[i,j] with
                    | Some w when w.Flag = true -> 
                        itemArray.[i,j] <- None
                        level.Set(i,j,'.',Color.White,Color.Black)
                    | Some w -> w.RenderOn(level)
                    | _ -> level.Set(i,j,'.',Color.White,Color.Black)
    *)

//for wall in wallList do
