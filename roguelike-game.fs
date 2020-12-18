module Canvas

type Color = System.ConsoleColor

type Canvas(dims:int*int) =
    let (rows, cols) = dims
    let field = Array2D.create rows cols ('.', Color.White, Color.Black)
    member this.Set(cx:int, cy:int, cc:char, cfg:Color, cbg:Color) =
        field.[cx,cy] <- (cc,cfg,cbg)
    member this.Show() = 
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
    member val x = xInit with get,set
    member val y = yInit with get,set
    member val c = cInit with get
    member val fg = fgInit with get
    member val bg = bgInit with get
    abstract member RenderOn : Canvas -> unit
    default this.RenderOn (canvas:Canvas) = 
        canvas.Set(this.x,this.y,this.c,this.fg,this.bg)

type Player(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    let mutable hp = 10
    member this.HitPoints = hp
    member this.IsDead = this.HitPoints <= 0
    member this.Damage(dmg:int) = hp <- (hp - dmg)
    member this.Heal(h:int) = hp <- (hp + h)
    member this.MoveTo(dx:int,dy:int) =
        this.x <- this.x + dx
        this.y <- this.y + dy


[<AbstractClass>]
type Item(xInit, yInit, cInit, fgInit, bgInit) =
    inherit Entity(xInit, yInit, cInit, fgInit, bgInit)
    abstract member InteractWith : Player -> unit
    member val FullyOccupy : bool = false

(*
type Water =
    inherit Item
*)


type Wall(xInit, yInit) = 
    inherit Item(xInit, yInit, ' ',Color.White,Color.White)
    override this.InteractWith(_) = ()
    member this.FullyOccupy = true


(*
type Fire (posInit, colorInit)=
    inherit Item
    let mutable fireLeft = 5
    member this.FullyOccupy = true
    override this.InteractWith (p:Player) =
        p.Damage 2
        fireLeft <- fireLeft - 1
*)

(*
type FleshEatingPlant = 
    inherit Item 
type Pot = 
    inherit Item
*)

type World(w:int, h:int) =
    let mutable level = Canvas(w,h)
    member this.AddItem (item:Item) =
        level.Set(item.x,item.y,item.c,item.fg,item.bg)
    member this.CreateRoom(w:int, h:int) =
        let mutable wallList = []
        for i=0 to h do
            wallList <- Wall(i,0) :: wallList
            wallList <- Wall(i, h) :: wallList
        for j=0 to w do
            wallList <- Wall(0, j) :: wallList
            wallList <- Wall(w, j) :: wallList
        wallList
    member this.Play() =
        level.Show()

let game = World(20,20)
let myList = game.CreateRoom(10,10)
for elm in myList do game.AddItem(elm)
game.Play()
        
printfn "Press any key to reset console..."
let x = System.Console.ReadKey()
System.Console.ResetColor()