(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Engine
open Gfx
open System.Text
open  System.Threading

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall


[< NoEquality; NoComparison >]
type state = {
    player : sprite
    lab : sprite
    arrived : sprite
}
let rng = new System.Random()
type Cell = | Muro | Passaggio
type Maze = { 
    Grid : Cell[,]
    Width : int
    Height : int
}

let W = 51
let H = 51
let finex = W*2-4
let finey = H-2
let engine = new engine (2*W, H)
//inizializza tutta la matrice MAZE a "muro"
let initMaze dx dy = 
    { 
        Grid = Array2D.init dx dy (fun _ _ -> Muro) 
        Width = dx
        Height = dy
    }

//genera MAZE
let generate (maze : Maze) : Maze =
    let isLegal (x,y) =
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    
    let frontier (x,y) =
        [x-2,y; x+2,y; x,y-2; x,y+2] |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Muro)

    
    let neighbor (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2] |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Passaggio)
    
    let randomCell () = rng.Next(maze.Width),rng.Next(maze.Height)

    let removeAt index (lst : (int * int) list) : (int * int) list =
        let x,y = lst.[index]
        lst |> List.filter (fun (a,b) -> not (a = x && b = y) )
    
    let between p1 p2 =
        let x = 
            match (fst p2 - fst p1) with
            | 0 -> fst p1
            | 2 -> 1 + fst p1
            | -2 -> -1 + fst p1
            | _ -> failwith "Invalid arguments for between()"
        let y = 
            match (snd p2 - snd p1) with
            | 0 -> snd p1
            | 2 -> 1 + snd p1
            | -2 -> -1 + snd p1
            | _ -> failwith "Invalid arguments for between()"
        (x,y)
    
    let connectRandomNeighbor (x,y) =
        let neighbors = neighbor (x,y)
        let pickedIndex = rng.Next(neighbors.Length)
        let xn,yn = neighbors.[pickedIndex]
        let xb,yb = between (x,y) (xn,yn)
        maze.Grid.[xb,yb] <- Passaggio
        ()
    
    let rec extend front =
        match front with
        | [] -> ()
        | _ ->
            let pickedIndex = rng.Next(front.Length)
            let xf,yf = front.[pickedIndex]
            maze.Grid.[xf,yf] <- Passaggio
            connectRandomNeighbor (xf,yf)
            extend ((front |> removeAt pickedIndex) @ frontier (xf,yf))

    let rec test (x,y) =
        let maxx = maze.Width
        let maxy = maze.Height
        match x,y with
        | 0,_ -> test (randomCell())
        | _,0 -> test (randomCell())
        | x,y -> if x%2=0 || y%2=0 || x=maxx || y=maxy then test (randomCell()) else x,y

    let x,y = test (randomCell())
    maze.Grid.[x,y] <- Passaggio
    extend (frontier (x,y))
    
    maze

let mazing = generate(initMaze W H)


//gestione uscita "vittoria"
let exit (st:state) = 
    let rect= image.rectangle (11, 5, pixel.filled Color.Yellow, pixel.filled Color.Blue)
    rect.draw_text("Vinto",3, 2, Color.Red, Color.Yellow)
    ignore <| engine.create_and_register_sprite (rect, W-5, H/2, 5)
    

//gestione movimenti modalita' interattiva
let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
    let isWall (x,y) =
        if mazing.Grid.[int (st.player.x / 2. + x), int (st.player.y + y)] = Passaggio then 2.*x,y else 0.,0.
    // move player
    let dx, dy =
        match key.KeyChar with 
        | 'w' -> isWall(0.,-1.)
        | 's' -> isWall(0., 1.)
        | 'a' -> isWall(-1., 0.)
        | 'd' -> isWall(1., 0.)
        | _   -> 0., 0.
    
    let pixpercorso = pixel.create(Config.wall_pixel_char, Color.Blue)
    ignore <| engine.create_and_register_sprite (image.rectangle (2, 1, pixpercorso), int (st.player.x), int (st.player.y), 2)
    Log.msg  "(%A, %A)" (st.player.x) (st.player.y)

    st.player.move_by (dx, dy)
    //controllo se è arrivato
    if st.player.x = float finex && st.player.y = float finey then 
        st.player.clear
        st.arrived.clear
        exit st
        st, true
    else
        st, key.KeyChar = 'q'

//   
type Direction = | LEFT | RIGHT | UP | DOWN | NULL


//gestione automatizzata ricerca percorso
let trymove (stat:state) (direction:Direction) (screen : wronly_raster) =
    let isWall (x,y) =
        if mazing.Grid.[int (stat.player.x / 2. + x), int (stat.player.y + y)] = Passaggio then 2.*x,y else 0.,0.

    let dx= stat.player.x
    let dy= stat.player.y
    //sfrutto la my_update della versione interattiva per la gestione dei movimenti
    let ret =
            match direction with
            | Direction.LEFT ->  isWall(-1., 0.)
            | Direction.RIGHT ->  isWall(1., 0.)
            | Direction.UP ->  isWall(0.,-1.)
            | Direction.DOWN ->  isWall(0., 1.)
            | Direction.NULL -> 0.,0.
    ret

let move (stat:state) (direction:Direction) (screen : wronly_raster) =
    let dx= stat.player.x
    let dy= stat.player.y
    //sfrutto la my_update della versione interattiva per la gestione dei movimenti
    let st, ret =
            match direction with
            | Direction.LEFT ->  my_update (new ConsoleKeyInfo ('a', new ConsoleKey(),false, false, false )) screen stat
            | Direction.RIGHT ->  my_update (new ConsoleKeyInfo('d', new ConsoleKey(),false, false, false )) screen stat
            | Direction.UP ->  my_update (new ConsoleKeyInfo('w', new ConsoleKey(),false, false, false )) screen stat
            | Direction.DOWN ->  my_update (new ConsoleKeyInfo('s', new ConsoleKey(),false, false, false )) screen stat
            | Direction.NULL -> (stat, false)
    if stat.player.x = dx && stat.player.y = dy then
        false
    else 
        true

type Visit = Visited | NotVisited
let mutable Vis = Array2D.init W H (fun _ _ -> NotVisited)

let rec research (st:state) screen (dx,dy) =
    //ora devo gestire il "tornare indietro"
    let dxd, dyd = trymove st Direction.DOWN screen
    if (dxd,dyd)<>(0.,0.) && (Vis.[(int st.player.x+int dxd)/2,int st.player.y+ int dyd]<>Visited) then
        Vis.[int st.player.x/2,int st.player.y] <- Visited
        if not(move st Direction.DOWN screen) then
            failwith "Error"
        Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
        research st screen (dxd,dyd)

    let dxr, dyr = trymove st Direction.RIGHT screen
    if (dxr,dyr)<>(0.,0.) && (Vis.[(int st.player.x + int dxr)/2,int st.player.y + int dyr]<>Visited) then
        Vis.[int st.player.x/2,int st.player.y] <- Visited
        if not(move st Direction.RIGHT screen) then
            failwith "Error"
        Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
        research st screen (dxr,dyr)

    let dxl, dyl = trymove st Direction.LEFT screen
    if (dxl,dyl)<>(0.,0.) && (Vis.[(int st.player.x + int dxl)/2,int st.player.y + int dyl]<>Visited) then
        Vis.[int st.player.x/2,int st.player.y] <- Visited
        if not(move st Direction.LEFT screen) then
            failwith "Error"
        Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
        research st screen (dxl,dyl)

    let dxu, dyu = trymove st Direction.UP screen
    if (dxu,dyu)<>(0.,0.) && (Vis.[(int st.player.x+ int dxu)/2 ,int st.player.y + int dyu]<>Visited) then
        Vis.[(int st.player.x)/2,int st.player.y] <- Visited
        if not(move st Direction.UP screen) then
            failwith "Error"
        Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
        research st screen (dxu,dyu)

    st.player.move_by(-dx,-dy)

let startResolver st screen =
    research st screen (0.,0.)


let auto_start (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) : (state*bool)= 
    startResolver st screen
    st, false

let main (gm: Config.GameMod) =
    //stampaggio maze sfruttando il motore
    let maz (grid: Maze): pixel[] = 
        //creo un'array contenente tutti i pixel della matrice
        let pixelarray = Array.zeroCreate ((grid.Height)*(grid.Width)*2) 
        grid.Grid |> Array2D.iteri 
            (fun y x cell ->
                let c = 
                    match cell with
                    | Muro -> pixel.wall
                    | Passaggio -> pixel.path
                if x<>W || y<>H then 
                    let pos = x*W+y
                    pixelarray.[2*pos] <- c
                    pixelarray.[2*pos+1] <- c
            )
        pixelarray
    // create simple backgroud and player
  
    //ignore <| 
    let labirinto = engine.create_and_register_sprite (new image (W*2,H,(maz mazing)), 0, 0, 0)
    let pixGiocatore = pixel.create(Config.wall_pixel_char, Color.Red)
    let pixArrivo = pixel.create(Config.wall_pixel_char, Color.Blue)
    let giocatore = engine.create_and_register_sprite (image.rectangle (2, 1, pixGiocatore), 2, 1, 2)
    let arrivo = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo), finex, finey, 2)
    //let player = engine.create_and_register_sprite (image.circle (2, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)
    // initialize state
    let st0 = { 
        player = giocatore
        lab = labirinto
        arrived = arrivo
    }

    //start engine
    if gm = Config.GameMod.Auto 
    then
        engine.loop_on_key auto_start st0 
    else
        engine.loop_on_key my_update st0
