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

let stampa = false

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall


[< NoEquality; NoComparison >]
type state = {
    player : sprite
}
let rng = new System.Random()
type Cell = | Muro | Passaggio
type Maze = { 
    Grid : Cell[,]
    Width : int
    Height : int
}

let W = 31
let H = 31


//COPIATO 
//Maze.initMaze 50 50 |> Maze.generate |> Maze.show |> Maze.render
//CONTROLLA CHE IL PIXEL CASUALE NON FINISCA SUL BORDO

let initMaze dx dy = 
    let six,siy = (1,1)
    let eix,eiy = (dx-2,dy-2)
    { 
        Grid = Array2D.init dx dy 
            (fun _ _ -> Muro
            ) 
        Width = dx
        Height = dy
    }

let show maze =
    //printfn "%A" maze   // stampa della struttura dati
    Console.Clear ()
    maze.Grid |> Array2D.iteri //scorrimento della matrice e stampa del labirinto ~ implementare con libreria grafica
        (fun y x cell ->
            if x = 0 && y > 0 then 
                printfn "|"
            let c = 
                match cell with
                | Muro -> Config.wall_pixel_char
                | Passaggio -> Config.empty_pixel_char
            printf "%c" c
        )
    maze

let generate (maze : Maze) : Maze =
    let isLegal (x,y) =
        //show maze |> ignore
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    
    let frontier (x,y) =
        //show maze |> ignore
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Muro)

    
    let neighbor (x,y) =
        //show maze |> ignore
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Passaggio)
    
    let randomCell () = rng.Next(maze.Width),rng.Next(maze.Height)

    let removeAt index (lst : (int * int) list) : (int * int) list =
        //show maze |> ignore
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
        if stampa then show maze |> ignore
        let neighbors = neighbor (x,y)
        let pickedIndex = rng.Next(neighbors.Length)
        let xn,yn = neighbors.[pickedIndex]
        let xb,yb = between (x,y) (xn,yn)
        maze.Grid.[xb,yb] <- Passaggio
        ()
    
    let rec extend front =
        if stampa then show maze |> ignore
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



let render maze =
    let cellWidth = 10;
    let cellHeight = 10;
    let pw = maze.Width * cellWidth
    let ph = maze.Height * cellHeight
    let passageBrush = System.Drawing.Brushes.White
    let wallBrush = System.Drawing.Brushes.Black
    let bmp = new System.Drawing.Bitmap(pw,ph)
    let g = System.Drawing.Graphics.FromImage(bmp);
    maze.Grid
    |> Array2D.iteri 
        (fun y x cell ->
            let brush = 
                match cell with
                | Passaggio -> passageBrush
                | Muro -> wallBrush
            g.FillRectangle(brush,x*cellWidth,y*cellHeight,cellWidth,cellHeight)
        )
    g.Flush()
(*
[< NoEquality; NoComparison>]
type state ={
    player: sprite
}*)

let main () =       
    let engine = new engine (2*W, H)

    let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        // move player
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> 0., -1.
            | 's' -> 0., 1.
            | 'a' -> -1., 0.
            | 'd' -> 1., 0.
            | _   -> 0., 0.
        // TODO: check bounds
        st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'

    let maz (grid: Maze): pixel[] = 
        let pixelarray = Array.zeroCreate ((grid.Height)*(grid.Width)*2) 
        //printf "\n\n%A\n" pixelarray.Length
        grid.Grid |> Array2D.iteri 
            (fun y x cell ->
                let c = 
                    match cell with
                    | Muro -> pixel.wall
                    | Passaggio -> pixel.path
                //printf "%A %A %A\n" x y W
                if x<>W || y<>H then 
                    let pos = y*W+x
                    pixelarray.[2*pos] <- c
                    pixelarray.[2*pos+1] <- c
                //pixelarray
            )
        pixelarray
    // create simple backgroud and player
    let mazing = generate (initMaze W H) 

    //ignore <| 
    ignore <| engine.create_and_register_sprite (new image (W,H,(maz mazing)), 0, 0, 0)
    let pixGiocatore = pixel.create(Config.wall_pixel_char, Color.Red)
    let pixArrivo = pixel.create(Config.wall_pixel_char, Color.Blue)
    let giocatore = engine.create_and_register_sprite (image.rectangle (2, 1, pixGiocatore), 1, 1, 2)
    let arrivo = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo), W-3, H-2, 2)
    //let player = engine.create_and_register_sprite (image.circle (2, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)
    
    //engine.

    // initialize state
    let st0 = { 
        player = giocatore
        }
    //start engine
    engine.loop_on_key my_update st0