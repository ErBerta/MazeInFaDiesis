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

let W = 30
let H = 30



//COPIATO 
//Maze.initMaze 50 50 |> Maze.generate |> Maze.show |> Maze.render


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

let generate (maze : Maze) : Maze =
    let isLegal (x,y) =
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    
    let frontier (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Muro)
    
    let neighbor (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Passaggio)
    
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

    let x,y = randomCell()
    maze.Grid.[x,y] <- Passaggio
    extend (frontier (x,y))

    maze

let show maze =
    printfn "%A" maze
    maze.Grid |> Array2D.iteri 
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
    let engine = new engine (W, H)

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
        let pixelarray = Array.zeroCreate ((grid.Height)*(grid.Width)) 
        printf "\n\n%A\n" pixelarray.Length
        grid.Grid |> Array2D.iteri 
            (fun y x cell ->
                let c = 
                    match cell with
                    | Muro -> pixel.wall
                    | Passaggio -> pixel.path
                //printf "%A %A %A\n" x y W
                if x<>W || y<>H then 
                    pixelarray.[y*W+x] <- c
                //pixelarray
            )
        pixelarray
    // create simple backgroud and player
    let mazing = generate (initMaze W H) 

    //ignore <| 
    let labirinto = engine.create_and_register_sprite (new image (W,H,(maz mazing)), 0, 0, 0)
    //let player = engine.create_and_register_sprite (image.circle (2, pixel.filled Color.White, pixel.filled Color.Gray), W / 2, H / 2, 1)
    
    //engine.

    // initialize state
    let st0 = { 
        player = labirinto
        }
    //start engine
    engine.loop_on_key my_update st0