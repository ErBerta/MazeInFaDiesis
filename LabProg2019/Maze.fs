(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: Maze Manager
* (C) 2020 Group 10 - Elia Bertapelle (881359), Leonardo Piccolo (882351) @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Engine
open Gfx
open System.Threading

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall

[< NoEquality; NoComparison >]
type state = {
    player : sprite
    lab : sprite
    arrived : sprite
    path_color: Color
}

[< NoEquality; NoComparison >]
type state_multi = {
    player : sprite
    player1 : sprite
    lab : sprite
    arrived : sprite
    arrived1 : sprite
    path_color: Color
    path_color1: Color
}


//Definizione tipi di supporto
type Direction = | LEFT | RIGHT | UP | DOWN | NULL
type Visit = Visited | NotVisited
type Cell = | Wall | Path

type Maze = { 
    Grid : Cell[,]
    Width : int
    Height : int
    Visited : Visit[,]
}


//Istanziamento della classe random
let rng = new System.Random()

//definizione dimensione del labirinto di default
let mutable W,H = 25,25

//definizione coordinate dell'arrivo
//let finex = W*2-4
//let finey = H-2

let mutable killerPointx = 0
let mutable killerPointy = 0

//creazione del motore grafico
let mutable engine = new engine (2*W, H)

//inizializza tutta la matrice MAZE a "muro"
let initMaze dx dy = 
    { 
        Grid = Array2D.init dx dy (fun _ _ -> Wall) 
        Width = dx
        Height = dy
        Visited = Array2D.init W H (fun _ _ -> NotVisited)
    }

///Generatore del labirinto
let generate (maze : Maze) : Maze =
    ///Controllo che le coordinate siano valide
    let isPossible (x,y) =
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    
    //Lista delle coordinate dei delle celle 'Wall' che son vicine a quella indicata (a distanza di 2) 
    let frontier (x,y) =
        [x-2,y; x+2,y; x,y-2; x,y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Wall)

    //Lista delle coordinate dei delle celle 'Path' che son vicine a quella indicata (a distanza di 2) 
    let neighbor (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Path)
    

    ///Generatore di coordinate valide random
    let randomCell () = rng.Next(maze.Width),rng.Next(maze.Height)

    let removeAt index (lst : (int * int) list) : (int * int) list =
        let x,y = lst.[index]
        lst |> List.filter (fun (a,b) -> not (a = x && b = y) )
    
    ///Funzione che restituisce le coordinate della cella intermedia
    let getMiddleCell p1 p2 =
        let x = 
            match (fst p2 - fst p1) with
            | 0 -> fst p1
            | 2 -> 1 + fst p1
            | -2 -> -1 + fst p1
            | _ -> failwith "Error. Not supported"
        let y = 
            match (snd p2 - snd p1) with
            | 0 -> snd p1
            | 2 -> 1 + snd p1
            | -2 -> -1 + snd p1
            | _ -> failwith "Error. Not supported"
        (x,y)
    

    let connectRandomNeighbor (x,y) =
        let neighbors = neighbor (x,y)
        let pickedIndex = rng.Next(neighbors.Length)
        let xn,yn = neighbors.[pickedIndex]
        let xb,yb = getMiddleCell (x,y) (xn,yn)
        maze.Grid.[xb,yb] <- Path
        ()
    
    let rec extend front =
        match front with
        | [] -> ()
        | _ ->
            let pickedIndex = rng.Next(front.Length)
            let xf,yf = front.[pickedIndex]
            maze.Grid.[xf,yf] <- Path
            connectRandomNeighbor (xf,yf)
            extend ((front |> removeAt pickedIndex) @ frontier (xf,yf))

    ///Ottiene un punto casuale di partenza per la generazione del labirinto, con coordinate valide
    let rec getInitCell (x,y) =
        let maxx = maze.Width
        let maxy = maze.Height
        //controllo che le coordinate generate non siano sui bordi o che non siano in una posizione dispari
        match x,y with
        | 0,_ -> getInitCell (randomCell())
        | _,0 -> getInitCell (randomCell())
        | x,y -> if x%2=0 || y%2=0 || x=maxx || y=maxy then getInitCell (randomCell()) else x,y
    

    let x,y = getInitCell (randomCell())
    maze.Grid.[x,y] <- Path
    extend (frontier (x,y))
    
    maze


///Generazione e del Labirinto
let mutable mazing = generate(initMaze W H)

let wait_escape (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state)=
   match key.KeyChar with 
   | 'q' -> st, true
   | _ -> st,false

let message (message: String) (z:int) st =
    let width = message.Length + 6 
    let rect= image.rectangle (width, 5, pixel.filled Color.Blue, pixel.filled Color.Yellow)
    rect.draw_text(message, 3, 2, Color.Red, Color.Yellow)
    ignore <| engine.create_and_register_sprite (rect, W-(width/2), H/2, z)
    engine.loop_on_key wait_escape st
  

///Funzione suppporto per la creazione degli sprite chetengono traccia del percorso
let creaPixPath (st:state) color char z =
    let pixpath = pixel.create(char, color)
    ignore <| engine.create_and_register_sprite (image.rectangle (2, 1, pixpath), int (st.player.x), int (st.player.y), z)

//gestione movimenti modalita' interattiva
let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
    let isWall (x,y) =
        if mazing.Grid.[int (st.player.x / 2. + x), int (st.player.y + y)] = Path then 2.*x,y else 0.,0.
    //aggiornamento della traccia del percorso
    creaPixPath st st.path_color Config.filled_pixel_char 2
    let dx, dy =
        match key.KeyChar with 
        | 'w'|'W' -> isWall(0.,-1.)
        | 's'|'S' -> isWall(0., 1.)
        | 'a'|'A' -> isWall(-1., 0.)
        | 'd'|'D' -> isWall(1., 0.)
        | _   -> 0., 0.
    //spostamento effettivo del player
    st.player.move_by (dx, dy)
    //messaggi di log, utili in fase di debug
    Log.msg  "Move to (%A, %A)" (st.player.x/2.) (st.player.y)

    //EASTER EGG - Killer point ~ funziona solo nella modalita' interattiva specifica
    if st.player.x = float (killerPointx*2) && st.player.y = float killerPointy then 
        st.player.clear
        //st.arrived.clear
        message "Surprise!_You're_die!_Game_Over" 6 st 
        st, true
    //controllo se è arrivato
    else if st.player.x = st.arrived.x && st.player.y = st.arrived.y then 
        //st.player.clear
        st.arrived.clear
        message "End" 5 st
        st, true
    else
        st, key.KeyChar = 'q'



let AutoResolver st screen =    
    ///Funzione per testare se è possibile spostarsi nella posizione specificata
    let trymove (stat:state) (direction:Direction) =
        let isWall (x,y) =
            if mazing.Grid.[int (stat.player.x / 2. + x), int (stat.player.y + y)] = Path then 2.*x,y else 0.,0.

        let ret =
                match direction with
                | Direction.LEFT    ->  isWall(-1., 0.)
                | Direction.RIGHT   ->  isWall(1., 0.)
                | Direction.UP      ->  isWall(0.,-1.)
                | Direction.DOWN    ->  isWall(0., 1.)
                | Direction.NULL    ->  0.,0.
        ret

    ///Funzione che sposta il player dello stat verso la direzione indicata
    let move (stat:state) (direction:Direction) (screen : wronly_raster) =
        let dx= stat.player.x
        let dy= stat.player.y
        //sfrutto la my_update della versione interattiva per la gestione dei movimenti
        let st, ret =
                match direction with
                | Direction.LEFT    ->  my_update (new ConsoleKeyInfo ('a', new ConsoleKey(),false, false, false )) screen stat
                | Direction.RIGHT   ->  my_update (new ConsoleKeyInfo('d', new ConsoleKey(),false, false, false )) screen stat
                | Direction.UP      ->  my_update (new ConsoleKeyInfo('w', new ConsoleKey(),false, false, false )) screen stat
                | Direction.DOWN    ->  my_update (new ConsoleKeyInfo('s', new ConsoleKey(),false, false, false )) screen stat
                | Direction.NULL    ->  (stat, false)
        //aggiornamento dello schermo
        engine.refresh st false
        if stat.player.x = dx && stat.player.y = dy then
            (ret, false)
        else 
            (ret, true)
    
    //flag per identificare l'arrivo
    let mutable stop = false

    ///funzione ricorsiva per la ricerca che tiene traccia delle stato, dello schermo e dell'ultima azione svolta
    let rec research (st:state) (screen: wronly_raster) (dx,dy) =
        let wait = 1
        if not stop then
            Thread.Sleep(wait)
            //controllo la possibilità di spostarmi in giù e di non esserci gia andato
            let dxd, dyd = trymove st Direction.DOWN 
            if (dxd,dyd)<>(0.,0.) && (mazing.Visited.[(int st.player.x+int dxd)/2,int st.player.y+ int dyd] <> Visited) then
                //aggiorno la matrice 
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.DOWN screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "Down (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxd,dyd)
                else
                    stop <- true
        
        if not stop then
        //controllo la possibilità di spostarmi a destra e di non esserci gia andato
            let dxr, dyr = trymove st Direction.RIGHT
            if (dxr,dyr)<>(0.,0.) && (mazing.Visited.[(int st.player.x + int dxr)/2,int st.player.y + int dyr] <> Visited) then
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.RIGHT screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "Right (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxr,dyr)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi a sinistra e di non esserci gia andato
            let dxl, dyl = trymove st Direction.LEFT 
            if (dxl,dyl)<>(0.,0.) && (mazing.Visited.[(int st.player.x + int dxl)/2,int st.player.y + int dyl] <> Visited) then
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.LEFT screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "Left (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxl,dyl)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi in su e di non esserci gia andato
            let dxu, dyu = trymove st Direction.UP 
            if (dxu,dyu)<>(0.,0.) && (mazing.Visited.[(int st.player.x+ int dxu)/2 ,int st.player.y + int dyu] <> Visited) then
                mazing.Visited.[(int st.player.x)/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.UP screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "Up (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxu,dyu)
                else
                    stop <- true

        if not stop then
            //segno il pecorso come già visitato 2 volte
            creaPixPath st Color.Red Config.filled_pixel_char 4
            //torno indietro
            st.player.move_by(-dx,-dy)
            Thread.Sleep(wait)
            engine.refresh st false

    //richiamo funzione per avvio della ricorsione
    research st screen (0.,0.)
    


///Funzione per la gestione dell'avvio della risoluzione automatica
let auto_start (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) : (state*bool) = 
    match key.KeyChar with 
    |'s'|'S' -> AutoResolver st screen
                st, true
    |'q'|'Q' -> st, true
    | _   -> st, false

let multi_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st:state_multi) : (state_multi*bool) =
    let st0 = { 
        player = st.player
        lab = st.lab
        arrived = st.arrived
        path_color = st.path_color
    }
    let st1 = { 
        player = st.player1
        lab = st.lab
        arrived = st.arrived1
        path_color = st.path_color1
    }
    let s,r=
        match key.KeyChar with 
        | 'a' | 's' |'d' |'w'| 'A' | 'S' |'D' |'W' -> my_update key screen st0
        | 'j' | 'J' ->  my_update (new ConsoleKeyInfo('a', new ConsoleKey(),false, false, false )) screen st1
        | 'l' | 'L' ->  my_update (new ConsoleKeyInfo('d', new ConsoleKey(),false, false, false )) screen st1
        | 'i' | 'I' ->  my_update (new ConsoleKeyInfo('w', new ConsoleKey(),false, false, false )) screen st1
        | 'k' | 'K' ->  my_update (new ConsoleKeyInfo('s', new ConsoleKey(),false, false, false )) screen st1
        | 'q' -> st0, true
        | _ -> st0, false
        //   | ('a' | 's' |'d' |'w') -> my_update key screen st1
    if r then
        if (st.player.x,st.player.y) = (st.arrived.x,st.arrived.y) then
            message "Il_player_1_ha_vinto!!" 6 st0
        if (st.player1.x,st.player1.y) = (st.arrived1.x,st.arrived1.y) then
            message  "Il_player_2_ha_vinto!!" 6 st1
    st, r
  

///Funzione di avvio del labirinto. Necessario fornire la modalità di gioco
let main (gm: Config.GameMod) (mW,mH) =
    W <- mW
    H <- mH
    engine <- new engine (2*W, H)
    mazing <- generate(initMaze W H)
    ///convertirore della griglia del labirinto generato e raddoppio delle pareti in orizzontale
    let maz (grid: Maze): pixel[] = 
        //creo un'array vuoto 
        let pixelarray = Array.zeroCreate ((grid.Height)*(grid.Width)*2) 
        //riempimento array
        grid.Grid |> Array2D.iteri 
            (fun y x cell ->
                let c = 
                    match cell with
                    | Wall -> pixel.wall
                    | Path -> pixel.path
                if x<>W || y<>H then 
                    //calcolo della posizione nell'array, date lo coordinete della matrice
                    let pos = x*W+y
                    pixelarray.[2*pos] <- c
                    pixelarray.[2*pos+1] <- c
            )
        pixelarray


  
    //creazione degli sprite
    //creazione e registrazione dello sprite del labirinto
    let lab = engine.create_and_register_sprite (new image (W*2,H,(maz mazing)), 0, 0, 0)
    //creazione e registrazione panel per mostrare le info
    let InfoPanel = engine.create_and_register_sprite (image.rectangle (W*2, 1, pixel.filled Color.White, pixel.filled Color.White),0,0,4)
    //definizione dei tipi di pixel per il giocatore e dell'arrivo
    let pixGiocatore = pixel.create(Config.wall_pixel_char, Color.Blue)
    let pixArrivo = pixel.create(Config.wall_pixel_char, Color.Yellow)
    //creazione e registrazione sprite del giocatore e dell'arrivo
    let player = engine.create_and_register_sprite (image.rectangle (2, 1, pixGiocatore), 2, 1, 2)
    let arrive = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo), W*2-4, H-2, 2)

    //inizializzazione variabile state
    let st0 = { 
        player = player
        lab = lab
        arrived = arrive
        path_color = Color.Cyan
    }

    killerPointx <- 0
    killerPointy <- 0
   
    //start engine
    match gm with
    | Config.GameMod.Auto ->
        let instruction = "Press 's' to start the automatic resolver, 'q' to quit"
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key auto_start st0 
        let instruction = "Press any key to exit..."
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
    | Config.GameMod.OnePlayer ->
        let instruction = "Use W^ A< Sv D> to move your player. Press 'q' to exit."
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key my_update st0
    | Config.GameMod.MultiPlayer -> 
        let instruction = "Use W^ A< Sv D> to move player 1. Use I^ J< Kv L>  to move player 2. Press 'q' to exit."
        let pixGiocatore1 = pixel.create(Config.wall_pixel_char, Color.Green)
        let pixArrivo1 = pixel.create(Config.wall_pixel_char, Color.Red)
        let player1 = engine.create_and_register_sprite (image.rectangle (2, 1, pixGiocatore1), W*2-4, 1, 2)
        let arrive1 = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo1), 2, H-2, 2)

        let status = { 
            player = st0.player
            player1 = player1
            lab = lab
            arrived = st0.arrived
            arrived1 = arrive1
            path_color = st0.path_color
            path_color1 = Color.Yellow
        }
        
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key multi_update status
        
    | _ -> 
        while mazing.Grid.[ killerPointx,killerPointy]<> Cell.Path do 
            killerPointx <- rng.Next (mazing.Width-1) 
            killerPointy <- rng.Next (mazing.Height-1)
        Log.msg "Killer point on (X: %A, Y: %A)" killerPointx killerPointy
        let instruction = "Use W^ A< Sv D> to move your player. Press 'q' to exit."
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key my_update st0
