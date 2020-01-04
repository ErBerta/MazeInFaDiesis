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

type Cell = | Muro | Passaggio

type Maze = { 
    Grid : Cell[,]
    Width : int
    Height : int
}


//Istanziamento della classe random
let rng = new System.Random()

//definizione dimensione del labirinto
let W = 51
let H = 51

//definizione coordinate dell'arrivo
let finex = W*2-4
let finey = H-2


//creazione del motore grafico
let engine = new engine (2*W, H)

//inizializza tutta la matrice MAZE a "muro"
let initMaze dx dy = 
    { 
        Grid = Array2D.init dx dy (fun _ _ -> Muro) 
        Width = dx
        Height = dy
    }

///Generatore del labirinto
let generate (maze : Maze) : Maze =
    ///Controllo che le coordinate siano valide
    let isPossible (x,y) =
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    
    let frontier (x,y) =
        [x-2,y; x+2,y; x,y-2; x,y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Muro)

    
    let neighbor (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Passaggio)
    

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
    maze.Grid.[x,y] <- Passaggio
    extend (frontier (x,y))
    
    maze


///Generazione e del Labirinto
let mazing = generate(initMaze W H)

//gestione uscita "vittoria"
let exit (st:state) = 
    let rect= image.rectangle (11, 5, pixel.filled Color.Yellow, pixel.filled Color.Blue)
    rect.draw_text("Vinto",3, 2, Color.Red, Color.Yellow)
    ignore <| engine.create_and_register_sprite (rect, W-5, H/2, 5)
   
///Funzione suppporto per la creazione degli sprite chetengono traccia del percorso
let creaPixPercorso st colore carattere z =
    let pixpercorso = pixel.create(carattere, colore)
    ignore <| engine.create_and_register_sprite (image.rectangle (2, 1, pixpercorso), int (st.player.x), int (st.player.y), z)

//gestione movimenti modalita' interattiva
let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
    let isWall (x,y) =
        if mazing.Grid.[int (st.player.x / 2. + x), int (st.player.y + y)] = Passaggio then 2.*x,y else 0.,0.
    
    let dx, dy =
        match key.KeyChar with 
        | 'w' -> isWall(0.,-1.)
        | 's' -> isWall(0., 1.)
        | 'a' -> isWall(-1., 0.)
        | 'd' -> isWall(1., 0.)
        | _   -> 0., 0.

    //spostamento effettivo del player
    st.player.move_by (dx, dy)

    //aggiornamento della traccia del percorso
    creaPixPercorso st Color.Cyan Config.filled_pixel_char 2
    
    Log.msg  "(%A, %A)" (st.player.x) (st.player.y)

    //controllo se è arrivato
    if st.player.x = float finex && st.player.y = float finey then 
        st.player.clear
        st.arrived.clear
        exit st
        st, true
    else
        st, key.KeyChar = 'q'

//Definizione tipi di supporto
type Direction = | LEFT | RIGHT | UP | DOWN | NULL
type Visit = Visited | NotVisited

let AutoResolver st screen =    
    ///Funzione per testare se è possibile spostarsi nella posizione specificata
    let trymove (stat:state) (direction:Direction) =
        let isWall (x,y) =
            if mazing.Grid.[int (stat.player.x / 2. + x), int (stat.player.y + y)] = Passaggio then 2.*x,y else 0.,0.

        let dx= stat.player.x
        let dy= stat.player.y
        let ret =
                match direction with
                | Direction.LEFT ->  isWall(-1., 0.)
                | Direction.RIGHT ->  isWall(1., 0.)
                | Direction.UP ->  isWall(0.,-1.)
                | Direction.DOWN ->  isWall(0., 1.)
                | Direction.NULL -> 0.,0.
        ret

    ///Funzione che sposta il player dello stat verso la direzione indicata
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
        //aggiornamento dello schermo
        engine.refresh st false
        if stat.player.x = dx && stat.player.y = dy then
            (ret, false)
        else 
            (ret, true)

    //inizializazione matrice a Con tutti i valori nulli (non visitati)
    let Vis = Array2D.init W H (fun _ _ -> NotVisited)  //<-------------- ELIA! E se usassimo la mazing, che è la griglia generata all'inizio, invece di creare un altra matrice delle stesse dimensioni????
    
    //flag per identificare l'arrivo
    let mutable stop = false

    ///funzione ricorsiva per la ricerca che tiene traccia delle stato, dello schermo e dell'ultima azione svolta
    let rec research (st:state) (screen: wronly_raster) (dx,dy) =
        let wait = 1
        if not stop then
            Thread.Sleep(wait)
            //controllo la possibilità di spostarmi in giù e di non esserci gia andato
            let dxd, dyd = trymove st Direction.DOWN 
            if (dxd,dyd)<>(0.,0.) && (Vis.[(int st.player.x+int dxd)/2,int st.player.y+ int dyd] <> Visited) then
                //Thread.Sleep(wait)
                //aggiorno la matrice 
                Vis.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.DOWN screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
                if not reto then
                    research st screen (dxd,dyd)
                else
                    stop <- true
        
        if not stop then
        //controllo la possibilità di spostarmi a destra e di non esserci gia andato
            let dxr, dyr = trymove st Direction.RIGHT
            if (dxr,dyr)<>(0.,0.) && (Vis.[(int st.player.x + int dxr)/2,int st.player.y + int dyr] <> Visited) then
                //Thread.Sleep(wait)
                Vis.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.RIGHT screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
                if not reto then
                    research st screen (dxr,dyr)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi a sinistra e di non esserci gia andato
            let dxl, dyl = trymove st Direction.LEFT 
            if (dxl,dyl)<>(0.,0.) && (Vis.[(int st.player.x + int dxl)/2,int st.player.y + int dyl] <> Visited) then
                //Thread.Sleep(wait)
                Vis.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.LEFT screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
                if not reto then
                    research st screen (dxl,dyl)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi in su e di non esserci gia andato
            let dxu, dyu = trymove st Direction.UP 
            if (dxu,dyu)<>(0.,0.) && (Vis.[(int st.player.x+ int dxu)/2 ,int st.player.y + int dyu] <> Visited) then
                //Thread.Sleep(wait)
                Vis.[(int st.player.x)/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.UP screen
                if not(fao) then
                    failwith "Error"
                Log.msg  "(%A, %A)" (st.player.x) (st.player.y)
                if not reto then
                    research st screen (dxu,dyu)
                else
                    stop <- true

        if not stop then
            //segno il pecorso come già visitato 2 volte
            creaPixPercorso st Color.Red Config.filled_pixel_char 4
            //torno indietro
            st.player.move_by(-dx,-dy)
            Thread.Sleep(wait)
            engine.refresh st false

    //richiamo funzione per avvio della ricorsione
    research st screen (0.,0.)
    


///Funzione per la gestione dell'avvio della risoluzione automatica
let auto_start (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) : (state*bool) = 
    match key.KeyChar with 
    | 's' -> 
        AutoResolver st screen
        st, true
    | 'q' -> st, true
    | _   -> st, false


///Funzione di avvio del labirinto. Necessario fornire la modalità di gioco
let main (gm: Config.GameMod) =
    ///convertirore della griglia del labirinto generato e raddoppio delle pareti in orizzontale
    let maz (grid: Maze): pixel[] = 
        //creo un'array vuoto 
        let pixelarray = Array.zeroCreate ((grid.Height)*(grid.Width)*2) 
        //riempimento array
        grid.Grid |> Array2D.iteri 
            (fun y x cell ->
                let c = 
                    match cell with
                    | Muro -> pixel.wall
                    | Passaggio -> pixel.path
                if x<>W || y<>H then 
                    //calcolo della posizione nell'array, date lo coordinete della matrice
                    let pos = x*W+y
                    pixelarray.[2*pos] <- c
                    pixelarray.[2*pos+1] <- c
            )
        pixelarray


  
    //creazione degli sprite
    //creazione e registrazione dello sprite del labirinto
    let labirinto = engine.create_and_register_sprite (new image (W*2,H,(maz mazing)), 0, 0, 0)
    //creazione e registrazione panel per mostrare le info
    let InfoPanel = engine.create_and_register_sprite (image.rectangle (W*2, 1, pixel.filled Color.White, pixel.filled Color.White),0,0,4)
    //definizione dei tipi di pixel per il giocatore e dell'arrivo
    let pixGiocatore = pixel.create(Config.wall_pixel_char, Color.Red)
    let pixArrivo = pixel.create(Config.wall_pixel_char, Color.Yellow)
    //creazione e registrazione sprite del giocatore e dell'arrivo
    let giocatore = engine.create_and_register_sprite (image.rectangle (2, 1, pixGiocatore), 2, 1, 2)
    let arrivo = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo), finex, finey, 2)

    // initialize state
    let st0 = { 
        player = giocatore
        lab = labirinto
        arrived = arrivo
    }

    //start engine
    if gm = Config.GameMod.Auto then
        let instruction = "Press 's' to start the automatic resolver, 'q' to quit"
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key auto_start st0 
        let instruction = "Press any key to exit..."
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
    else
        let instruction = "Use W^ A< Sv D> to move your player. Press 'q' to exit."
        InfoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        engine.loop_on_key my_update st0
