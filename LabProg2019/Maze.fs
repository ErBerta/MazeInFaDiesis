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
open System.Diagnostics

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall

//Definizionde del tipo stato del giocatore
[< NoEquality; NoComparison >]
type state = {
    player : sprite
    lab : sprite
    finish : sprite
    path_color: Color
}

//definizione di tipo dello stato di supporto per il multiplayer, contenente gli stati dei giocatori
[< NoEquality; NoComparison >]
type state_multi = {
    st_player0: state
    st_player1: state
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

//definizione dimensione del labirinto (assegno un valore di default nel caso qualcosa dovesse andare storto)
let mutable W,H = 25,25

//cronometro per l'autoresolver
let mutable stopWatch = new Stopwatch ()

let mutable killerPointx = 0
let mutable killerPointy = 0

//creazione del motore grafico
let mutable engine = new engine (2*W, H)

//flag per identificare l'arrivo
let mutable stop = false

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
    
    ///Lista delle coordinate delle celle 'Wall' che son vicine a quella indicata (a distanza di 2) 
    let frontier (x,y) = 
        [x-2,y; x+2,y; x,y-2; x,y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Wall)

    ///Lista delle coordinate delle celle 'Path' che son vicine a quella indicata (a distanza di 2) 
    let neighbor (x,y) = 
        [x-2,y;x+2,y; x,y-2; x, y+2] |> List.filter (fun (x,y) -> isPossible (x,y) && maze.Grid.[x,y] = Path)
    
    ///Generatore di coordinate valide random
    let randomCell () = 
        rng.Next(maze.Width),rng.Next(maze.Height)

    ///Restituisce la lista senza l'elemento di indice 'index' da una lista di coppie di interi
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
    
    ///collegamento tra il punto indicato e un vicino "a caso"
    let connectRandomNeighbor (x,y) =
        //ottiene lista vicini di x,y
        let neighbors = neighbor (x,y)
        //scelta randomica del vicino da collegare
        let selectedIndex = rng.Next(neighbors.Length)
        //ottenimento coordinate del vicino scelto
        let xn,yn = neighbors.[selectedIndex]
        //ottenimento cella intermedia tra il punto indicato e il vicino scelto
        let xb,yb = getMiddleCell (x,y) (xn,yn)
        maze.Grid.[xb,yb] <- Path
        ()
    
    ///estensione ricorsiva del punto
    let rec pathextender front =
        match front with
        | [] -> ()
        | _ -> 
            //scelta randomica di uno dei vicini e impostazione a passaggio
            let selectedIndex = rng.Next(front.Length)
            let xf,yf = front.[selectedIndex]
            maze.Grid.[xf,yf] <- Path
            connectRandomNeighbor (xf,yf)
            //richiamo ricorsivo passando la lista dei vicini del nuovo punto e la lista dei vicini del punto precedente, rimuovendo quello già usato
            pathextender ((front |> removeAt selectedIndex) @ frontier (xf,yf))

    ///Ottiene un punto casuale di partenza per la generazione del labirinto, con coordinate valide
    let rec getInitCell (x,y) =
        let maxx = maze.Width
        let maxy = maze.Height
        //controllo che le coordinate generate non siano sui bordi o che non siano in una posizione dispari
        match x,y with
        | 0,_ -> getInitCell (randomCell())
        | _,0 -> getInitCell (randomCell())
        | x,y -> if x%2=0 || y%2=0 || x=maxx || y=maxy then getInitCell (randomCell()) else x,y
    
    //genero punto di partenza
    let x,y = getInitCell (randomCell())
    //imposto il punto a path
    maze.Grid.[x,y] <- Path
    //avvio la generazione ricorsiva del labirinto
    pathextender (frontier (x,y))
    
    maze


///Generazione del Labirinto
let mutable mazing = generate(initMaze W H)

//attende il tasto d'uscita
let wait_escape (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state)=
   match key.KeyChar with 
   |'q' | 'Q' -> st, true
   | _ -> st,false

//trova larghezza e altezza del rettangolo da creare
let findvalues (message:string) =
    //trova la lunghezza massima delle varie stringhe
    let rec maxL (split:string[]) (i:int) :int=
        if i>=0 then
            let dim = split.[i].Length
            let dim2 = maxL split (i-1)
            if dim > dim2 then dim else dim2
        else
            0
    let split = message.Split ('\n')
    (maxL split (split.Length-1),split.Length)

//stampa messaggio e avvia il motore in attesa di una risposta
let message (message: String) (z:int) st =
    let (width,height) = findvalues message
    let rect= image.rectangle (width+6, height + 4, pixel.filled Color.Blue, pixel.filled Color.Yellow)
    rect.draw_text(message, 3, 2, Color.Red, Color.Yellow)
    ignore <| engine.create_and_register_sprite (rect, W-((width+6)/2), H/2-((height + 4)/2), z)
    engine.loop_on_key wait_escape st
  

///Funzione suppporto per la creazione degli sprite chetengono traccia del percorso
let creaPixPath (st:state) color char z =
    let pixpath = pixel.create(char, color)
    ignore <| engine.create_and_register_sprite (image.rectangle (2, 1, pixpath), int (st.player.x), int (st.player.y), z)

//gestione movimenti modalita' interattiva
let my_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
    let isWall (x,y) =
        if mazing.Grid.[int (st.player.x / 2. + x), int (st.player.y + y)] = Path then 
            creaPixPath st st.path_color Config.filled_pixel_char 2 
            2.*x,y 
        else 0.,0.
    //aggiornamento della traccia del percorso
    //creaPixPath st st.path_color Config.filled_pixel_char 2
    let dx, dy =
        match key.KeyChar with 
        |'w'|'W' -> isWall(0.,-1.)
        |'s'|'S' -> isWall(0., 1.)
        |'a'|'A' -> isWall(-1., 0.)
        |'d'|'D' -> isWall(1., 0.)
        | _ -> 0., 0.
    //spostamento effettivo del player
    st.player.move_by (dx, dy)
    //messaggi di log, utili in fase di debug
    Log.msg  "Move to (%A, %A)" (st.player.x/2.) (st.player.y)

    //EASTER EGG - Killer point ~ funziona solo nella modalita' interattiva specifica
    if st.player.x = float (killerPointx*2) && st.player.y = float killerPointy then 
        st.player.clear
        message "Surprise!_You're_die!\nGame_Over" 6 st 
        st, true
    //controllo se è arrivato
    else if st.player.x = st.finish.x && st.player.y = st.finish.y then 
        message "Game_Over" 5 st
        st, true
    else
        st,(key.KeyChar = 'q' || key.KeyChar = 'Q')



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
    


    ///funzione ricorsiva per la ricerca del percorso, salva le celle in cui è passata in un array di supporto
    let rec research (st:state) (screen: wronly_raster) (dx,dy) =
        let wait = 500
        
        if not stop then
            //controllo la possibilità di spostarmi a destra e di non esserci gia andato
            let dxr, dyr = trymove st Direction.RIGHT
            if (dxr,dyr)<>(0.,0.) && (mazing.Visited.[(int st.player.x + int dxr)/2,int st.player.y + int dyr] <> Visited) then
                //imposto un tempo di attesa per migliorare la visualizzazione dell'utente
                Thread.Sleep(wait)
                //aggiorno la matrice 
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                //muovo lo sprite
                let reto, fao = move st Direction.RIGHT screen
                //errore
                if not(fao) then
                    failwith "Error, automatic resolution failed"
                Log.msg  "Right (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxr,dyr)
                else
                    stop <- true    //caso uscita

        if not stop then
            //controllo la possibilità di spostarmi in giù e di non esserci gia andato
            let dxd, dyd = trymove st Direction.DOWN 
            if (dxd,dyd)<>(0.,0.) && (mazing.Visited.[(int st.player.x+int dxd)/2,int st.player.y+ int dyd] <> Visited) then
                Thread.Sleep(wait)
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.DOWN screen
                if not(fao) then
                    failwith "Error, automatic resolution failed"
                Log.msg  "Down (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxd,dyd)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi a sinistra e di non esserci gia andato
            let dxl, dyl = trymove st Direction.LEFT 
            if (dxl,dyl)<>(0.,0.) && (mazing.Visited.[(int st.player.x + int dxl)/2,int st.player.y + int dyl] <> Visited) then
                Thread.Sleep(wait)
                mazing.Visited.[int st.player.x/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.LEFT screen
                if not(fao) then
                    failwith "Error, automatic resolution failed"
                Log.msg  "Left (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxl,dyl)
                else
                    stop <- true

        if not stop then
            //controllo la possibilità di spostarmi in su e di non esserci gia andato
            let dxu, dyu = trymove st Direction.UP 
            if (dxu,dyu)<>(0.,0.) && (mazing.Visited.[(int st.player.x+ int dxu)/2 ,int st.player.y + int dyu] <> Visited) then
                Thread.Sleep(wait)
                mazing.Visited.[(int st.player.x)/2,int st.player.y] <- Visited
                let reto, fao = move st Direction.UP screen
                if not(fao) then
                    failwith "Error, automatic resolution failed"
                Log.msg  "Up (%A, %A)" (st.player.x/2.) (st.player.y)
                if not reto then
                    research st screen (dxu,dyu)
                else
                    stop <- true

        if not stop then
            Thread.Sleep(wait)
            //segno il pecorso come già visitato 2 volte
            creaPixPath st Color.Red Config.filled_pixel_char 4
            //torno indietro
            st.player.move_by(-dx,-dy)
            engine.refresh st false

    //richiamo funzione per avvio della ricorsione
    research st screen (0.,0.)
    


///Funzione per la gestione dell'avvio della risoluzione automatica
let auto_start (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) : (state*bool) = 
    match key.KeyChar with 
    |'s'|'S' -> stopWatch.Reset ()
                stopWatch.Start () //ottengo il tempo impiegato tra i log
                AutoResolver st screen
                stopWatch.Stop();
                Log.msg "Time elapsed: %i min, %i sec, %i ms" stopWatch.Elapsed.Minutes stopWatch.Elapsed.Seconds stopWatch.Elapsed.Milliseconds
                st, true
    |'q'|'Q' -> st, true
    | _   -> st, false

//gestione aggiornamento posizione multiplayer
let multi_update (key : ConsoleKeyInfo) (screen : wronly_raster) (st:state_multi) : (state_multi*bool) =
    let st0 = st.st_player0
    let st1 = st.st_player1
    let s,r=
        match key.KeyChar with 
        | 'a' | 's' |'d' |'w'| 'A' | 'S' |'D' |'W' -> my_update key screen st0
        | 'j' | 'J' ->  my_update (new ConsoleKeyInfo('a', new ConsoleKey(),false, false, false )) screen st1
        | 'l' | 'L' ->  my_update (new ConsoleKeyInfo('d', new ConsoleKey(),false, false, false )) screen st1
        | 'i' | 'I' ->  my_update (new ConsoleKeyInfo('w', new ConsoleKey(),false, false, false )) screen st1
        | 'k' | 'K' ->  my_update (new ConsoleKeyInfo('s', new ConsoleKey(),false, false, false )) screen st1
        | 'q' | 'Q' -> st0, true
        | _ -> st0, false

    //gestione vittoria p
    if r then
        if (st0.player.x,st0.player.y) = (st0.finish.x,st0.finish.y) then
            message "Il_player_1_ha_vinto!!" 6 st0
        if (st1.player.x,st1.player.y) = (st1.finish.x,st1.finish.y) then
            message  "Il_player_2_ha_vinto!!" 6 st1
    st, r
  

///Funzione di avvio del labirinto. Necessario fornire la modalità di gioco
let main (gm: Config.GameMod) (mW,mH) =
    W <- mW
    H <- mH
    engine <- new engine (2*W, H+4)
    mazing <- generate(initMaze W H)

    ///convertirore della griglia del labirinto generato nell'array di pixel per l'engine, raddoppiando le pareti in orizzontale
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
                    //calcolo della posizione nell'array, date lo coordinate della matrice
                    let pos = x*W+y
                    pixelarray.[2*pos] <- c
                    pixelarray.[2*pos+1] <- c
            )
        pixelarray

    //creazione degli sprite
    //creazione e registrazione dello sprite del labirinto
    let lab = engine.create_and_register_sprite (new image (W*2,H,(maz mazing)), 0, 0, 0)
    //definizione dei tipi di pixel per il giocatore e dell'arrivo
    let pixPlayer = pixel.create(Config.wall_pixel_char, Color.Blue)
    let pixEnd = pixel.create(Config.wall_pixel_char, Color.Yellow)
    //creazione e registrazione sprite del giocatore e dell'arrivo
    let player = engine.create_and_register_sprite (image.rectangle (2, 1, pixPlayer), 2, 1, 2)
    let finish = engine.create_and_register_sprite (image.rectangle (2, 1, pixEnd), W*2-4, H-2, 2)

    //inizializzazione variabile state
    let st0 = { 
        player = player
        lab = lab
        finish = finish
        path_color = Color.Cyan
    }
    //inizializzazione killerpoint
    killerPointx <- 0
    killerPointy <- 0
    //inizializzazione stop
    stop <- false

    //select game mode
    match gm with
    | Config.GameMod.Auto ->
        //definizione e stampa delle istruzioni di gioco per la modalità automatica
        let instruction = "Press_'s'_to_start_resolver.\nPress_'q'_to_quit"
        let (w,h) = findvalues instruction
        //creazione e registrazione panel per mostrare le info
        let infoPanel = engine.create_and_register_sprite (image.rectangle (W*2, h+1, pixel.filled Color.White, pixel.filled Color.White),0,H,4)
        infoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        //avvio key listener 
        engine.loop_on_key auto_start st0 
    | Config.GameMod.OnePlayer ->
        //definizione e stampa delle istruzioni di gioco per la modalità singolo giocatore
        let instruction = "Use_W^_A<_Sv_D>_to_move.\nPress_'q'_to_exit."
        let (w,h) = findvalues instruction
        //creazione e registrazione panel per mostrare le info
        let infoPanel = engine.create_and_register_sprite (image.rectangle (W*2, h+1, pixel.filled Color.White, pixel.filled Color.White),0,H,4)
        infoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        //avvio key listener 
        engine.loop_on_key my_update st0
    | Config.GameMod.MultiPlayer -> 
        //definizione e stampa delle istruzioni di gioco per la modalità multi giocatore
        let instruction = "Use_W^_A<_Sv_D>_to_move_p1.\nUse_I^_J<_Kv_L>_to_move_p2.\nPress_'q'_to_exit."
        let (w,h) = findvalues instruction
        //creazione e registrazione panel per mostrare le info
        let infoPanel = engine.create_and_register_sprite (image.rectangle (W*2, h+1, pixel.filled Color.White, pixel.filled Color.White),0,H,4)
        infoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)

        //definizione del secondo player, del suo arrivo e del suo stato
        let pixPlayer1 = pixel.create(Config.wall_pixel_char, Color.Green)
        let pixArrivo1 = pixel.create(Config.wall_pixel_char, Color.Red)
        let player1 = engine.create_and_register_sprite (image.rectangle (2, 1, pixPlayer1), W*2-4, 1, 2)
        let finish1 = engine.create_and_register_sprite (image.rectangle (2, 1, pixArrivo1), 2, H-2, 2)
        let st1 = { 
            player =  player1
            lab = lab
            finish =  finish1
            path_color =  Color.Yellow
        }

        //stato di supporto per il trasporto degli stati dei giocare
        let status = {
            st_player0 = st0
            st_player1 = st1
        }
        //avvio key listener 
        engine.loop_on_key multi_update status
        
    | _ -> //valorizzo il killerPoint in modo casuale, lo scrivo nei log e avvio la modalità interattiva single player
        while mazing.Grid.[ killerPointx,killerPointy]<> Cell.Path do 
            //valorizzazione killer point
            killerPointx <- rng.Next (mazing.Width-1) 
            killerPointy <- rng.Next (mazing.Height-1)
        Log.msg "Killer point on (X: %A, Y: %A)" killerPointx killerPointy

        //definizione e stampa delle istruzioni di gioco per la modalità giocatore singolo con easter egg
        let instruction = "Use_W^_A<_Sv_D>_to_move_your_player.\nPress_'q'_to_exit."
        let (w,h) = findvalues instruction
        //creazione e registrazione panel per mostrare le info
        let infoPanel = engine.create_and_register_sprite (image.rectangle (W*2, h+1, pixel.filled Color.White, pixel.filled Color.White),0,H,4)
        infoPanel.draw_text (instruction, 2, 0, Color.Red, Color.White)
        //avvio key listener 
        engine.loop_on_key my_update st0
    
