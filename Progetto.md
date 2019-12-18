# Appunti sul progetto

* Non dobbiamo fare un programma intero da 0

## Consiste in
 * Gioco da console
   * Grafica ASCII ~"aski"
   * sprite   
   * CONTINUA A DIR ROBA INUTILE

# TASK 1
 Da controllare
  * Non si possono oltrepasssare i muri. Il player non e' un fantasma
  * Dev'essere un labirinto generato da un algoritmo nostro e giocabile da un giocatore single-player
  * Rilasciano un pdf con la specifica del programma
Consiglio
  * Rappresentare il labirinto attraverso


Ocio alla condivisione dei sorgenti
 * GitAb -> il + carino
 * Cloud -> problema (sovrascrittura)
 * In locale -> problema condivisione sorgenti

* Il let e' TUTTO


## ALGORITMO GENERAZIONE MAZE
```fsharp
let rng = new System.Random()
type Cell = | Blocked | Passage
type Maze =
    {
        Grid : Cell[,]
        Width : int
        Height : int
    }

let initMaze dx dy =
    let six,siy = (1,1)
    let eix,eiy = (dx-2,dy-2)
    {
        Grid = Array2D.init dx dy
            (fun _ _ -> Blocked
            )
        Width = dx
        Height = dy
    }

let generate (maze : Maze) : Maze =
    let isLegal (x,y) =
        x>0 && x < maze.Width-1 && y>0 && y<maze.Height-1
    let frontier (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Blocked)
    let neighbor (x,y) =
        [x-2,y;x+2,y; x,y-2; x, y+2]
        |> List.filter (fun (x,y) -> isLegal (x,y) && maze.Grid.[x,y] = Passage)
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
        maze.Grid.[xb,yb] <- Passage
        ()
    let rec extend front =
        match front with
        | [] -> ()
        | _ ->
            let pickedIndex = rng.Next(front.Length)
            let xf,yf = front.[pickedIndex]
            maze.Grid.[xf,yf] <- Passage
            connectRandomNeighbor (xf,yf)
            extend ((front |> removeAt pickedIndex) @ frontier (xf,yf))

    let x,y = randomCell()
    maze.Grid.[x,y] <- Passage
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
                | Blocked -> "X"
                | Passage -> " "
            printf "%s" c
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
                | Passage -> passageBrush
                | Blocked -> wallBrush
            g.FillRectangle(brush,x*cellWidth,y*cellHeight,cellWidth,cellHeight)
        )
    g.Flush()
    bmp.Save("""E:\temp\maze.bmp""")

initMaze 50 50 |> generate |> show |> render
```

## Consigli 12/12/'19

* Per la ricerca del percorso
  * Utilizzare BFS o DFS per la ricerca del percorso inizio - fine

* Plagio
  * Accettano lo scambio di idee ed eventualmente soluzioni comuni
  * L'implementazione dev'essere diversa (Il codice non dev'essere la stessa identica)
    * In sintesi, basta non fare copia e incolla. Ed eventualmente modificare della roba

* Idee
  * Disegna la strada effettuata
  * La versione non iterativa deve mostrare il percorso da fare
    * Algoritmo ricorsivo:
      * BFS
      * DFS

* Si programma in inglese. Sti cazzi
