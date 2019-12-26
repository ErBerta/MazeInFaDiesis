(*


*)
module LabProg2019.Menu

open System
open External
open Engine
open Gfx
open System.Text

[< NoEquality; NoComparison >]
type state = {
    menuscelta : sprite
}
let mutable scelta = Config.GameMod.Player
let main (W,H) = 
    let engine = new engine (W, H)
    let maxy = 6. //numero scelte - 2 (cornice + introduzione del menu)
    let miny = 1. //minimo, non cambiare
    
    //MENU
    let updateMenu (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let ismenu (x,y) =
            if st.menuscelta.y + y < maxy && st.menuscelta.y + y > miny then x,y else 0.,0.
        //la 'scelta' potra' muoversi solo su e giu' in un intervallo limitato dalla funzione 'ismenu' che controlla se la prossima posizione rientra in quelle selezionabili
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> ismenu(0.,-1.)
            | 's' -> ismenu(0., 1.)
            | _   -> ismenu(0., 0.)
        //eseguo il movimento della scelta
        ignore <| st.menuscelta.move_by (dx, dy)

        //controllo la posizione y della scelta per individuare la modalita' del gioco da avviare
        if key.KeyChar = 'p' then
            match (st.menuscelta.y - 1.) with
            | 1. -> scelta <- Config.GameMod.Player
            | 2. -> scelta <- Config.GameMod.Auto
            | 3. -> scelta <- Config.GameMod.Game2
            | 4. -> scelta <- Config.GameMod.Player
            | _ -> failwith "errore"

        //salvo lo stato 'exit' per interpretarlo correttamente una volta arrivato al main
        if key.KeyChar = 'q' then scelta <- Config.GameMod.Exit 
        //se vengono premuti i caratteri 'q' e 'p' esco dal loop e passo al main per l'esecuzione del gioco
        if key.KeyChar = 'q' || key.KeyChar = 'p' then st,true else st,false

    //creo lo sfondo del menu' utilizzando un quadrato gia' supportato dal motore
    let menu = engine.create_and_register_sprite (image.rectangle (100, 50, pixel.filled Color.Yellow, pixel.filled Color.Blue),0,0,1)
    menu.draw_text("Game\n_1._Labirinto\n_2._Labirinto_automatico\n_3._Altro\n_4._\n", 2, 1, Color.Red, Color.Yellow)
    //creo lo sprite del menu' (un rettangolo colorato che si muove su e giu)
    let pixScelta = pixel.create(Config.wall_pixel_char, Color.Blue)
    let scelta = engine.create_and_register_sprite (image.rectangle (1, 1, pixScelta), 2, 2, 2)
    //salvo lo stato dello sprite
    let st0 = {
        menuscelta = scelta
    }
    //avvio il key loop che rimane in attesa della pressione di un tasto
    engine.loop_on_key updateMenu st0