(*


*)
module LabProg2019.Menu

open System
open External
open Engine
open Gfx
open System.Text

let mutable gameMod = Config.GameMod.Exit

[< NoEquality; NoComparison >]
type state = {
    menuscelta : sprite
}

let main (W,H) = 
    let engine = new engine (W, H)
    let maxy = 6. //numero scelte - 2
    let miny = 1. //minimo, non cambiare
    //MENU
    let updateMenu (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let ismenu (x,y) =
            if st.menuscelta.y + y < maxy && st.menuscelta.y + y > miny then x,y else 0.,0.
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> ismenu(0.,-1.)
            | 's' -> ismenu(0., 1.)
            | _   -> ismenu(0., 0.)
        // TODO: check bounds
        //controllo se è arrivato
        ignore <| st.menuscelta.move_by (dx, dy)
        
         
        if key.KeyChar = 'p' then
            gameMod <-
                match (st.menuscelta.y - 1.) with
                    | 1. ->  Config.GameMod.Player
                    | 2. ->  Config.GameMod.Auto
                    | 3. ->  Config.GameMod.Game2
                    | 4. ->  Config.GameMod.Player
                    | _ -> Config.GameMod.Exit
            Maze.main (gameMod)
        
        st, key.KeyChar = 'q'


    let menu = engine.create_and_register_sprite (image.rectangle (100, 50, pixel.filled Color.Yellow, pixel.filled Color.Blue),0,0,1)
    menu.draw_text("Game\n_1._Labirinto\n_2._Labirinto_automatico\n_3._Altro\n_4._\n", 2, 1, Color.Red, Color.Yellow)

    let pixScelta = pixel.create(Config.wall_pixel_char, Color.Blue)
    let scelta = engine.create_and_register_sprite (image.rectangle (1, 1, pixScelta), 2, 2, 2)

    let st0 = {
        menuscelta = scelta
    }

    engine.loop_on_key updateMenu st0