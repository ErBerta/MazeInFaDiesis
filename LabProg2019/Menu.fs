﻿(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: Maze Manager
* (C) 2020 Group 10 - Elia Bertapelle (), Leonardo Piccolo (882351) @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Menu

open System
open External
open Engine
open Gfx
open System.Text

//let mutable gameMod = Config.GameMod.Exit

[< NoEquality; NoComparison >]
type state = {
    choiceMenu : sprite
}
let mutable gameMod = Config.GameMod.Exit
let main (W,H) = 
    let engine = new engine (W, H)
    let maxy = 6. //numero scelte - 2 (cornice + introduzione del menu)
    let miny = 1. //minimo, non cambiare
    
    //MENU
    let updateMenu (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =
        let ismenu (x,y) =
            if st.choiceMenu.y + y < maxy && st.choiceMenu.y + y > miny then x,y else 0.,0.
        //la 'scelta' potra' muoversi solo su e giu' in un intervallo limitato dalla funzione 'ismenu' che controlla se la prossima posizione rientra in quelle selezionabili
        let dx, dy =
            match key.KeyChar with 
            |'w'|'W' -> ismenu(0.,-1.)
            |'s'|'S' -> ismenu(0., 1.)
            | _   -> ismenu(0., 0.)
        //eseguo il movimento della scelta
        ignore <| st.choiceMenu.move_by (dx, dy)

        //controllo la posizione y della scelta per individuare la modalita' del gioco da avviare
        if key.KeyChar = 'p' then
            match (st.choiceMenu.y - 1.) with
            | 1. -> gameMod <- Config.GameMod.OnePlayer
            | 2. -> gameMod <- Config.GameMod.Auto
            | 3. -> gameMod <- Config.GameMod.MazeEasterEgg
            | 4. -> gameMod <- Config.GameMod.MultiPlayer
            | _ -> failwith "errore"

        //salvo lo stato 'exit' per interpretarlo correttamente una volta arrivato al main
        if key.KeyChar = 'q' || key.KeyChar = 'Q' then gameMod <- Config.GameMod.Exit 
        //se vengono premuti i caratteri 'q' e 'p' esco dal loop e passo al main per l'esecuzione del gioco
        if key.KeyChar = 'q' || key.KeyChar = 'Q' || key.KeyChar = 'p' || key.KeyChar = 'P' then st,true else st,false

    //creo lo sfondo del menu' utilizzando un quadrato gia' supportato dal motore
    let menu = engine.create_and_register_sprite (image.rectangle (W, H/2, pixel.filled Color.Blue, pixel.filled Color.Yellow),0,0,1)
    menu.draw_text("Game\n_1._Interactive_maze\n_2._Automatic_maze_resolver\n_3._Interactive_maze_with_special_funtions\n_4._\n\n\nUse_W^_Sv_to_move.\nPress_'p'_to_enter_or_'q'_to_quit\n", 2, 1, Color.Red, Color.Yellow)
    //creo lo sprite del menu' (un rettangolo colorato che si muove su e giu)
    let pixChoice = pixel.create(Config.wall_pixel_char, Color.Red)
    let choice = engine.create_and_register_sprite (image.rectangle (1, 1, pixChoice), 2, 2, 2)
    //salvo lo stato dello sprite
    let st0 = {
        choiceMenu = choice
    }
    //avvio il key loop che rimane in attesa della pressione di un tasto
    engine.loop_on_key updateMenu st0