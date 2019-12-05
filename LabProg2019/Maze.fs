(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open System
open External
open Gfx
open System.Text

type CharInfo with
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    static member internal path = pixel.filled Color.Black
    member this.isWall = this = pixel.wall


// TASK 1: implement the maze type
type maze (w, h) =

    member private __.generate = ()
    