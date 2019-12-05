(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Test.fs: test modules
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Test

open System
open System.Runtime.InteropServices
open External
open System.IO


module Encoders =
    [< DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto) >]
    extern bool SetConsoleOutputCP([< MarshalAs(UnmanagedType.U4) >] uint32 wCodePageID)

    [< DllImport("kernel32.dll", SetLastError = true, CharSet = CharSet.Auto) >]
    extern uint32 GetConsoleOutputCP()

    let main () =
        let handle = CreateFile ("CONOUT$", 0x40000000u, 2u, IntPtr.Zero, FileMode.Open, 0, IntPtr.Zero)
        let pcon () =
            Console.WriteLine '█'
            Console.WriteLine '\219'
            Console.WriteLine '\u2588'
        let pwrite () =
            let w = 4
            let h = 5
            let p (c : char) =
                let mutable ci = CharInfo ()
                ci.Attributes <- 0b1110s
                ci.Char.UnicodeChar <- c
                let buf : CharInfo[] = Array.create (w * h) ci
                let mutable rect = new SmallRect (0s, int16 Console.CursorTop, int16 w, int16 Console.CursorTop + int16 h)
                ignore <| WriteConsoleOutput (handle, buf, new Coord (int16 w, int16 h), new Coord (0s, 0s), &rect)
                Console.CursorTop <- Console.CursorTop + h + 1
            p '█'
            p '\219'
            p '\u2588'



        let dotnet () =
            let l = [ Console.OutputEncoding; Text.Encoding.Unicode; Text.Encoding.ASCII; Text.Encoding.UTF8; Text.Encoding.Default ]
            for e in l do
                Console.OutputEncoding <- e
                let code = GetConsoleOutputCP()
                Console.WriteLine (sprintf "\n.NET Console.OutputEncoding: %O (code page: %d) (GetConsoleOutputCP(): %d)" e e.CodePage code)
                pcon ()
        let win32 f =
            let l = [GetConsoleOutputCP(); 850u; 1200u]
            for n in l do
                ignore <| SetConsoleOutputCP n
                let code = GetConsoleOutputCP ()
                Console.WriteLine (sprintf "WIN32 Code Page: %d" code)
                f ()
        dotnet ()
        win32 pwrite
        win32 pcon


module WriteConsoleOutput =

    let main () =
        let h = CreateFile ("CONOUT$", 0x40000000u, 2u, IntPtr.Zero, FileMode.Open, 0, IntPtr.Zero)

        if not h.IsInvalid then
            let buf : CharInfo[] = Array.zeroCreate (80 * 25)
            let mutable rect = new SmallRect (0s, 0s, 80s, 25s)

            for character = 65 to 65 + 26 do
                for attribute = 0 to 15 do
                    for i = 0 to buf.Length - 1 do
                        buf.[i].Attributes <- int16 attribute
                        buf.[i].Char.UnicodeChar <- char character
                        ignore <| WriteConsoleOutput (h, buf, new Coord (80s, 25s), new Coord (0s, 0s), &rect)


let main = Encoders.main
