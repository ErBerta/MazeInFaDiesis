(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Gfx.fs: graphics stuff
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Gfx

open System
open Globals
open System.Drawing
open Prelude
open External
open System.IO
open System.Text


// some utilities
//

/// Calculates the intersection between the two given rectangles.
let clamp (x0, y0, w0, h0) (x1, y1, w1, h1) =
    let r1 = new Rectangle (x0, y0, w0, h0)
    let r2 = new Rectangle (x1, y1, w1, h1)
    r1.Intersect r2
    r1.Left, r1.Top, r1.Width, r1.Height



// pixel type
//

type CharInfo with
    static member create (c : char, fg : Color, ?bg : Color) = 
        let bg = defaultArg bg Color.Black
        let mutable ci = new CharInfo ()
        ci.Char.UnicodeChar <- c
        ci.fg <- fg
        ci.bg <- bg
        ci

    static member filled (fg : Color, ?bg : Color) = CharInfo.create (Config.filled_pixel_char, fg, ?bg = bg) 
    static member empty = CharInfo.create (Config.empty_pixel_char, Color.White) 
    member this.is_empty = this.Char.UnicodeChar = Config.empty_pixel_char


type pixel = CharInfo


       
// write-only raster abstract class: provides basic drawing primitives and no item-getter property 
//

[< AbstractClass; NoEquality; NoComparison; Diagnostics.DebuggerDisplay("{ToString()}") >]
type wronly_raster (w, h) =
    abstract member width : int
    abstract member height : int
    default __.width = w
    default __.height = h

    // low level unsafe write access
    abstract member unsafe_plot : int * int * pixel -> unit

    // item setter shortcut
    member inline this.Item 
        with set (x, y) px = this.unsafe_plot (x, y, px)

    // clear
    abstract member clear : unit
    default this.clear =
        for y = 0 to this.height - 1 do
            for x = 0 to this.width - 1 do
                this.[x, y] <- pixel.empty

    // commit
    abstract member commit : unit

    member this.is_inside (x, y) =
        let f v m = v >= 0 && v < m
        in
            f x this.width && f y this.height

    member this.is_inside (x, y, w, h) = this.is_inside (x, y) && this.is_inside (x + w - 1, y + h - 1)

    member this.plot (x, y, px) =
        if this.is_inside (x, y) then 
            this.[x, y] <- px

    abstract draw_text : string * int * int * Color * ?bg:Color -> unit
    default this.draw_text (s, x, y, fg, ?bg) =
        let mutable dx = 0
        let mutable dy = 0
        for i = 0 to s.Length - 1 do
            match s.Chars i with
            | '\n'  -> dy <- dy + 1
                       dx <- 0
            | c     -> this.plot (x + dx, y + dy, pixel.create (c, fg, ?bg = bg))
                       dx <- dx + 1
      
    // bresenham line drawing algorithm
    member this.draw_line (x0, y0, x1, y1, px) =
        let plot x y = this.plot (x, y, px)
        let steep = abs (y1 - y0) > abs (x1 - x0)
        let x0, y0, x1, y1 = if steep then y0, x0, y1, x1 else x0, y0, x1, y1
        let x0, y0, x1, y1 = if x0 > x1 then x1, y1, x0, y0 else x0, y0, x1, y1
        let dx, dy = x1 - x0, abs (y1 - y0)
        let s = if y0 < y1 then 1 else -1
        let rec R e x y =
            if x <= x1 then
                if steep then plot y x else plot x y
                if e < dy then R (e - dy + dx) (x + 1) (y + s)
                else R (e - dy) (x + 1) y
        in
            R (dx / 2) x0 y0

    member this.draw_rectangle (x0, y0, w, h, px) =
        let x1, y1 = x0 + w - 1, y0 + h - 1
        this.draw_line (x0, y0, x1, y0, px)
        this.draw_line (x0, y0, x0, y1, px)
        this.draw_line (x1, y1, x1, y0, px)
        this.draw_line (x1, y1, x0, y1, px)
               
    // mid-point circle drawing algorithm
    // TODO: da debuggare perché non fa il bordo a sinistra
    [< Obsolete >]
    member this.draw_circle (xc, yc, r, px) =   
        let plot (x, y) = this.plot (x, y, px)
        let mutable x = r
        let mutable y = 0
        plot (x + xc, y + yc) 
        if r > 0 then
            plot (x + xc, -y + yc)
            plot (y + xc, x + yc)
            plot (-y + xc, x + yc)        
        let mutable P = 1 - r
        let mutable quit = false
        while not quit && x > y do 
            y <- y + 1
            if P <= 0 then
                P <- P + 2 * y + 1
            else
                x <- x - 1
                P <- P + 2 * y - 2 * x + 1
            if x < y then quit <- true
            else
                plot (x + xc, y + yc)
                plot (-x + xc, y + yc)
                plot (x + xc, -y + yc)
                plot (-x + xc, -y + yc)
                if x <> y then
                    plot (y + xc, x + yc)
                    plot (-y + xc, x + yc)
                    plot (y + xc, -x + yc)
                    plot (-y + xc, -x + yc)

    member this.draw_unfilled_circle (x0, y0, r, px) =
        let plot x y = this.plot (x, y, px)
        let x = 0
        let y = r
        let m = 5 - 4 * r
        let rec loop x y m =
            plot (x0 + x) (y0 + y)
            plot (x0 + y) (y0 + x)
            plot (x0 - x) (y0 + y)
            plot (x0 - y) (y0 + x)
            plot (x0 + x) (y0 - y)
            plot (x0 + y) (y0 - x)
            plot (x0 - x) (y0 - y)
            plot (x0 - y) (y0 - x)
            let y, m =
                if m > 0 then (y - 1), (m - 8 * y)
                else y, m
            if x <= y then
              let x = x + 1 in
              let m = m + 8 * x + 4 in
              loop x y m      
        loop x y m

    override this.ToString () = sprintf "wronly_raster (%d, %d)" this.width this.height
        


[< AbstractClass; Diagnostics.DebuggerDisplay("{ToString()}") >]
 type raster (w, h) =
    inherit wronly_raster (w, h)

    // low level unsafe read access
    abstract member unsafe_get : int * int -> pixel

    // item getter and setter shortcuts
    member inline this.Item 
        with get (x, y) = this.unsafe_get (x, y)
        and set (x, y) px = this.unsafe_plot (x, y, px)  // setter must be reimplemented in subclass because the whole property definition shadows the inherited one

    // left-to-right blitter-like filling algorithm
    [< Obsolete >]
    member this.scanline_fill (y1, y2, fill_px, border_px : pixel) =
        for y = y1 to y2 do
            let mutable filling = false
            for x = 0 to this.width - 1 do
                if this.[x, y].Char.UnicodeChar = border_px.Char.UnicodeChar then filling <- filling <> true
                elif filling then this.[x, y] <- fill_px 

    member this.flood_fill (x, y, fill_px) =
        if x < 0 || x >= this.width then ()
        elif y < 0 || y >= this.height then ()
        elif this.[x, y].is_empty then 
            this.[x, y] <- fill_px
            this.flood_fill (x + 1, y, fill_px)
            this.flood_fill (x, y + 1, fill_px)
            this.flood_fill (x - 1, y, fill_px)
            this.flood_fill (x, y - 1, fill_px)       

    /// Reads a pixel value at the given coordinates.
    member this.get (x, y) =
        if this.is_inside (x, y) then this.[x, y]
        else pixel.empty

    /// Blit a rectangular region of pixels from this object as source to the destination raster.
    /// Expression src.unsafe_blit (x0, y0, w, h, dst, x1, y1) blits from source raster src to destination raster dst,
    /// copying the rectangular region with top-left corner at (x0, y0) and bottom-right corner at (x0 + w - 1, y0 + h -1) to destination coordinates (x1, y1).
    abstract unsafe_blit : int * int * int * int * wronly_raster * int * int -> unit
    default src.unsafe_blit (x0, y0, w, h, dst, x1, y1) =
        // TODO: debuggare questo nuovo algoritmo di blit proposto da Davide Pizzolato
        #if OLD_BLIT
        let inline p xi yi =
            let px = src.[x0 + xi, y0 + yi]
            if not px.is_empty then dst.[x1 + xi, y1 + yi] <- px
        if obj.ReferenceEquals (src, dst) && (y1 < y0 || x1 < x0) then
            // reverse blit
            for yi = h - 1 downto 0 do
                for xi = w - 1 downto 0 do
                    p xi yi
        else
            // straight blit
            for yi = 0 to h - 1 do
                for xi = 0 to w - 1 do                    
                    p xi yi

        #else
        let inline p xi yi xOffset yOffset =
            let px = src.[x0 + xi + xOffset, y0 + yi + yOffset]
            if not px.is_empty then dst.[x1 + xi, y1 + yi] <- px 
        if y1 <= y0 || x1 <= x0 then
            for yi = 0 to h - 1 do
                for xi = 0 to w - 1 do
                    p xi yi (src.width - w) (src.height - h)
        else
            for yi = 0 to h - 1 do
                for xi = 0 to w - 1 do                    
                    p xi yi 0 0
        #endif

    member src.blit (dst : wronly_raster, x1, y1) = src.blit (0, 0, src.width, src.height, dst, x1, y1)

    member src.blit (x0, y0, w, h, dst, x1, y1) =
        let x0', y0', w0, h0 = clamp (0, 0, src.width, src.height) (x0, y0, w, h)
        let x1', y1', w1, h1 = clamp (0, 0, dst.width, dst.height) (x1, y1, w, h)
        let w', h' = min w0 w1, min h0 h1
        src.unsafe_blit (x0', y0', w', h', dst, x1', y1')
    
    override src.commit =
        src.commit_from [|
            for y = 0 to src.height - 1 do
                for x = 0 to src.width - 1 do
                    yield src.[x, y]
            |]

    member internal __.commit_from buff =
        let mutable rect = new SmallRect (0s, 0s, int16 w, int16 h)
        let handle = CreateFile ("CONOUT$", 0x40000000u, 2u, IntPtr.Zero, FileMode.Open, 0, IntPtr.Zero)
        ignore <| WriteConsoleOutput (handle, buff, new Coord (int16 w, int16 h), new Coord (0s, 0s), &rect)

    override this.ToString () =
        let sb = new StringBuilder ()
        for y = 0 to this.height - 1 do
            for x = 0 to this.width - 1 do
                ignore <| sb.Append (this.[x, y].ToString ())
        sb.ToString ()



type system_console_raster (w, h) =
    inherit wronly_raster (w, h)
    let w = min w Console.LargestWindowWidth
    let h = min h (Console.LargestWindowHeight - 1)
    do
        Console.Title <- sprintf "%s (%d x %d)" Config.game_console_title w h
        Console.CursorVisible <- false
        Console.OutputEncoding <- Text.Encoding.Unicode
        Console.SetWindowSize (w, h + 1)    // +1 on both to prevent console scrolling down after commit
        Console.SetBufferSize (w, h + 1)
        Log.msg "Console info:\n\tBufferWidth = %d\n\tBufferHeight = %d\n\tWindowWidth = %d\n\tWindowHeight = %d\n"
            Console.BufferWidth Console.BufferHeight Console.WindowWidth Console.WindowHeight
                          
    static member internal at (x, y, fg, ?bg) =
        Console.SetCursorPosition (x, y)
        Console.ForegroundColor <- fg
        Option.iter (fun bg -> Console.BackgroundColor <- bg) bg

    override __.clear = Console.Clear ()        

    override __.unsafe_plot (x, y, px) =
        system_console_raster.at (x, y, px.fg, px.bg)
        Console.Write px.Char.UnicodeChar

    override __.draw_text (s, x, y, fg, ?bg) =
        system_console_raster.at (x, y, fg, ?bg = bg)
        Console.Write s

    override __.commit = ()


// image type
//

[< Diagnostics.DebuggerDisplay("{ToString()}") >]
type image (w, h, pixels : pixel[]) =
    inherit raster (w, h)

    do assert (w * h = pixels.Length)

    new (w, h, ?px) = new image (w, h, Array.create (w * h) (defaultArg px pixel.empty))

    member val internal pixels = pixels

    override __.unsafe_get (x, y) = pixels.[y * w + x]
    override __.unsafe_plot (x, y, px) = pixels.[y * w + x] <- px
            
    override this.commit = this.commit_from pixels

    static member circle (r, px : pixel, ?filled_px) =
        let d = r * 2 + 1
        let c = d / 2
        let i = new image (d, d)
        i.draw_unfilled_circle (c, c, r, px)
        Option.iter (fun px -> i.flood_fill (c, c, px)) filled_px
        //i.draw_text (sprintf "r=%d\nd=%d\nc=%d" r d c, 0, 0, Color.Green)
        i

    static member rectangle (w, h, px, ?filled_px) =
        let i = new image (w, h)
        i.draw_rectangle (0, 0, w, h, px)
        Option.iter (fun px -> i.flood_fill (i.width / 2, i.height / 2, px)) filled_px
        i


// sprite type
//

type sprite (img : image, x_ : int, y_ : int, z_ : int) =    
    inherit image (img.width, img.height, img.pixels)
    
    member val x : float = float x_ with get, set
    member val y : float = float y_ with get, set
    member val z = z_ with get, set

    member this.move_by (dx, dy) =
        this.x <- this.x + dx
        this.y <- this.y + dy

    member this.move_by (dx, dy) = this.move_by (float dx, float dy)
    
    member spr.draw wr = spr.blit (wr, int spr.x, int spr.y)


// raster subregion
//

type region (parent : raster, x0, y0, w, h) =
    inherit raster (w, h)
    
    override __.unsafe_get (x, y) = parent.[x0 + x, y0 + y]
    override __.unsafe_plot (x, y, px) = parent.[x0 + x, y0 + y] <- px

    member val parent = parent
    
type raster with
    member this.region (x, y, w, h) = new region (this, x, y, w, h)
