namespace FGUI

open System
open System.Drawing
open LunarLabs.Fonts
open OpenTK.Windowing.Common

open Utils
open FontMgr
open InputMgr

module Elements =
    // REGION Colors

    type ColorRGB(r: float32, g, b, ?a) =
        member this.R = r
        member this.G = g
        member this.B = b
        member this.A = valueOnNone a 1f
        member this.Array = [| this.R; this.G; this.B; this.A |]

        static member Black = ColorRGB(0f, 0f, 0f)
        static member Red = ColorRGB(1f, 0f, 0f)
        static member Yellow = ColorRGB(1f, 1f, 0f)
        static member Green = ColorRGB(0f, 1f, 0f)
        static member Teal = ColorRGB(0f, 1f, 1f)
        static member Blue = ColorRGB(0f, 0f, 1f)
        static member Purple = ColorRGB(1f, 0f, 1f)

    type ColorRGBByte(r, g, b, ?a) =
        inherit ColorRGB(byteToPct r, byteToPct g, byteToPct b, valueOnNone a 255uy
                                                                |> byteToPct)

    type ColorRGBString(hex) =
        inherit ColorRGBByte(colorOfHexStr hex 0, colorOfHexStr hex 1, colorOfHexStr hex 2, appendString hex "ff"
                                                                                            |> fun hex8 -> colorOfHexStr hex8 3
                            )
    
    // REGION Polys

    type Polygon =
        { verts:   float32[]
          uvMap:   float32[]
          indices: uint32[]
          atlasId: int
          texId: int }

    type Texture =
        { path: string
          x:    int
          y:    int
          w:    int
          h:    int
          atlas: float32[][] }

    let newUVMap x y w h =
        let xEnd = x+w
        let yEnd = y+h
        [| xEnd; yEnd
           xEnd; y
           x;    y
           x;    yEnd
        |]

    let newQuad x y w h z uvx uvy uvw uvh atlasId texId =
        let xEnd = x+w
        let yEnd = y+h
        {
          verts = [| xEnd; yEnd; z
                     xEnd; y;    z
                     x;    y;    z
                     x;    yEnd; z
                  |]
          uvMap = newUVMap uvx uvy uvw uvh
          indices = [| 0u; 1u; 3u
                       1u; 2u; 3u
                    |]
          atlasId = atlasId
          texId = texId
        }

    let newStretchedQuad x y w h z sliceX sliceY atlasId texId =
        let realSliceX = sliceX * w
        let realSliceY = sliceY * h

        let widthMid = w - (realSliceX * 2f)
        let heightMid = h - (realSliceY * 2f)

        let uvw = 1f / 3f;
        let uvh = 1f / 3f;

        // Slice chunk order: 0 1 0
        //                    2 3 2
        //                    0 1 0
        let widthHeights = [| realSliceX, realSliceY
                              widthMid, realSliceY
                              realSliceX, heightMid
                              widthMid, heightMid
                           |]

        let xBegin = [| x; x+realSliceX; x+realSliceX+widthMid  |]
        let yBegin = [| y; y+realSliceY; y+realSliceY+heightMid |]

        let mutable result = Array.empty
        for yCell in 0..2 do
            let yStart = yBegin.[yCell]
            let yCellf = yCell |> float32
            let whStart = (yCell*2) % 4
            for xCell in 0..2 do
                let xStart = xBegin.[xCell]
                let xCellf = xCell |> float32
                let wh = widthHeights.[whStart+(xCell%2)]
                         |> tuple2ToArray
                result <- Array.append result [| newQuad xStart yStart wh.[0] wh.[1] z (uvw*xCellf) (uvh*yCellf) uvw uvh atlasId texId |]

        result

    let pointInWindowBox (px: int) (py: int) (x: float32) (y: float32) w h windW windH =
        let (%^)  p size = pctToInt  p size 1f 0.5f // Percent goes up to px on window, hence percent-up
        let (%^-) p size = pctToInt -p size 1f 0.5f // Same as above, except p is flipped
        let (%^.) p size = pctToInt  p size 0f 0.5f // Same as first, no offset
        let windWf = float32 windW
        let windHf = float32 windH
        let xReal = x %^  windWf
        let yReal = y %^- windHf
        let wReal = w %^. windWf
        let hReal = h %^. windHf
        pointInBox px py xReal yReal wReal hReal

    type IElement =
        abstract member Polys: Polygon[]
        abstract member PointInBounds: int -> int -> int -> int -> bool
        abstract member GUID: Guid

    type IInteractiveElement =
        inherit IElement
        abstract member HoverPolys: Polygon[]
        abstract member DisabledPolys: Polygon[]
        abstract member SetOnClick: (MouseButtonEventArgs -> unit) -> IInteractiveElement
        abstract member CallOnClick: MouseButtonEventArgs -> unit
        abstract member Hovered: bool with get, set
        abstract member Disabled: bool with get, set

    [<AbstractClass>]
    type ElementBase(x, y, w, h) =
        let guid = Guid.NewGuid ()
        abstract member Polys: Polygon[]
        interface IElement with
            member this.PointInBounds px py windW windH = pointInWindowBox px py x y w h windW windH
            member this.GUID = guid
            member this.Polys = this.Polys

    type BaseQuad(x, y, w, h, z, sliceX, sliceY, atlasId, texId) =
        inherit ElementBase(x, y, w, h)
        override this.Polys = newStretchedQuad x y w h z sliceX sliceY atlasId texId

    type ButtonQuad(x, y, w, h, z, sliceX, sliceY, idleId, hoverId, disableId, texId) =
        inherit BaseQuad(x, y, w, h, z, sliceX, sliceY, idleId, texId)
        let mutable func = fun (i: MouseButtonEventArgs) -> ()
        let mutable hovered = false
        let mutable disabled = false

        let quad = newStretchedQuad x y w h z sliceX sliceY
        interface IInteractiveElement with
            member this.HoverPolys = quad hoverId texId
            member this.DisabledPolys = quad disableId texId
            member this.SetOnClick f = func <- f
                                       this :> IInteractiveElement
            member this.CallOnClick input = func input
            member this.Hovered
                       with get() = hovered
                       and set state = hovered <- state
            member this.Disabled
                       with get() = disabled
                       and set state = disabled <- state

    // REGION Text

    let newTextQuads x y w h z (text: string) charset texId =
        [| for i = 0 to text.Length-1 do
               let c = text.[i]
               let cData = Array.find (fun cd -> cd.id = c) charset
               let xOff = float32 (cData.glyph.xOfs + cData.glyph.Image.Width * i)
               let yOff = float32 (cData.glyph.Image.Height - cData.glyph.yOfs)
               let (uvx, uvy, uvw, uvh) = cData.uv
               newQuad (x + xOff) (y + yOff) w h z uvx uvy uvw uvh (int c) texId
        |]

    type ITextElement =
        inherit IElement
        abstract member TextPolys: Polygon[]

    type Label(x, y, w, h, z, text, charset, texId) =
        inherit ElementBase(x, y, w, h)
        override this.Polys = Array.empty
        interface ITextElement with
            member this.TextPolys = newTextQuads x y w h z text charset texId

    type LinkLabel(x, y, w, h, z, text, charset, texId) =
        inherit Label(x, y, w, h, z, text, charset, texId)
        let mutable func = fun (i: MouseButtonEventArgs) -> ()
        let mutable hovered = false
        let mutable disabled = false

        interface IInteractiveElement with
            member this.HoverPolys = Array.empty
            member this.DisabledPolys = Array.empty
            member this.SetOnClick f = func <- f
                                       this :> IInteractiveElement
            member this.CallOnClick input = func input
            member this.Hovered
                       with get() = hovered
                       and set state = hovered <- state
            member this.Disabled
                       with get() = disabled
                       and set state = disabled <- state

    type LabelButton(x, y, w, h, z, sliceX, sliceY, idleId, hoverId, disableId, texId, text, charset, fontTexId) =
        inherit ButtonQuad(x, y, w, h, z, sliceX, sliceY, idleId, hoverId, disableId, texId)
        interface ITextElement with
            member this.TextPolys = newTextQuads x y w h z text charset fontTexId