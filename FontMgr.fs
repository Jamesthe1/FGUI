namespace FGUI

open System.IO
open LunarLabs.Fonts
open Utils

module FontMgr =
    type FontType = {
        font: Font
        scale: float32
    } with static member empty = { font = null; scale = -1f }

    type CharData = {
        id: char
        font: FontType
        glyph: FontGlyph
        uv: float32 * float32 * float32 * float32
    } with static member empty = { id = '\x00'
                                   font = FontType.empty
                                   glyph = null
                                   uv = 0f, 0f, 0f, 0f
                                 }

    /// <summary>
    /// Loads a font into memory.
    /// </summary>
    /// <param name="path">The location of the file</param>
    /// <param name="pixSize">The size of the font, in pixels</param>
    /// <returns>A FontType</returns>
    let loadFont (path: string) pixSize =
        let font = File.ReadAllBytes (path)
                   |> fun data -> Font(data)
        let scale = font.ScaleInPixels pixSize
        { font = font; scale = scale }

    let rasterize (fontType: FontType) (text: string) =
        text
        |> Seq.map (fun c -> fontType.font.RenderGlyph(c, fontType.scale)
                             |> fun g -> { CharData.empty with
                                             id = c
                                             font = fontType
                                             glyph = g
                                         }
                   )
        |> Seq.filter (fun cd -> cd.glyph <> null)
        |> Seq.toArray

    let rasterizeArray fontType (arr: char[]) =
        arr
        |> Array.map (fun c -> string c)
        |> String.concat ""
        |> rasterize fontType

    let rasterizeAllChars fontType =
        rasterizeArray fontType ([|'\x00'..'\x80'|])    // 0..128