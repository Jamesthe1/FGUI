namespace FGUI

open System
open System.IO
open System.Drawing
open System.Drawing.Imaging
open FSharp.NativeInterop
open OpenTK.Graphics.OpenGL
open LunarLabs.Fonts

open Utils
open Elements
open FontMgr

module TextureMgr =
    let makeAtlasRegion (x: float32) (y: float32) (width: float32) (height: float32) =
        [| x;    y
           width; height
        |]

    let makeAtlasGrid (rows: int) (columns: int) =
        let colsf = (columns |> float32)
        let rowsf = (rows |> float32)
        let cellWidth = 1f / colsf
        let cellHeight = 1f / rowsf
        
        let mutable result = Array.empty<float32[]>
        for y = 0 to rows - 1 do
            for x = 0 to columns - 1 do
                let xf = (x |> float32) / colsf
                let yf = (y |> float32) / rowsf
                result <- Array.append result [| makeAtlasRegion xf yf cellWidth cellHeight |]

        result

    /// <summary>
    /// Gets the real UV position of a polygon on an atlas map
    /// </summary>
    /// <param name="atlasMap">The map the polygon rests on</param>
    /// <param name="poly">The polygon</param>
    let getUVFromAtlas (atlasMap: float32[][]) (poly: Polygon) =
        let region = atlasMap.[poly.atlasId]
        let uvMap = poly.uvMap

        // Subtract beginning from end
        let cellWidth = region.[2]
        let cellHeight = region.[3]
        let x = (uvMap.[4] * cellWidth) + region.[0]
        let y = (uvMap.[3] * cellHeight) + region.[1]
        let width = (uvMap.[0] - uvMap.[4]) * cellWidth
        let height = (uvMap.[1] - uvMap.[3]) * cellHeight
        newUVMap x y width height

    let private bitmapObjToBitmap (data: Bitmap) =
        let bmData = data.LockBits (Rectangle (0, 0, data.Width, data.Height), ImageLockMode.ReadOnly, Imaging.PixelFormat.Format32bppArgb)
        let numBytes = bmData.Stride * bmData.Height
        arrPtrToArr<byte> (NativePtr.ofNativeInt bmData.Scan0) numBytes
        |> convertBGRAToRGBA

    let private bitmapToBitmapObj width height (data: byte[]) =
        let getPixelOfData x y =
            let pos = (y*width)+x
                      |> (*) 4
            Color.FromArgb(int data.[pos+3], int data.[pos+2], int data.[pos+1], int data.[pos])

        let bm = new Bitmap(width, height, Imaging.PixelFormat.Format32bppArgb)
        for y=0 to height-1 do
            for x=0 to width-1 do
                bm.SetPixel (x, y, getPixelOfData x y)

        bm

    let extractBitmap (data: Bitmap) =
        bitmapObjToBitmap data

    /// <summary>
    /// Tells OpenGL to use a texture handle for texture operations
    /// </summary>
    /// <param name="handle">The texture handle</param>
    let useTexture (handle: int) =
        GL.ActiveTexture TextureUnit.Texture0
        GL.BindTexture(TextureTarget.Texture2D, handle)

    /// <summary>
    /// Deletes a texture by its handle
    /// </summary>
    /// <param name="handle">The texture handle</param>
    let deleteTexture (handle: int) =
        GL.DeleteTexture handle

    let deleteTextures (handles: int[]) =
        GL.DeleteTextures(handles.Length, handles)

    /// <summary>
    /// Creates a new texture for OpenGL
    /// </summary>
    /// <param name="bitmap">The image data</param>
    /// <param name="width">Width of the image cutout</param>
    /// <param name="height">Height of the image cutout</param>
    /// <returns>A new image handle</returns>
    let newTexture (bitmap: byte[]) width height =
        let handle = GL.GenTexture()
        useTexture handle

        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat)

        let borderColor = Array.zeroCreate<float32> 4
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureBorderColor, borderColor)

        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, PixelFormat.Rgba, PixelType.UnsignedByte, bitmap)
        GL.GenerateMipmap GenerateMipmapTarget.Texture2D
        useTexture 0

        handle

    /// <summary>
    /// Creates a new texture from a path for OpenGL
    /// </summary>
    /// <param name="path">The image path</param>
    /// <param name="x">X position on the image</param>
    /// <param name="y">Y position on the image</param>
    /// <param name="width">Width of the image cutout</param>
    /// <param name="height">Height of the image cutout</param>
    /// <returns>A new image handle</returns>
    let newTexturePathed (path: string) x y width height =
        use image = new Bitmap (path)
        image.RotateFlip (RotateFlipType.RotateNoneFlipY)
        let bitmap = bitmapObjToBitmap image

        newTexture bitmap width height

    let newTextureFromObj (texture: Texture) =
        newTexturePathed texture.path texture.x texture.y texture.w texture.h