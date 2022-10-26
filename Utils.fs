namespace FGUI

open System
open System.IO
open System.Drawing
open System.Reflection
open FSharp.NativeInterop

open OpenTK.Windowing.Desktop
open OpenTK.Graphics.OpenGL

module internal Utils =
    let iterate func arr =
        Array.iter (fun a -> func a) arr

    let throwOnNotEmpty (str: string) =
        if str.Equals ""
        then ()
        else failwith str

    let streamToArray (stream: MemoryStream) =
        // Position reset needed
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        stream.ToArray()

    let mapArr func arr =
        Array.map func arr
        |> Array.concat

    let mapArrI func arr =
        Array.mapi func arr
        |> Array.concat

    /// <summary>
    /// Offsets all data in an array by the offset
    /// </summary>
    /// <param name="offset">The offset</param>
    /// <param name="arr">The array</param>
    /// <returns>An array with offset information</returns>
    let offsetArr (offset: uint32) arr =
        Array.map (fun a -> a+offset) arr

    /// <summary>
    /// Intertwines the data of two arrays into one larger array
    /// </summary>
    /// <param name="strideA">The stride of the first array</param>
    /// <param name="strideB">The stride of the second array</param>
    /// <param name="arrA">The first array</param>
    /// <param name="arrB">The second array</param>
    /// <returns>The result of intertwining two arrays</returns>
    let intertwine strideA strideB (arrA: 'a[]) (arrB: 'a[]) =
        let mutable result = arrA
        for i in 0..(arrA.Length/strideA - 1) do
                             // Accomodate for previous merges
            let strideAPos = i * (strideA+strideB) + strideA
            let strideBPos = i * strideB
            let chunk = arrB.[strideBPos..(strideBPos+strideB-1)]

            result <- Array.insertManyAt strideAPos chunk result
        result

    let valToEnum value =
        LanguagePrimitives.EnumOfValue value

    let valsToTextureUnits start length =
        let mutable result = Array.empty<TextureUnit>
        for i in start..(start+length-1) do
            result <- Array.append result [|valToEnum i|]
        result

    let tuple2ToArray tuple =
        match tuple with
        | (a, b) -> [|a; b|]

    let byteToPct (a: byte) = float32 a / 255f

    let splitByCount (str: string) (n: int) =
        if n < 0 then failwith "The parameter n cannot be negative"
        seq { for i in 0..(str.Length/n)-1 do
                  str.Substring (i*n, min n (str.Length - i*n))
            }

    /// <summary>
    /// Nybbleshift left operator
    /// </summary>
    /// <param name="value">The number to be shifted</param>
    /// <param name="shift">The amount of bytes this must be shifted</param>
    let (<<^) value shift = value <<< (shift * 4)

    /// <summary>
    /// Nybbleshift right operator
    /// </summary>
    /// <param name="value">The number to be shifted</param>
    /// <param name="shift">The amount of bytes this must be shifted</param>
    let (>>^) value shift = value >>> (shift * 4)

    let private hexStrToByte (str: string) =
        let charToByte (ch: char) (start: char) offset = byte ch - byte start + byte offset
        str
        |> Seq.mapi (fun i c -> match c with
                                | c when c >= '0' && c <= '9' -> charToByte c '0' 0
                                | c when c >= 'a' && c <= 'f' -> charToByte c 'a' 10
                                | c when c >= 'A' && c <= 'F' -> charToByte c 'A' 10
                                | _ -> failwith $"Expected char 0-9 or a-f, got '{c}'"
                                |> fun b -> b <<^ (str.Length - 1 - i)
                    )
        |> Seq.fold (fun s b -> s ||| b) 0uy

    let hexStrToByteArr (str: string) =
        let hexLengths = [| 6; 8 |]
        if Array.forall (fun hl -> str.Length <> hl) hexLengths then    // If the string length does not match any specified values...
            let lengths = Array.map (fun (ch: int) -> ch.ToString()) hexLengths
                          |> String.concat ", "
            failwith ("Hex code must be this long, exact: " + lengths)

        splitByCount str 2
        |> Seq.map (fun s -> hexStrToByte s)
        |> Array.ofSeq

    let colorOfHexStr str i =
        (hexStrToByteArr str).[i]

    let callArrayGl<'a when 'a : unmanaged> (func: int * nativeptr<'a> -> unit) count (arr: outref<'a[]>) =
        let ptr = &&arr.[0]
        func(count, ptr)

    ///<summary>
    /// Call a function to create multiple items with one OpenGL call (i.e. GL.GenBuffers)
    ///</summary>
    let callMultiCountGl<'a when 'a : unmanaged> (func: int * nativeptr<'a> -> unit) count =
        let mutable arr = Array.zeroCreate count
        callArrayGl<'a> func count &arr
        arr

    let getClosestExpOf2 arg =
        MathF.Log2 arg
        |> MathF.Ceiling

    let getClosestPowOf2 arg =
        getClosestExpOf2 arg
        |> fun exp -> MathF.Pow(2f, exp)

    let valueOnNone option value =
        match option with
        | Some(a) -> a
        | None(_) -> value

    let valuesOnStatement statement onTrue onFalse =
        if statement then onTrue
        else onFalse

    let getBitmapSector x y xEnd yEnd bpp (bitmap: byte[]) =
        let stride = xEnd*bpp
        let pos = y*stride+(x*bpp)
        Array.zeroCreate (yEnd-y)
        |> Array.indexed
        |> Array.map (fun (i,_) -> i)
        |> Array.fold (fun s i -> Array.append s bitmap.[pos..pos+(bpp*i)-1]) Array.empty

    let getPixelOnBitmap x y width bpp (bitmap: byte[]) =
        let stride = width*bpp
        let pos = y*stride+(x*bpp)
        bitmap.[pos..pos+bpp-1]
        |> Array.map int
        |> fun cdata -> match bpp with
                        | 4 -> Color.FromArgb(cdata.[3], cdata.[2], cdata.[1], cdata.[0])
                        | 3 -> Color.FromArgb(cdata.[2], cdata.[1], cdata.[0])
                        | 1 -> Color.FromArgb(cdata.[0], cdata.[0], cdata.[0])
                        | _ -> failwith "Unsupported bytes per pixel"

    let arrPtrToArr<'a when 'a : unmanaged> (ptr: nativeptr<'a>) len =
        Array.zeroCreate<'a> len
        |> Array.mapi (fun i _ -> NativePtr.get ptr i)

    let convertBGRAToRGBA (arr: byte[]) =
        Array.splitInto (arr.Length / 4) arr
        |> Array.map (fun px -> [| px.[2]; px.[1]; px.[0]; px.[3] |])
        |> Array.concat

    let appendString a b =
        sprintf "%s%s" a b

    /// <summary>
    /// Variable in range operator
    /// </summary>
    /// <param name="x">Variable to check</param>
    /// <param name="l">Lower bound</param>
    /// <param name="r">Upper bound</param>
    let (<?<) x l r =
        l < x && x < r

    let pointInBox px py x y w h =
        let xInRange = (<?<) px x (x+w)
        let yInRange = (<?<) py (y-h) y
        xInRange && yInRange

    let pctToInt (pct: float32) size offset scale =
        (pct + offset) * scale * size
        |> int