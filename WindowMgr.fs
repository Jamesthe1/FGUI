namespace FGUI

open System.Drawing

open TextureMgr

open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.Common.Input
open OpenTK.Mathematics

module WindowMgr =
    let screenSize (window: NativeWindow) =
        let monitor = Monitors.GetMonitorFromWindow window
        if monitor <> null
        then [| monitor.HorizontalResolution
                monitor.VerticalResolution
             |]
        else
            [| 0; 0 |]
    
    let private getWindowBegin windHandle width height =
        let xy = screenSize windHandle
        new Vector2i(xy.[0] - width, xy.[1] - height) / 2

    let private makeIconImage (path: string) =
        use icon = new Bitmap(path)
        let iconRGBA = extractBitmap icon
        Image(icon.Width, icon.Height, iconRGBA)

    let private makeIconImages =
        Array.map makeIconImage

    /// <summary>
    /// Creates a new <see cref="OpenTK.Windowing.Desktop.GameWindow" />
    /// </summary>
    /// <param name="title">The window title</param>
    /// <param name="width">The window width</param>
    /// <param name="height">The window height</param>
    /// <param name="icons">The window's icon paths</param>
    let newWindow title width height icons =
        let windSet = NativeWindowSettings.Default
        windSet.Title <- title
        windSet.IsEventDriven <- true
        windSet.Size <- new Vector2i(width, height)
        windSet.WindowBorder <- WindowBorder.Fixed

        let winIcon = WindowIcon(makeIconImages icons)
        windSet.Icon <- winIcon
    
        let wind = new GameWindow(GameWindowSettings.Default, windSet)
        wind.Location <- getWindowBegin wind width height
        wind

    /// <summary>
    /// Gets the window's dimensions on the screen
    /// </summary>
    /// <param name="wind">The window size</param>
    let getWindowSize (wind: NativeWindow) =
        wind.Size.X, wind.Size.Y