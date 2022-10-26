namespace FGUI

open System
open System.ComponentModel
open OpenTK.Graphics.OpenGL
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.Common
open OpenTK.Windowing.GraphicsLibraryFramework

open WindowMgr
open Utils
open ShaderMgr
open TextureMgr
open Elements
open InputMgr
open FontMgr

module RuntimeMgr =
    let private initBuffer target (bufferHandle: int) (buffer: 'a[]) (size: int) usage =
        GL.BindBuffer(target, bufferHandle)
        // Sets data into buffer
        GL.BufferData(target, size, buffer, usage)

    let private initBuffers vbuf ebuf verts indices =
        initBuffer BufferTarget.ArrayBuffer vbuf verts (verts.Length * sizeof<float32>) BufferUsageHint.StaticDraw
        initBuffer BufferTarget.ElementArrayBuffer ebuf indices (indices.Length * sizeof<uint32>) BufferUsageHint.StaticDraw

    // Requires a vertex array be bound to OpenGL
    let private makeVertexPointers () =
        GL.VertexAttribPointer(0, 3, VertexAttribPointerType.Float, false, 5 * sizeof<float32>, 0)
        GL.EnableVertexAttribArray 0

        GL.VertexAttribPointer(1, 2, VertexAttribPointerType.Float, false, 5 * sizeof<float32>, 3 * sizeof<float32>)
        GL.EnableVertexAttribArray 1
    
    let private loadCall (vbufs: int[]) (vobjs: int[]) (ebufs: int[]) (handle: int) (color: ColorRGB) (verts: float32[]) (indices: uint32[]) () =   // Parenthesis at end to prevent using the function when passing it
        GL.ClearColor(color.R, color.G, color.B, color.A)
        for i = 0 to (vobjs.Length-1) do
            let vobj = vobjs.[i]
            let vbuf = vbufs.[i]
            let ebuf = ebufs.[i]
            GL.BindVertexArray vobj
            initBuffers vbuf ebuf verts indices
            makeVertexPointers()
        GL.BindVertexArray vobjs.[0]    // Reset so that we don't get any weirdness with OpenGL

        GL.UseProgram handle
        setUniformInt handle "texture0" 0

    let private resizeCall (args: ResizeEventArgs) =
        GL.Viewport(0, 0, args.Width, args.Height)

    let private updateCall (vbufs: int[]) (vobjs: int[]) (wind: GameWindow) (handle: int) (textures: int[]) (indices: uint32[]) (args: FrameEventArgs) =
        GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
        
        for i = 0 to vobjs.Length - 1 do
            let vobj = vobjs.[i]
            let vbuf = vbufs.[i]
            let texture = textures.[i]
            GL.BindBuffer(BufferTarget.ArrayBuffer, vbuf)
            GL.UseProgram handle
            useTexture texture

            GL.BindVertexArray vobj
            GL.DrawElements(PrimitiveType.Triangles, indices.Length, DrawElementsType.UnsignedInt, 0)
        
        wind.SwapBuffers ()

    let private unloadCall (vbufs: int[]) handle textures (_: CancelEventArgs) =
        GL.BindBuffer(BufferTarget.ArrayBuffer, 0)
        GL.DeleteBuffers(vbufs.Length, vbufs)
        deleteProgram handle
        deleteTextures textures

    let private eventHandler (elements: IElement[]) wind (event: InputEvent) =
        let (windW, windH) = getWindowSize wind
        let pos = wind.MousePosition
        let (x, y) = int pos.X, int pos.Y
        match event.InputType with
        | MouseButtonEvent e -> if event.State = InputState.InputUp then
                                    Array.iter (fun (el: IElement) -> if el.PointInBounds x y windW windH then
                                                                        if el :? IInteractiveElement then
                                                                            (el :?> IInteractiveElement).CallOnClick e
                                                ) elements
        | MouseMoveEvent e -> Array.filter (fun (el: IElement) -> el :? IInteractiveElement) elements
                              |> Array.map (fun el -> el :?> IInteractiveElement)
                              |> Array.iter (fun iel -> iel.Hovered <- iel.PointInBounds x y windW windH)
        | _ -> ()

    /// <summary>
    /// Creates a new window. To create a window without additional calls, use <see cref="loadGraphicsShort" />
    /// </summary>
    /// <param name="title">The title of the window</param>
    /// <param name="width">The width of the window</param>
    /// <param name="height">The height of the window</param>
    /// <param name="icons">The window's icons</param>
    /// <param name="backgroundColor">The color for the background</param>
    /// <param name="textures">The textures used</param>
    /// <param name="fonts">The fonts the program will use</param>
    /// <param name="elements">An array of elements</param>
    /// <param name="loadCalls">Additional calls on window load</param>
    /// <param name="resizeCalls">Additional calls on resize</param>
    /// <param name="updateCalls">Additional calls on frame updates</param>
    /// <param name="unloadCalls">Additional calls on unload</param>
    /// <param name="inputCalls">Calls on input</param>
    /// <returns>A new <see cref="OpenTK.Windowing.Desktop.GameWindow" /></returns>
    let loadGraphics title width height icons backgroundColor textures fonts elements loadCalls resizeCalls updateCalls unloadCalls inputCalls =
        let texCount = Array.length<Texture> textures

        let wind = newWindow title width height icons
        let vbufs = callMultiCountGl<int> GL.GenBuffers texCount

        let vobjs = callMultiCountGl<int> GL.GenVertexArrays texCount
        let program = newProgram "shaders/shader.vert" "shaders/shader.frag"
        let ebufs = callMultiCountGl<int> GL.GenBuffers texCount
        let textureIds = Array.map newTextureFromObj textures

        let polys = mapArr (fun (e: IElement) -> e.Polys) elements

                                           // Intertwine takes vertices (vector3) and uvs (vector2), then combines them so that OpenGL properly reads them
        let vertsAndUvs = mapArr (fun (p: Polygon) -> getUVFromAtlas textures.[p.texId].atlas p
                                                      |> intertwine 3 2 p.verts
                                 ) polys
        let indices = mapArrI (fun i p -> offsetArr (uint32 (i*4)) p.indices) polys

        wind.add_Load (Action(loadCall vbufs vobjs ebufs program backgroundColor vertsAndUvs indices))
        wind.add_Resize (Action<ResizeEventArgs>(resizeCall))
        wind.add_UpdateFrame (Action<FrameEventArgs>(updateCall vbufs vobjs wind program textureIds indices))
        wind.add_Closing (Action<CancelEventArgs>(unloadCall vbufs program textureIds))
        
        iterate wind.add_Load loadCalls
        iterate wind.add_Resize resizeCalls
        iterate wind.add_UpdateFrame updateCalls
        iterate wind.add_Closing unloadCalls

        InputManager.OnInputEvent.Add (eventHandler elements wind)
        iterate InputManager.OnInputEvent.Add inputCalls
        wind.add_KeyDown (Action<KeyboardKeyEventArgs>(InputManager.OnKeyPushed))
        wind.add_KeyUp (Action<KeyboardKeyEventArgs>(InputManager.OnKeyLifted))
        wind.add_MouseDown (Action<MouseButtonEventArgs>(InputManager.OnMouseClick))
        wind.add_MouseUp (Action<MouseButtonEventArgs>(InputManager.OnMouseLift))
        wind.add_MouseWheel (Action<MouseWheelEventArgs>(InputManager.OnMouseScroll))
        wind.add_MouseMove (Action<MouseMoveEventArgs>(InputManager.OnMouseMove))

        wind
        
    /// <summary>
    /// Similar to <see cref="loadGraphicsShort" /> with some customization.
    /// </summary>
    /// <param name="title">The title of the window</param>
    /// <param name="width">The width of the window</param>
    /// <param name="height">The height of the window</param>
    /// <param name="icons">The window's icons</param>
    /// <param name="backgroundColor">The color for the background</param>
    /// <param name="textures">The textures used</param>
    /// <param name="fonts">The fonts the program will use</param>
    /// <param name="polys">An array of polygons</param>
    /// <param name="inputCalls">Calls on input</param>
    /// <returns>A new <see cref="OpenTK.Windowing.Desktop.GameWindow" /></returns>
    let loadGraphicsShortStyled title width height icons backgroundColor textures fonts polys inputCalls =
        loadGraphics title width height icons backgroundColor textures fonts polys Array.empty Array.empty Array.empty Array.empty inputCalls

    /// <summary>
    /// Creates a new window, shorter than <see cref="loadGraphics" />. Easiest setup.
    /// </summary>
    /// <param name="title">The title of the window</param>
    /// <param name="width">The width of the window</param>
    /// <param name="height">The height of the window</param>
    /// <param name="polys">An array of polygons</param>
    /// <param name="inputCalls">Calls on input</param>
    /// <returns>A new <see cref="OpenTK.Windowing.Desktop.GameWindow" /></returns>
    let loadGraphicsShort title width height polys inputCalls =
        let uiTexture = { path = "UI/default.png"; x = 0; y = 0; w = 128; h = 128; atlas = makeAtlasGrid 2 2 }
        let icons = [| "icons/icon.png" |]
        let font = loadFont "fonts/default.ttf" 20f
        loadGraphicsShortStyled title width height icons ColorRGB.Black [| uiTexture |] [| font |] polys inputCalls

    /// <summary>
    /// Runs a window on the current thread.
    /// </summary>
    /// <param name="wind">A <see cref="OpenTK.Windowing.Desktop.GameWindow" /> from <see cref="loadGraphics" /> or <see cref="loadGraphicsShort" /></param>
    let runWindow (wind: GameWindow) =
        wind.Run()  // Until the window is closed, the thread will be stuck at this function