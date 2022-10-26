namespace FGUI

open System.IO
open System.Text
open OpenTK.Graphics.OpenGL
open Utils

module ShaderMgr =
    let private compileShader shader (shaderPath: string) =
        let shader_src = (new StreamReader(shaderPath, Encoding.UTF8))
                             .ReadToEnd()

        GL.ShaderSource(shader, shader_src)
        GL.CompileShader shader
        GL.GetShaderInfoLog shader
        |> throwOnNotEmpty

    let private loadShader handle (shaderPath: string) shaderType =
        let shader = GL.CreateShader shaderType
        compileShader shader shaderPath
        GL.AttachShader(handle, shader)
        shader

    let private detatchShader (handle: int) shader =
        GL.DetachShader(handle, shader)
        GL.DeleteShader shader

    let private detatchShaders handle shaders =
        iterate (detatchShader handle) shaders

    /// <summary>
    /// Sets a uniform's value in a shader program.
    /// </summary>
    /// <param name="handle">The program handle from <see cref="newProgram" /></param>
    /// <param name="name">The name of the uniform</param>
    /// <param name="value">The new value of the uniform</param>
    let setUniformInt (handle: int) name (value: int) =
        let location = GL.GetUniformLocation(handle, name)
        GL.Uniform1(location, value)

    /// <summary>
    /// Tells OpenGL to delete a handle.
    /// </summary>
    /// <param name="handle">The program handle from <see cref="newProgram" /></param>
    let deleteProgram (handle: int) =
        GL.DeleteProgram handle

    let getAttributeLocation (handle: int) location =
        GL.GetAttribLocation(handle, location)

    /// <summary>
    /// Creates a new program handle. Must be used with <see cref="useProgram"/> and deleted with <see cref="deleteProgram" />.
    /// </summary>
    /// <param name="vertexPath">Path to vertex shader</param>
    /// <param name="fragmentPath">Path to fragment shader</param>
    /// <returns>An int representing a program handle</returns>
    let newProgram vertexPath fragmentPath =
        let handle = GL.CreateProgram()
        let vshader = loadShader handle vertexPath ShaderType.VertexShader
        let fshader = loadShader handle fragmentPath ShaderType.FragmentShader

        GL.LinkProgram handle
        GL.GetProgramInfoLog handle
        |> throwOnNotEmpty

        detatchShaders handle [| vshader; fshader |]
        handle