namespace FGUI

open OpenTK.Windowing.Common

module InputMgr =
    type InputState =
        | InputDown = 1
        | InputUp = 0

    type InputType =
        | KeyboardEvent of KeyboardKeyEventArgs
        | MouseButtonEvent of MouseButtonEventArgs
        | MouseWheelEvent of MouseWheelEventArgs
        | MouseMoveEvent of MouseMoveEventArgs

    type InputEvent(state: InputState, inputType: InputType) =
        member this.State = state
        member this.InputType = inputType
    
    type InputManager() =
        static let event = new Event<InputEvent> ()

        [<CLIEventAttribute>]
        static member OnInputEvent = event.Publish

        static member OnKeyPushed (key: KeyboardKeyEventArgs) =
            InputEvent(InputState.InputDown, KeyboardEvent (key))
            |> event.Trigger

        static member OnKeyLifted (key: KeyboardKeyEventArgs) =
            InputEvent(InputState.InputUp, KeyboardEvent (key))
            |> event.Trigger

        static member OnMouseClick (button: MouseButtonEventArgs) =
            InputEvent(InputState.InputDown, MouseButtonEvent (button))
            |> event.Trigger

        static member OnMouseLift (button: MouseButtonEventArgs) =
            InputEvent(InputState.InputUp, MouseButtonEvent (button))
            |> event.Trigger

        static member OnMouseScroll (wheel: MouseWheelEventArgs) =
            InputEvent(InputState.InputDown, MouseWheelEvent (wheel))
            |> event.Trigger

        static member OnMouseMove (mouse: MouseMoveEventArgs) =
            InputEvent(InputState.InputDown, MouseMoveEvent (mouse))
            |> event.Trigger