namespace impF.Effects


open System.Diagnostics


type Debug<'msg> 
    ( debugMessage : string
    ) =
    inherit Cmd<'msg> ()
    override __.Invoke () = 
        Debug.WriteLine (debugMessage)
        None


module Debug =
    let debug<'msg> debugMessage = 
        Debug (debugMessage) :> Cmd<'msg>