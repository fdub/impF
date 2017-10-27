namespace impF.Effects


type Debug<'msg> (debugMessage : string) =
    inherit Cmd<'msg> ()
    override __.Invoke () = 
        System.Diagnostics.Debug.WriteLine(debugMessage)
        None


module Debug =
    let debug debugMessage = Debug (debugMessage) :> Cmd<'msg>