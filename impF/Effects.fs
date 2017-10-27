namespace impF.Effects


[<AbstractClass>]
type Cmd<'msg> () =
    abstract Invoke : unit -> 'msg option 


type CmdNone<'msg> () =
    inherit Cmd<'msg> ()
    override __.Invoke () = 
        None


module Cmd =
    let none<'msg> = 
        CmdNone<'msg> () :> Cmd<'msg>
