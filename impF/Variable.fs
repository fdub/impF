module Variables

type IVariable<'a> =
   abstract member Get : unit -> 'a
   abstract member Set : 'a -> unit
  
type Variable<'a when 'a : equality>(init : 'a, afterChanged : 'a -> unit) =
    let mutable state = init
    interface IVariable<'a> with
        member x.Get() = state
        member x.Set v =
            if v <> (state) 
            then
                state <- v
                afterChanged v

let createVariable<'a when 'a : equality>(init : 'a) (afterChanged : 'a -> unit) = 
    Variable(init, afterChanged) :>  IVariable<_> 

[<AutoOpen>]
module VarblExt=
    type IVariable<'a> with
        member x.UpdateVar<'b> (update : 'a -> 'a) =
            x.Get()
            |> update
            |> x.Set
               
        member x.UpdateVar2<'b> (update : 'a -> 'a * 'b) =
            x.Get()
            |> update
            |> fun (a, b) -> 
                x.Set a
                b