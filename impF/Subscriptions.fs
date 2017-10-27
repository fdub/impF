namespace impF.Subscriptions


open System


[<AbstractClass>]
type Sub<'msg> () =
    abstract Dispose : unit -> unit
    abstract Callback : IEvent<'msg>
    interface IDisposable with
        member this.Dispose () = this.Dispose ()


type Batch<'msg> (children : Sub<'msg> list) =
    inherit Sub<'msg> ()

    let event = Event<'msg> ()

    let subscriptions = 
        children 
        |> List.map (fun c -> c.Callback.Subscribe (event.Trigger))

    override __.Callback = event.Publish

    override __.Dispose () = 
        subscriptions
        |> List.iter (fun s -> s.Dispose ())
        children 
        |> List.iter (fun c -> c.Dispose ())


module Sub =
    let batch children = new Batch<'msg> (children)