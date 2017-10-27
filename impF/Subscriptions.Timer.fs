namespace impF.Subscriptions.Timer

open impF.Subscriptions
open System.Threading
open System

type Timer<'msg> 
    ( dueTime : int
    , period : int
    , msg : int -> 'msg ) =
    inherit Sub<'msg> ()

    let toUnixTime date = 
        (date - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero)).TotalSeconds 
        |> int
    let event = 
        Event<_> ()
    let syncContext = 
        SynchronizationContext.Current
    let raiseEvent = 
        SendOrPostCallback 
            ( fun _ -> 
                DateTimeOffset.UtcNow 
                |> toUnixTime 
                |> msg 
                |> event.Trigger )
    let timer = 
        new System.Threading.Timer 
            ( fun _ -> syncContext.Post (raiseEvent, null :> obj) 
            , null :> obj
            , dueTime
            , period )

    override __.Callback = 
        event.Publish

    override __.Dispose () = 
        timer.Dispose ()

module Timer =
    let timer<'msg> dueTime period msg = 
        new Timer<'msg>(dueTime, period, msg) :> Sub<'msg>
