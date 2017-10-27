namespace Example6_Subscriptions


open impF.VmBase
open impF.Subscriptions
open impF.Subscriptions.Timer


module Subscriptions = 
    type Msg = 
        | TickSeconds of int
        | TickTenth of int

    type Model = 
        { Seconds : int
        ; Tenth : int
        }
 
    let init =
        { Seconds = 0
        ; Tenth = 0
        }

    let update msg state = 
        match msg with 
        | TickSeconds _ ->
            { state with Seconds = state.Seconds + 1 }
        | TickTenth _ ->
            { state with Tenth = (state.Tenth + 1) % 10 }

    let subscription = 
        Sub.batch
            [ Timer.timer 0 1000 TickSeconds
            ; Timer.timer 0 100 TickTenth 
            ]


type LoggingVm(p) =
    let sm = 
        stateManagerWithSubcription (Simple Subscriptions.update) p Subscriptions.subscription

    member val Seconds = 
        sm.RoField (fun m -> m.Seconds) 
    member val Tenth = 
        sm.RoField (fun m -> m.Tenth) 


module VmFactory =
    let newVm () = 
        createRootVm LoggingVm Subscriptions.init