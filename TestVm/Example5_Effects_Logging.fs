namespace Example5_Effects_Logging

open impF.VmBase
open impF.Effects
open impF.Effects.Debug

module Logging = 
    type Msg = 
        | Message of string
        | Log

    type Model = 
        { Message : string 
        }
 
    let init =
        { Message = "" 
        }

    let update msg state = 
        match msg with 
        | Message s -> 
            { state with Message = s }, Cmd.none
        | Log -> 
            state, Debug.debug state.Message


 type LoggingVm(p) =
    let sm = 
        stateManager (WithEffects Logging.update) p

    member val Message = 
        sm.Field Logging.Msg.Message (fun m -> m.Message) 
    member val LogCommand = 
        sm.Command Logging.Msg.Log always


module VmFactory =
    let newVm () = 
        createRootVm LoggingVm Logging.init