module impF.VmBase


open System.ComponentModel
open Utils
open System
open System.Windows.Input
open Variables

open impF.Effects


type PropertyChangedNotifier(sender : obj) =
    static let propChangedArgs = PropertyChangedEventArgs("V")
    let propertyChanged = Event<PropertyChangedEventHandler,PropertyChangedEventArgs>()
    member x.Trigger() = propertyChanged.Trigger(sender,propChangedArgs)
    member x.Publish = propertyChanged.Publish


type ReadOnlyField<'state when 'state : equality> 
    ( init : 'state
    , onStateChanged : 'state -> unit
    , setFromParent : IEvent<'state>
    ) as self 
    =
    let propertyChanged = PropertyChangedNotifier(self)
    let state = createVariable<'state> init (ignoreParam propertyChanged.Trigger |> chain <| onStateChanged)    
    
    do setFromParent.Add state.Set

    member x.V = state.Get()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
         member x.PropertyChanged = propertyChanged.Publish
 
let createRoField ( state : 'state) = 
    let next = Event<_>() 
    ReadOnlyField<_>(state, ignore, next.Publish), next
       

type Field<'state when 'state : equality>
    ( init : 'state
    , onStateChanged : 'state -> unit
    , setFromParent : Event<'state>
    ) =
    inherit ReadOnlyField<'state>(init, onStateChanged, setFromParent.Publish)
    member x.V 
        with get() = base.V
        and set v = setFromParent.Trigger v

let createField ( state : 'state, onStateChanged : 'state -> unit) = 
    Field<_>(state, onStateChanged, Event<_>())


type ImmutableCommand
    ( init : bool
    , execute :  unit -> unit
    , setFromParent : IEvent<bool>
    ) as self =
    let canExecuteChanged = Event<EventHandler,EventArgs>()
    let canExecute = createVariable<bool> init (fun _ -> canExecuteChanged.Trigger(self, EventArgs.Empty))   

    do setFromParent.Add canExecute.Set

    interface ICommand with
        [<CLIEvent>]
        member x.CanExecuteChanged = canExecuteChanged.Publish
        member x.CanExecute _ = canExecute.Get()
        member x.Execute _ = execute()

let always _ = true


type IVmStateManager<'msg, 'state when 'state : equality> =
    abstract member AddChildStateUpdator : ('state -> unit) -> unit 
    abstract member UpdateState : 'msg -> unit
    abstract member Get : unit -> 'state
 

type VmStateManagerParams<'state> = 
    { Init : 'state
    ; OnStateChanged : 'state -> unit
    ; SetFromParent : IEvent<'state>
    }  

type UpdateFn<'msg, 'state when 'state : equality>  = 
    | Simple of ('msg -> 'state -> 'state)
    | WithEffects of ('msg -> 'state -> 'state * Cmd<'msg>)
    

type VmStateManager<'msg, 'state when 'state : equality> 
    ( update : UpdateFn<'msg, 'state>
    , p : VmStateManagerParams<'state>
    ) = 

    let fieldUpdators = 
        ResizeArray<'state -> unit>()     
    let state = 
        createVariable<'state> p.Init ((fieldUpdators |> applyIter) |> chain <| p.OnStateChanged)        

    let rec updateStateRec msg (updateFn : 'msg -> 'state -> 'state * Cmd<'msg>) =
        let cmd = state.UpdateVar2 <| updateFn msg
        match cmd.Invoke () with
        | None -> ()
        | Some msg -> updateStateRec msg updateFn

    do p.SetFromParent.Add state.Set

    interface IVmStateManager<'msg,'state> with
        member x.Get() = 
            state.Get()  
        member x.AddChildStateUpdator u = 
            fieldUpdators.Add u
        member x.UpdateState msg = 
            match update with
            | Simple updateFn -> 
                state.UpdateVar <| updateFn msg
            | WithEffects updateFn -> 
                updateStateRec msg updateFn          
 
 
let stateManager update (p : VmStateManagerParams<'state>) = 
    VmStateManager<_,_>(update, p)

let createRootVm (factory : VmStateManagerParams<'vmState> -> 'vm) init = 
     let p = {Init = init; OnStateChanged = ignore; SetFromParent = Event<_>().Publish }
     factory p

