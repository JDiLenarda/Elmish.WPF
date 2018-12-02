
#r "PresentationCore"
open System.Windows.Input

#load "Types.fs"
#load "Binding.fs"

open Elmish.WPF

[<RequireQualifiedAccess>] 
module Toggle =
    type Model = { Name:string ; Value: bool }
    type Message = | ChangeValue of bool
    let updade (ChangeValue value) model = { model with Value = value }
    let bindings () =
        [
            "CharacType" |> Binding.oneWay (fun _ -> "Toggle")
            "Name" |> Binding.oneWay (fun mdl -> mdl.Name)
            "ValueBool" |> Binding.twoWay (fun mdl -> mdl.Value) (fun v _ -> ChangeValue v)
        ]

[<RequireQualifiedAccess>]
module ItemList =
    type Model = { Name: string ; Value: string option ; Items: string list }
    type Message = | ChangeValue of string option
    let updade (ChangeValue value) model = { model with Value = value }
    let bindings () =
        [
            "CharacType" |> Binding.oneWay (fun _ -> "ItemList")
            "Name" |> Binding.oneWay (fun mdl -> mdl.Name)
            "ItemList" |> Binding.oneWay (fun mdl -> mdl.Items)
            "SelectedItem" |> Binding.twoWay (fun mdl -> mdl.Value) (fun v mdl -> ChangeValue v)
        ]

[<RequireQualifiedAccess>]
module Numeric =
    type Model = { Name: string ; Value: decimal option }
    type Message = | ChangeValue of decimal option
    let updade (ChangeValue value) model = { model with Value = value }
    let bindings () =
        [
            "CharacType" |> Binding.oneWay (fun _ -> "Numeric")
            "Name" |> Binding.oneWay (fun mdl -> mdl.Name)
            "NumericValue" |> Binding.twoWay
                (fun mdl -> mdl.Value |> Option.map string |> Option.toObj)
                (fun v mdl -> 
                    match System.Decimal.TryParse(v) with
                    | (true, x) -> ChangeValue (Some x)
                    | (false,_) ->  ChangeValue None )
        ]

module Defect =
    type CharacType =
        | Toggle of Toggle.Model
        | ItemList of ItemList.Model
        | Numeric of Numeric.Model
    
    type Model = { Name: string ;  Characteristics: CharacType list }

    type Message =
        | ChangeToggleValue of string * Toggle.Message
        | ChangeItemListValue of string * ItemList.Message
        | ChangeNumericValue of string * Numeric.Message

    let getName = function
        | Toggle mdl -> mdl.Name
        | ItemList mdl -> mdl.Name
        | Numeric mdl -> mdl.Name

    let update msg model =
        let updateCharac msg charac =
            let checkName id = id = getName charac
            match msg,charac with
            | ChangeToggleValue (id,msg'), Toggle sm ->
                if checkName id then Toggle.updade msg' sm else sm
                |> Toggle
            | ChangeItemListValue (id,msg'), ItemList sm ->
                if checkName id then ItemList.updade msg' sm else sm
                |> ItemList
            | ChangeNumericValue (id,msg'), Numeric sm ->
                if checkName id then Numeric.updade msg' sm else sm
                |> Numeric
            | _,_ -> charac

        { model with Characteristics = model.Characteristics |> List.map (updateCharac msg) }

    let bindings _ _ =
        let tryGetToggle = function | Toggle m -> Some m | _ -> None
        let tryGetItemList = function | ItemList m -> Some m | _ -> None
        //let tryGetNumeric (x:int) = None
        let tryGetNumeric  = function | Numeric m -> Some m | _ -> None
        [
            "DefectName" |> Binding.oneWay (fun m -> m.Name)
            "Characs" |> Binding.multiSubModelSeq (fun m -> m.Characteristics) getName
                      |> Binding.addSubModel tryGetToggle Toggle.bindings ChangeToggleValue
                      |> Binding.addSubModel tryGetItemList ItemList.bindings ChangeItemListValue
                      |> Binding.addSubModel tryGetNumeric Numeric.bindings ChangeNumericValue
        ]
