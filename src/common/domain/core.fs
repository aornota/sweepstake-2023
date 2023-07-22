module Aornota.Sweepstake2023.Common.Domain.Core

open System

type SessionId = | SessionId of guid : Guid with static member Create () = Guid.NewGuid () |> SessionId

type Group = | GroupA | GroupB | GroupC | GroupD

type DraftOrdinal = | DraftOrdinal of draftOrdinal : int

let groups = [ GroupA ; GroupB ; GroupC ; GroupD ]

let groupText group =
    let groupText = match group with | GroupA -> "A" | GroupB -> "B" | GroupC -> "C" | GroupD -> "D"
    sprintf "Group %s" groupText
