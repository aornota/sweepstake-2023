module Aornota.Sweepstake2023.Ui.Pages.Fixtures.Common

open Aornota.Sweepstake2023.Common.Domain.Core
open Aornota.Sweepstake2023.Common.Domain.Fixture
open Aornota.Sweepstake2023.Common.Domain.Squad
open Aornota.Sweepstake2023.Common.WsApi.ServerMsg
open Aornota.Sweepstake2023.Common.WsApi.UiMsg
open Aornota.Sweepstake2023.Ui.Common.Notifications
open Aornota.Sweepstake2023.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2023.Ui.Shared

type FixturesFilter =
    | AllFixtures
    | GroupFixtures of group : Group option
    | KnockoutFixtures
    | Fixture of fixtureId : FixtureId

type ConfirmParticipantInput =
    | SquadSelected of squadIdJson : string
    | ConfirmConfirmParticipant
    | CancelConfirmParticipant

type KickType = | KickSuccessful | KickMissed

type AddMatchEventInput =
    | PlayerSelected of playerIdJson : string
    | KickTypeChanged of kickType : KickType
    | CardSelected of card : Card
    | AddMatchEvent
    | CancelAddMatchEvent

type RemoveMatchEventInput =
    | RemoveMatchEvent
    | CancelRemoveMatchEvent

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerFixturesMsg of serverFixturesMsg : ServerFixturesMsg
    | ShowAllFixtures
    | ShowGroupFixtures of group : Group option
    | ShowKnockoutFixtures
    | ShowFixture of fixtureId : FixtureId
    | ShowConfirmParticipantModal of fixtureId : FixtureId * role : Role * unconfirmed : Unconfirmed
    | ConfirmParticipantInput of confirmParticipantInput : ConfirmParticipantInput
    | ShowAddTryModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddPenaltyTryModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddPenaltyKickModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddConversionModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddDropGoalModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddCardModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddManOfTheMatchModal of fixtureId : FixtureId * squadId : SquadId
    | AddMatchEventInput of addMatchEventInput : AddMatchEventInput
    | ShowRemoveMatchEventModal of fixtureId : FixtureId * matchEventId : MatchEventId * matchEvent : MatchEvent
    | RemoveMatchEventInput of removeMatchEventInput : RemoveMatchEventInput

type ConfirmParticipantStatus =
    | ConfirmParticipantPending
    | ConfirmParticipantFailed of errorText : string

type ConfirmParticipantState = {
    FixtureId : FixtureId
    Role : Role
    Unconfirmed : Unconfirmed
    SquadId : SquadId option
    ConfirmParticipantStatus : ConfirmParticipantStatus option }

type AddMatchEvent =
    | TryEvent of playerId : PlayerId option
    | PenaltyTryEvent
    | PenaltyKickEvent of playerId : PlayerId option * kickType : KickType option
    | ConversionEvent of playerId : PlayerId option * kickType : KickType option
    | DropGoalEvent of playerId : PlayerId option
    | CardEvent of playerId : PlayerId option * card : Card option
    | ManOfTheMatchEvent of playerId : PlayerId option

type AddMatchEventStatus =
    | AddMatchEventPending
    | AddMatchEventFailed of errorText : string

type AddMatchEventState = {
    FixtureId : FixtureId
    SquadId : SquadId
    AddMatchEvent : AddMatchEvent
    AddMatchEventStatus : AddMatchEventStatus option }

type RemoveMatchEventStatus =
    | RemoveMatchEventPending
    | RemoveMatchEventFailed of errorText : string

type RemoveMatchEventState = {
    FixtureId : FixtureId
    MatchEventId : MatchEventId
    MatchEvent : MatchEvent
    RemoveMatchEventStatus : RemoveMatchEventStatus option }

type State = {
    CurrentFixturesFilter : FixturesFilter
    LastGroup : Group option
    ConfirmParticipantState : ConfirmParticipantState option
    AddMatchEventState : AddMatchEventState option
    RemoveMatchEventState : RemoveMatchEventState option }

let unconfirmedText unconfirmed =
    match unconfirmed with
    | StageWinner (Group group) -> sprintf "%s winner" (group |> groupText)
    | StageWinner (QuarterFinal quarterFinalOrdinal) -> sprintf "Quarter-final %i winner" quarterFinalOrdinal
    | StageWinner (SemiFinal semiFinalOrdinal) -> sprintf "Semi-final %i winner" semiFinalOrdinal
    | StageWinner (BronzeFinal) | StageWinner (Final) -> SHOULD_NEVER_HAPPEN
    | GroupRunnerUp group -> sprintf "%s runner-up" (group |> groupText)
    | SemiFinalLoser semiFinalOrdinal -> sprintf "Semi-final %i loser" semiFinalOrdinal

let homeAndAway (squadDic:SquadDic) fixture =
    let homeParticipant, awayParticipant = fixture.HomeParticipant, fixture.AwayParticipant
    let home =
        match homeParticipant with
        | Confirmed squadId ->
            let (SquadName squadName) = squadId |> squadName squadDic
            squadName
        | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
    let away =
        match awayParticipant with
        | Confirmed squadId ->
            let (SquadName squadName) = squadId |> squadName squadDic
            squadName
        | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
    home, away

let fixtureText (fixtureDic:FixtureDic) (squadDic:SquadDic) fixtureId =
    if fixtureId |> fixtureDic.ContainsKey then
        let home, away = fixtureDic.[fixtureId] |> homeAndAway squadDic
        sprintf "%s vs. %s" home away |> Some
    else None

let matchEventText (squadDic:SquadDic) matchEvent =
    match matchEvent with
    | Try (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Try scored by %s" playerName
    | PenaltyTry _ ->
        "Penalty try"
    | PenaltyKick (squadId, playerId, Successful) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Penalty kicked by %s" playerName
    | PenaltyKick (squadId, playerId, Missed) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Penalty missed by %s" playerName
    | Conversion (squadId, playerId, Successful) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Conversion kicked by %s" playerName
    | Conversion (squadId, playerId, Missed) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Conversion missed by %s" playerName
    | DropGoal (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Drop goal kicked by %s" playerName
    | YellowCard (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Yellow card for %s" playerName
    | RedCard (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Red card for %s" playerName
    | ManOfTheMatch (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Man-of-the-match for %s" playerName
