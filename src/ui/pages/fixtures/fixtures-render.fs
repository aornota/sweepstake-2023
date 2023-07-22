module Aornota.Sweepstake2023.Ui.Pages.Fixtures.Render

open Aornota.Sweepstake2023.Common.Domain.Core
open Aornota.Sweepstake2023.Common.Domain.User
open Aornota.Sweepstake2023.Common.Domain.Fixture
open Aornota.Sweepstake2023.Common.UnitsOfMeasure
open Aornota.Sweepstake2023.Ui.Common.JsonConverter
open Aornota.Sweepstake2023.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2023.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2023.Ui.Common.TimestampHelper
open Aornota.Sweepstake2023.Ui.Pages.Fixtures.Common
open Aornota.Sweepstake2023.Ui.Render.Bulma
open Aornota.Sweepstake2023.Ui.Render.Common
open Aornota.Sweepstake2023.Ui.Shared
open Aornota.Sweepstake2023.Ui.Theme.Common
open Aornota.Sweepstake2023.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2023.Ui.Theme.Shared
open Aornota.Sweepstake2023.Common.Domain.Squad // note: after Aornota.Sweepstake2023.Ui.Render.Bulma to avoid collision with Icon.Forward

open System

module RctH = Fable.React.Helpers

let private possibleParticipants unconfirmed (fixtureDic:FixtureDic) (squadDic:SquadDic) =
    match unconfirmed with
    | StageWinner (Group group) | GroupRunnerUp group ->
        squadDic |> List.ofSeq |> List.choose (fun (KeyValue (squadId, squad)) -> if squad.Group = group then squadId |> Some else None)
    | StageWinner (QuarterFinal quarterFinalOrdinal) ->
        fixtureDic |> List.ofSeq |> List.map (fun (KeyValue (_, fixture)) ->
            match fixture.Stage with
            | QuarterFinal otherOrdinal when otherOrdinal = quarterFinalOrdinal ->
                match fixture.HomeParticipant, fixture.AwayParticipant with
                | Confirmed homeSquadId, Confirmed awaySquadId -> [ homeSquadId ; awaySquadId ]
                | _ -> []
            | _ -> [])
            |> List.collect id
    | StageWinner (SemiFinal semiFinalOrdinal) | SemiFinalLoser semiFinalOrdinal ->
        fixtureDic |> List.ofSeq |> List.map (fun (KeyValue (_, fixture)) ->
            match fixture.Stage with
            | SemiFinal otherOrdinal when otherOrdinal = semiFinalOrdinal ->
                match fixture.HomeParticipant, fixture.AwayParticipant with
                | Confirmed homeSquadId, Confirmed awaySquadId -> [ homeSquadId ; awaySquadId ]
                | _ -> []
            | _ -> [])
            |> List.collect id
    | _ -> []

let private renderConfirmParticipantModal (useDefaultTheme, fixtureDic:FixtureDic, squadDic:SquadDic, confirmParticipantState:ConfirmParticipantState) dispatch =
    let theme = getTheme useDefaultTheme
    let unconfirmed, squadId = confirmParticipantState.Unconfirmed, confirmParticipantState.SquadId
    let unconfirmedText = unconfirmed |> unconfirmedText
    let titleText = sprintf "Confirm %s" unconfirmedText
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let isConfirmingParticipant, confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmConfirmParticipant |> dispatch)
        let cancel = (fun _ -> CancelConfirmParticipant |> dispatch)
        match confirmParticipantState.ConfirmParticipantStatus with
        | Some ConfirmParticipantPending -> true, Loading, None
        | Some (ConfirmParticipantFailed _) | None ->
            match squadId with
            | Some _ -> false, Clickable (confirm, None), cancel |> Some
            | None -> false, NotEnabled None, cancel |> Some
    let errorText = match confirmParticipantState.ConfirmParticipantStatus with | Some (ConfirmParticipantFailed errorText) -> errorText |> Some | Some ConfirmParticipantPending | None -> None
    let warning = [
        [ strong (sprintf "Please confirm the %s" unconfirmedText) ] |> para theme paraCentredSmaller
        br
        [ str "Please note that this action is irreversible." ] |> para theme paraCentredSmallest ]
    let possibleParticipants = possibleParticipants unconfirmed fixtureDic squadDic
    let values =
        possibleParticipants
        |> List.choose (fun squadId ->
            if squadId |> squadDic.ContainsKey then
                let squad = squadDic.[squadId]
                if squad.Eliminated |> not then
                    let (SquadName squadName) = squad.SquadName
                    (squadId |> toJson, squadName) |> Some
                else None
            else None) // note: should never happen
        |> List.sortBy snd
    let values = (String.Empty, String.Empty) :: values
    let defaultValue = match squadId with | Some squadId -> squadId |> toJson |> Some | None -> String.Empty |> Some
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield [ str "Please select the team" ] |> para theme paraCentredSmaller
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            select theme values defaultValue isConfirmingParticipant (SquadSelected >> dispatch) ]
        yield divVerticalSpace 5
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Confirm participant" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private possiblePlayers (squadDic:SquadDic) backsFirst forSquadId =
    if forSquadId |> squadDic.ContainsKey then
        let squad = squadDic.[forSquadId]
        let forwardOrder, backOrder = if backsFirst then 2, 1 else 1, 2
        squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (playerId, player)) ->
            match player.PlayerStatus with
            | Active _ ->
                let (PlayerName playerName) = player.PlayerName
                (playerId, player, playerName) |> Some
            | Withdrawn _ -> None)
        |> List.sortBy (fun (_, player, playerName) -> (match player.PlayerType with | Forward -> forwardOrder | Back -> backOrder), playerName)
        |> List.map (fun (playerId, _, playerName) -> playerId |> toJson, playerName)
    else []

let private renderAddMatchEventModal (useDefaultTheme, fixtureDic:FixtureDic, squadDic:SquadDic, addMatchEventState:AddMatchEventState) dispatch =
    let radioInline theme text isChecked disabled onChange =
        let semantic = if isChecked then Success else Link
        let radioData = { radioDefaultSmall with RadioSemantic = semantic |> Some ; HasBackgroundColour = isChecked }
        radioInline theme radioData (Guid.NewGuid()) (text |> str) isChecked disabled onChange
    let theme = getTheme useDefaultTheme
    let fixtureId, squadId, addMatchEvent = addMatchEventState.FixtureId, addMatchEventState.SquadId, addMatchEventState.AddMatchEvent
    let matchEvents = if fixtureId |> fixtureDic.ContainsKey then match fixtureDic.[fixtureId].MatchResult with | Some matchResult -> matchResult.MatchEvents | None -> [] else []
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let addText =
        match addMatchEvent with
        | TryEvent _ -> "Add try"
        | PenaltyTryEvent -> "Add penalty try"
        | PenaltyKickEvent _ -> "Add penalty kick"
        | ConversionEvent _ -> "Add conversion"
        | DropGoalEvent _ -> "Add drop goal"
        | CardEvent _ -> "Add card"
        | ManOfTheMatchEvent _ -> "Add man-of-the-match"
    let titleText =
        match squad with
        | Some squad ->
            let (SquadName squadName) = squad.SquadName
            sprintf "%s for %s" addText squadName
        | _ -> addText
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let isAdding, onDismiss =
        match addMatchEventState.AddMatchEventStatus with
        | Some AddMatchEventPending -> true, None
        | Some (AddMatchEventFailed _) | None -> false, (fun _ -> CancelAddMatchEvent |> dispatch) |> Some
    let errorText = match addMatchEventState.AddMatchEventStatus with | Some (AddMatchEventFailed errorText) -> errorText |> Some | Some AddMatchEventPending | None -> None
    let addMatchEventInteraction = Clickable ((fun _ -> AddMatchEvent |> dispatch), None)
    let addMatchEventInteraction, contents =
        match addMatchEvent with
        | TryEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic false)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please enter the player who scored the try" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | PenaltyTryEvent -> addMatchEventInteraction, []
        | PenaltyKickEvent (playerId, kickType) ->
            let interaction = match playerId, kickType with | Some _, Some _ -> addMatchEventInteraction | _ -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic true)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let successful, missed = "Successful", "Missed"
            let onSuccessful = (fun _ -> KickSuccessful |> KickTypeChanged |> dispatch)
            let onMissed = (fun _ -> KickMissed |> KickTypeChanged |> dispatch)
            let successfulRadio = radioInline theme successful (match kickType with | Some KickSuccessful -> true | _ -> false) isAdding onSuccessful
            let missedRadio = radioInline theme missed (match kickType with | Some KickMissed -> true | _ -> false) isAdding onMissed
            let contents =
                [
                    [ str "Please enter the player who took the penalty kick and whether the kick was successful" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [ successfulRadio ; missedRadio ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | ConversionEvent (playerId, kickType) ->
            let interaction = match playerId, kickType with | Some _, Some _ -> addMatchEventInteraction | _ -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic true)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let successful, missed = "Successful", "Missed"
            let onSuccessful = (fun _ -> KickSuccessful |> KickTypeChanged |> dispatch)
            let onMissed = (fun _ -> KickMissed |> KickTypeChanged |> dispatch)
            let successfulRadio = radioInline theme successful (match kickType with | Some KickSuccessful -> true | _ -> false) isAdding onSuccessful
            let missedRadio = radioInline theme missed (match kickType with | Some KickMissed -> true | _ -> false) isAdding onMissed
            let contents =
                [
                    [ str "Please enter the player who took the conversion and whether the kick was successful" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [ successfulRadio ; missedRadio ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | DropGoalEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic true)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please enter the player who kicked the drop goal" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | CardEvent (playerId, card) ->
            let interaction = match playerId, card with | Some _, Some _ -> addMatchEventInteraction | _ -> NotEnabled None
            let alreadyHasYellow =
                matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | YellowCard (_, otherPlayerId) when otherPlayerId |> Some = playerId -> true | _ -> false)
            let alreadyHasRed =
                matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | RedCard (_, otherPlayerId) when otherPlayerId |> Some = playerId -> true | _ -> false)
            let warning =
                match playerId with
                | Some playerId when alreadyHasYellow || alreadyHasRed ->
                    let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
                    let warnings =
                        [
                            if alreadyHasYellow then yield [ str (sprintf "%s already has a yellow card" playerName) ] |> para theme paraCentredSmaller
                            if alreadyHasRed then yield [ str (sprintf "%s already has a red card" playerName) ] |> para theme paraCentredSmaller
                        ]
                    [ notification theme notificationWarning warnings ; br ]
                | _ -> []
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic false)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let yellowChecked, redChecked = match card with | Some Yellow | Some SecondYellow -> true, false | Some Red -> false, true | None -> false, false
            let yellowRadio = radioInline theme "Yellow card" yellowChecked isAdding (fun _ -> Yellow |> CardSelected |> dispatch)
            let redRadio = radioInline theme "Red card" redChecked isAdding (fun _ -> Red |> CardSelected |> dispatch)
            let contents =
                [
                    yield! warning
                    yield [ str "Please select the player and the card type" ] |> para theme paraCentredSmaller
                    yield br
                    yield field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    yield divVerticalSpace 5
                    yield field theme { fieldDefault with Grouped = Centred |> Some } [ yellowRadio ; redRadio ]
                    yield divVerticalSpace 5
                ]
            interaction, contents
        | ManOfTheMatchEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers squadDic false)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please select the man-of-the-match" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield! contents
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str addText ] |> button theme { buttonLinkSmall with Interaction = addMatchEventInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderRemoveMatchEventModal (useDefaultTheme, squadDic:SquadDic, removeMatchEventState:RemoveMatchEventState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Remove match event" ] |> para theme paraCentredSmall ]
    let matchEvent = removeMatchEventState.MatchEvent
    let removeMatchEventInteraction, onDismiss =
        match removeMatchEventState.RemoveMatchEventStatus with
        | Some RemoveMatchEventPending -> Loading, None
        | Some (RemoveMatchEventFailed _) | None -> Clickable ((fun _ -> RemoveMatchEvent |> dispatch), None), (fun _ -> CancelRemoveMatchEvent |> dispatch) |> Some
    let errorText = match removeMatchEventState.RemoveMatchEventStatus with | Some (RemoveMatchEventFailed errorText) -> errorText |> Some | Some RemoveMatchEventPending | None -> None
    let warning = [
        [ str (matchEvent |> matchEventText squadDic) ] |> para theme paraCentredSmaller
        br
        [ strong "Are you sure you want to remove this match event?" ] |> para theme paraCentredSmaller ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove match event" ] |> button theme { buttonLinkSmall with Interaction = removeMatchEventInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private filterTabs currentFixturesFilter dispatch =
    let isActive filter =
        match filter with
        | AllFixtures -> currentFixturesFilter = AllFixtures
        | GroupFixtures _ -> match currentFixturesFilter with | GroupFixtures _ -> true | _ -> false
        | KnockoutFixtures -> currentFixturesFilter = KnockoutFixtures
        | Fixture _ -> false
    let filterText filter = match filter with | AllFixtures -> "All" | GroupFixtures _ -> "Group" | KnockoutFixtures -> "Knockout" | Fixture _ -> SHOULD_NEVER_HAPPEN
    let onClick filter =
        match filter with
        | AllFixtures -> (fun _ -> ShowAllFixtures |> dispatch )
        | GroupFixtures _ -> (fun _ -> None |> ShowGroupFixtures |> dispatch )
        | KnockoutFixtures -> (fun _ -> ShowKnockoutFixtures |> dispatch )
        | Fixture _ -> ignore
    let filters = [ AllFixtures ; None |> GroupFixtures ; KnockoutFixtures ]
    filters |> List.map (fun filter -> { IsActive = filter |> isActive ; TabText = filter |> filterText ; TabLinkType = Internal (filter |> onClick) } )

let private groupTabs currentFixturesFilter dispatch =
    let groupTab currentGroup dispatch group =
        { IsActive = group |> Some = currentGroup ; TabText = group |> groupText ; TabLinkType = Internal (fun _ -> group |> Some |> ShowGroupFixtures |> dispatch ) }
    match currentFixturesFilter with
    | GroupFixtures currentGroup -> groups |> List.map (groupTab currentGroup dispatch)
    | _ -> []

let private startsIn (_timestamp:DateTime) : Fable.React.ReactElement option * bool =
    let startsIn, imminent = _timestamp |> startsIn
    (if imminent then strong startsIn else str startsIn) |> Some, imminent

let private stageText stage =
    match stage with
    | Group group -> group |> groupText
    | QuarterFinal quarterFinalOrdinal -> sprintf "Quarter-final %i" quarterFinalOrdinal
    | SemiFinal semiFinalOrdinal -> sprintf "Semi-final %i" semiFinalOrdinal
    | BronzeFinal -> "Bronze final"
    | Final -> "Final"

let private confirmedFixtureDetails (squadDic:SquadDic) fixture =
    match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
    | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
        let matchOutcome, homeScoreEvents, awayScoreEvents, matchEvents = matchResult.MatchOutcome, matchResult.HomeScoreEvents, matchResult.AwayScoreEvents, matchResult.MatchEvents
        let winnerSquadId =
            if matchOutcome.HomeScore > matchOutcome.AwayScore then homeSquadId |> Some
            else if matchOutcome.AwayScore > matchOutcome.HomeScore then awaySquadId |> Some
            else None
        let homeIsWinner, homeName, homeScore =
            let (SquadName squadName) = homeSquadId |> squadName squadDic
            homeSquadId |> Some = winnerSquadId, squadName, matchOutcome.HomeScore
        let awayIsWinner, awayName, awayScore =
            let (SquadName squadName) = awaySquadId |> squadName squadDic
            awaySquadId |> Some = winnerSquadId, squadName, matchOutcome.AwayScore
        let teams = (homeSquadId, homeName, awaySquadId, awayName) |> Some
        let result = (homeIsWinner, homeScore, awayIsWinner, awayScore, homeScoreEvents, awayScoreEvents, matchEvents) |> Some
        teams, result
    | Confirmed homeSquadId, Confirmed awaySquadId, None ->
        let (SquadName homeName), (SquadName awayName) = homeSquadId |> squadName squadDic, awaySquadId |> squadName squadDic
        (homeSquadId, homeName, awaySquadId, awayName) |> Some, None
    | _ -> None, None

let private teamEvents theme fixtureId role forSquadId matchEvents canAdministerResults (squadDic:SquadDic) dispatch =
    let isHome = match role with | Home -> true | Away -> false
    let nonPenaltyTryCount = matchEvents |> List.filter (fun (_, matchEvent) -> match matchEvent with | Try (squadId, _) when squadId = forSquadId -> true | _ -> false) |> List.length
    let conversionCount = matchEvents |> List.filter (fun (_, matchEvent) -> match matchEvent with | Conversion (squadId, _, _) when squadId = forSquadId -> true | _ -> false) |> List.length
    let paraEvent = if isHome then { paraDefaultSmallest with ParaAlignment = RightAligned } else paraDefaultSmallest
    matchEvents
    |> List.mapi (fun i (matchEventId, matchEvent) -> i, matchEventId, matchEvent)
    |> List.sortBy (fun (i, _, matchEvent) ->
        (match matchEvent with | Try _ | Conversion _ -> 0 | PenaltyTry _ -> 1 | PenaltyKick _ -> 2 | DropGoal _ -> 3 | YellowCard _ | RedCard _ -> 4 | ManOfTheMatch _ -> 5), i)
    |> List.choose (fun (_, matchEventId, matchEvent) ->
        let textAndCanRemove  =
            match matchEvent with
            | Try (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, nonPenaltyTryCount <> conversionCount) |> Some
            | PenaltyTry squadId when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | PenaltyKick (squadId, _, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | Conversion (squadId, _, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | DropGoal (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | YellowCard (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | RedCard (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | ManOfTheMatch (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | _ -> None
        match textAndCanRemove, canAdministerResults with
        | Some (text, canRemove), true when canRemove ->
            let removeMatchEvent = [ str "Remove" ] |> link theme (Internal (fun _ -> (fixtureId, matchEventId, matchEvent) |> ShowRemoveMatchEventModal |> dispatch))
            if isHome then [ str (sprintf "%s " text) ; removeMatchEvent ] |> para theme paraEvent |> Some
            else [ removeMatchEvent ; str (sprintf " %s" text) ] |> para theme paraEvent |> Some
        | Some (text, _), _ ->
            [ str text ] |> para theme paraEvent |> Some
        | _ -> None)

let private addLinks theme fixtureId role forSquadId matchEvents dispatch =
    let isHome = match role with | Home -> true | Away -> false
    let nonPenaltyTryCount = matchEvents |> List.filter (fun (_, matchEvent) -> match matchEvent with | Try (squadId, _) when squadId = forSquadId -> true | _ -> false) |> List.length
    let conversionCount = matchEvents |> List.filter (fun (_, matchEvent) -> match matchEvent with | Conversion (squadId, _, _) when squadId = forSquadId -> true | _ -> false) |> List.length
    let needsConversion = conversionCount < nonPenaltyTryCount
    let needsManOfTheMatch = matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | ManOfTheMatch _ -> true | _ -> false) |> not
    let paraEvent = if isHome then { paraDefaultSmallest with ParaAlignment = RightAligned } else paraDefaultSmallest
    let addTry =
        if needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddTryModal |> dispatch)
            [ [ str "Add try" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addPenaltyTry =
        if needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddPenaltyTryModal |> dispatch)
            [ [ str "Add penalty try" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addPenaltyKick =
        if needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddPenaltyKickModal |> dispatch)
            [ [ str "Add penalty kick" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addConversion =
        if needsConversion then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddConversionModal |> dispatch)
            [ [ str "Add conversion" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addDropGoal =
        if needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddDropGoalModal |> dispatch)
            [ [ str "Add drop goal" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addCard =
        if needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddCardModal |> dispatch)
            [ [ str "Add card" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addManOfTheMatch =
        if needsManOfTheMatch && needsConversion |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddManOfTheMatchModal |> dispatch)
            [ [ str "Add man-of-the-match" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    [
        yield RctH.ofOption addTry
        yield RctH.ofOption addPenaltyTry
        yield RctH.ofOption addPenaltyKick
        yield RctH.ofOption addConversion
        yield RctH.ofOption addDropGoal
        yield RctH.ofOption addCard
        yield RctH.ofOption addManOfTheMatch
    ]

let private renderFixture useDefaultTheme fixtureId (fixtureDic:FixtureDic) (squadDic:SquadDic) (_userDic:UserDic) authUser dispatch =
    let theme = getTheme useDefaultTheme
    let canAdministerResults = match authUser with | Some authUser -> authUser.Permissions.ResultsAdminPermission | None -> false
    let fixture, (teams, result) =
        if fixtureId |> fixtureDic.ContainsKey then
            let fixture = fixtureDic.[fixtureId]
            fixture |> Some, fixture |> confirmedFixtureDetails squadDic
        else None, (None, None)
    match fixture, teams with
    | Some fixture, Some (homeSquadId, homeName, awaySquadId, awayName) ->
        let homeIsWinner, homeScore, awayIsWinner, awayScore, _homeScoreEvents, _awayScoreEvents, matchEvents =
            match result with
            | Some (homeIsWinner, homeScore, awayIsWinner, awayScore, homeScoreEvents, awayScoreEvents, matchEvents) ->
                homeIsWinner, homeScore, awayIsWinner, awayScore, homeScoreEvents, awayScoreEvents, matchEvents
            | None ->
                let emptyScoreEvents = { TeamScoreEvents = [] ; PlayerScoreEvents = [] }
                false, 0u, false, 0u, emptyScoreEvents, emptyScoreEvents, []
        let date, time = fixture.KickOff.LocalDateTime |> dateText, fixture.KickOff.LocalDateTime.ToString ("HH:mm")
        let dateAndTime = sprintf "%s at %s" date time
        let homeOutcome =
            let homeOutcome = sprintf "%s %i" homeName homeScore
            let homeOutcome = if homeIsWinner then strong homeOutcome else str homeOutcome
            [ homeOutcome ] |> para theme { paraDefaultSmall with ParaAlignment = RightAligned }
        let awayOutcome =
            let awayOutcome = sprintf "%i %s" awayScore awayName
            let awayOutcome = if awayIsWinner then strong awayOutcome else str awayOutcome
            [ awayOutcome ] |> para theme paraDefaultSmall
        let homeEvents = teamEvents theme fixtureId Home homeSquadId matchEvents canAdministerResults squadDic dispatch
        let awayEvents = teamEvents theme fixtureId Away awaySquadId matchEvents canAdministerResults squadDic dispatch
        let homeAddLinks = if canAdministerResults then addLinks theme fixtureId Home homeSquadId matchEvents dispatch else []
        let awayAddLinks = if canAdministerResults then addLinks theme fixtureId Away awaySquadId matchEvents dispatch else []
        [
            yield [ str (fixture.Stage |> stageText) ] |> para theme paraCentredSmaller
            yield [ str dateAndTime ] |> para theme paraCentredSmallest
            yield divVerticalSpace 10
            yield columnsLeftAndRight [ homeOutcome ] [ awayOutcome ]
            if homeEvents.Length + awayEvents.Length > 0 then
                yield columnsLeftAndRight homeEvents awayEvents
            if homeAddLinks.Length + awayAddLinks.Length > 0 then
                yield columnsLeftAndRight homeAddLinks awayAddLinks

            // TODO-SOON: Points-4-sweepstakers? "Special" News post?...

        ]
    | _ -> [] // note: should never happen

let private renderFixtures (useDefaultTheme, currentFixtureFilter, fixtureDic:FixtureDic, squadDic:SquadDic, authUser, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let matchesFilter fixture =
        match currentFixtureFilter with
        | AllFixtures -> true
        | GroupFixtures currentGroup ->
            match fixture.Stage with | Group group -> group |> Some = currentGroup | QuarterFinal _ | SemiFinal _ | BronzeFinal | Final -> false
        | KnockoutFixtures ->
            match fixture.Stage with | QuarterFinal _ | SemiFinal _ | BronzeFinal | Final -> true | Group _ -> false
        | Fixture _ -> false
    let canConfirmParticipant, canAdministerResults, canCancelFixture =
        match authUser with
        | Some authUser ->
            let canConfirmParticipant = match authUser.Permissions.FixturePermissions with | Some fixturePermissions -> fixturePermissions.ConfirmFixturePermission | None -> false
            let canCancelFixture = match authUser.Permissions.FixturePermissions with | Some fixturePermissions -> fixturePermissions.CreateFixturePermission | None -> false
            canConfirmParticipant, authUser.Permissions.ResultsAdminPermission, canCancelFixture
        | None -> false, false, false
    let confirmParticipant role participant fixtureId =
        match participant with
        | Confirmed _ -> None
        | Unconfirmed unconfirmed ->
            if canConfirmParticipant then
                let confirmable =
                    match unconfirmed with
                    | StageWinner (Group group) | GroupRunnerUp group ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | Group otherGroup when otherGroup = group -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | StageWinner (QuarterFinal quarterFinalOrdinal) ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | QuarterFinal otherOrdinal when otherOrdinal = quarterFinalOrdinal -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | StageWinner (SemiFinal semiFinalOrdinal) | SemiFinalLoser semiFinalOrdinal ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | SemiFinal otherOrdinal when otherOrdinal = semiFinalOrdinal -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | _ -> false
                if confirmable then
                    let paraConfirm = match role with | Home -> { paraDefaultSmallest with ParaAlignment = RightAligned } | Away -> paraDefaultSmallest
                    let onClick = (fun _ -> (fixtureId, role, unconfirmed) |> ShowConfirmParticipantModal |> dispatch)
                    [ [ str "Confirm participant" ] |> para theme paraConfirm ] |> link theme (Internal onClick) |> Some
                else None
            else None
    let stageElement paraDetails stage =
        let stageText =
            match stage with
            | Group _ -> match currentFixtureFilter with | GroupFixtures _ | Fixture _ -> None | AllFixtures | KnockoutFixtures -> stage |> stageText |> Some
            | _ -> stage |> stageText |> Some
        match stageText with | Some stageText -> [ str stageText ] |> para theme paraDetails |> Some | None -> None
    let details fixture =
        match fixture |> confirmedFixtureDetails squadDic with
        | Some (_, homeName, _, awayName), Some (homeIsWinner, homeScore, awayIsWinner, awayScore, _, _, _) ->
            let home = if homeIsWinner then strong homeName else str homeName
            let homeScore = sprintf "%i" homeScore
            let homeScore = if homeIsWinner then strong homeScore else str homeScore
            let homeScore = [ homeScore ] |> para theme paraDefaultSmallest |> Some
            let away = if awayIsWinner then strong awayName else str awayName
            let awayScore = sprintf "%i" awayScore
            let awayScore = if awayIsWinner then strong awayScore else str awayScore
            let awayScore = [ awayScore ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
            home, homeScore, str "-", away, awayScore
        | _ ->
            let home, away = fixture |> homeAndAway squadDic
            str home, None, str "vs.", str away, None
    let extra (fixtureId, fixture) =
        let local = fixture.KickOff.LocalDateTime
        let hasResult = match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with | Confirmed _ , Confirmed _, Some _ -> true | _ -> false
        let onClick = (fun _ -> fixtureId |> ShowFixture |> dispatch)
        if hasResult then
            let showFixtureText = if canAdministerResults then "Edit details" else "View details"
            [ [ str showFixtureText ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
        else
            if canAdministerResults && local < DateTime.Now then
                [ [ str "Add details" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
            else
                let paraExtra = { paraDefaultSmallest with ParaAlignment = RightAligned ; ParaColour = GreyscalePara Grey }
                let extra, imminent = if local < DateTime.Now then em "Result pending" |> Some, true else local |> startsIn
                let paraExtra = if imminent then { paraExtra with ParaColour = GreyscalePara GreyDarker } else paraExtra
                match extra with | Some extra -> [ extra ] |> para theme paraExtra |> Some | None -> None
    let fixtureRow (fixtureId, fixture) =
        let date, time = fixture.KickOff.LocalDateTime |> dateText, fixture.KickOff.LocalDateTime.ToString ("HH:mm")
        let home, homeGoals, vs, away, awayGoals = fixture |> details
        tr false [
            td [ [ str date ] |> para theme paraDefaultSmallest ]
            td [ [ str time ] |> para theme paraDefaultSmallest ]
            td [ RctH.ofOption (fixture.Stage |> stageElement paraDefaultSmallest) ]
            td [ RctH.ofOption (confirmParticipant Home fixture.HomeParticipant fixtureId) ]
            td [ [ home ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
            td [ RctH.ofOption homeGoals ]
            td [ [ vs ] |> para theme { paraDefaultSmallest with ParaAlignment = Centred } ]
            td [ RctH.ofOption awayGoals ]
            td [ [ away ] |> para theme paraDefaultSmallest ]
            td [ RctH.ofOption (confirmParticipant Away fixture.AwayParticipant fixtureId) ]
            td [ RctH.ofOption ((fixtureId, fixture) |> extra) ] ]
    let fixtures =
        fixtureDic
        |> List.ofSeq
        |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture))
        |> List.filter (fun (_, fixture) -> fixture |> matchesFilter)
        |> List.sortBy (fun (_, fixture) -> fixture.KickOff)
    let fixtureRows = fixtures |> List.map (fun (fixtureId, fixture) -> (fixtureId, fixture) |> fixtureRow)
    div divCentred [
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [
                tr false [
                    th [ [ strong "Date" ] |> para theme paraDefaultSmallest ]
                    th [ [ strong "Time" ] |> para theme paraDefaultSmallest ]
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th [] ] ]
            tbody [ yield! fixtureRows ] ] ]

let render (useDefaultTheme, state, authUser:AuthUser option, fixturesProjection:Projection<_ * FixtureDic>, squadsProjection:Projection<_ * SquadDic>, usersProjection:Projection<_ * UserDic>, hasModal, ticks:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "Fixtures / Results" ] |> para theme paraCentredSmall
        yield hr theme false
        match fixturesProjection, squadsProjection, usersProjection with
        | Pending, _, _ | _, Pending, _ | _, _, Pending ->
            yield div divDefault [ divVerticalSpace 10 ; div divCentred [ icon iconSpinnerPulseMedium ] ]
        | Failed, _, _ | _, Failed, _ | _, _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, fixtureDic), Ready (_, squadDic), Ready (_, userDic) ->
            let currentFixturesFilter = match state.CurrentFixturesFilter with | GroupFixtures None -> GroupA |> Some |> GroupFixtures | _ -> state.CurrentFixturesFilter
            let filterTabs = filterTabs currentFixturesFilter dispatch
            match hasModal, state.ConfirmParticipantState with
            | false, Some confirmParticipantState ->
                yield div divDefault [ lazyViewOrHMR2 renderConfirmParticipantModal (useDefaultTheme, fixtureDic, squadDic, confirmParticipantState) (ConfirmParticipantInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.AddMatchEventState with
            | false, Some addMatchEventState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddMatchEventModal (useDefaultTheme, fixtureDic, squadDic, addMatchEventState) (AddMatchEventInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.RemoveMatchEventState with
            | false, Some removeMatchEventState ->
                yield div divDefault [ lazyViewOrHMR2 renderRemoveMatchEventModal (useDefaultTheme, squadDic, removeMatchEventState) (RemoveMatchEventInput >> dispatch) ]
            | _ -> ()
            yield div divCentred [ tabs theme { tabsDefault with TabsSize = Normal ; Tabs = filterTabs } ]
            match currentFixturesFilter with
            | Fixture fixtureId ->
                yield br
                yield! renderFixture useDefaultTheme fixtureId fixtureDic squadDic userDic authUser dispatch
            | _ ->
                let groupTabs = groupTabs currentFixturesFilter dispatch
                match groupTabs with
                | _ :: _ ->
                    yield div divCentred [ tabs theme { tabsDefault with Tabs = groupTabs } ]
                | [] -> ()
                yield br
                yield lazyViewOrHMR2 renderFixtures (useDefaultTheme, currentFixturesFilter, fixtureDic, squadDic, authUser, ticks) dispatch ]
