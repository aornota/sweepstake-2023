module Aornota.Sweepstake2023.Server.Agents.Projections.Fixtures

(* Broadcasts: SendMsg
   Subscribes: FixturesRead
               FixtureEventWritten (ParticipantConfirmed | MatchEventAdded | MatchEventRemoved)
               SquadsRead
               Disconnected *)

open Aornota.Sweepstake2023.Common.Domain.Fixture
open Aornota.Sweepstake2023.Common.Domain.Squad
open Aornota.Sweepstake2023.Common.Revision
open Aornota.Sweepstake2023.Common.UnitsOfMeasure
open Aornota.Sweepstake2023.Common.WsApi.ServerMsg
open Aornota.Sweepstake2023.Server.Agents.Broadcaster
open Aornota.Sweepstake2023.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2023.Server.Common.DeltaHelper
open Aornota.Sweepstake2023.Server.Connection
open Aornota.Sweepstake2023.Server.Events.FixtureEvents
open Aornota.Sweepstake2023.Server.Signal

open System
open System.Collections.Generic

type private FixtureInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnFixturesRead of fixturesRead : FixtureRead list
    | OnParticipantConfirmed of fixtureId : FixtureId * rvn : Rvn * role : Role * squadId : SquadId
    | OnMatchEventAdded of fixtureId : FixtureId * rvn : Rvn * matchEventId : MatchEventId * matchEvent : MatchEvent
    | OnMatchEventRemoved of fixtureId : FixtureId * rvn : Rvn * matchEventId : MatchEventId
    | OnSquadsRead of squadsRead : SquadRead list
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeFixturesProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<FixtureDto list, OtherError<string>>>

type private MatchEventDic = Dictionary<MatchEventId, MatchEvent>

type private Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset ; MatchEventDic : MatchEventDic }
type private FixtureDic = Dictionary<FixtureId, Fixture>

type private PlayerDic = Dictionary<PlayerId, PlayerType>
type private SquadDic = Dictionary<SquadId, Seeding option * PlayerDic>

type private Projectee = { LastRvn : Rvn }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { FixtureDic : FixtureDic ; SquadDic : SquadDic }

type private StateChangeType =
    | Initialization of fixtureDic : FixtureDic * squadDic : SquadDic
    | FixtureChange of fixtureDic : FixtureDic * state : State

type private FixtureDtoDic = Dictionary<FixtureId, FixtureDto>

type private MatchResult = | HomeWin | AwayWin | Draw

let private log category = (Projection Projection.Fixtures, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private matchOutcome fixture =
    let matchEventDic = fixture.MatchEventDic
    let score forSquadId =
        matchEventDic
        |> List.ofSeq |> List.choose (fun (KeyValue (_, matchEvent)) ->
            match matchEvent with
            | Try (squadId, _) when squadId = forSquadId -> 5u |> Some
            | PenaltyTry squadId when squadId = forSquadId -> 7u |> Some
            | PenaltyKick (squadId, _, Successful) when squadId = forSquadId -> 3u |> Some
            | Conversion (squadId, _, Successful) when squadId = forSquadId -> 2u |> Some
            | DropGoal (squadId, _) when squadId = forSquadId -> 3u |> Some
            | _ -> None)
        |> List.sum
    let tries penaltyOnly forSquadId =
        matchEventDic
        |> List.ofSeq |> List.choose (fun (KeyValue (_, matchEvent)) ->
            match matchEvent with
            | Try (squadId, _) when not penaltyOnly && squadId = forSquadId -> 1u |> Some
            | PenaltyTry squadId when squadId = forSquadId -> 1u |> Some
            | _ -> None)
        |> List.sum
    match fixture.HomeParticipant, fixture.AwayParticipant with
    | Confirmed homeSquadId, Confirmed awaySquadId ->
        if fixture.KickOff < DateTimeOffset.UtcNow && matchEventDic.Count > 0 then
            let homeScore, homeTotalTries, homePenaltyTries = homeSquadId |> score, homeSquadId |> tries false, homeSquadId |> tries true
            let awayScore, awayTotalTries, awayPenaltyTries = awaySquadId |> score, awaySquadId |> tries false, awaySquadId |> tries true
            let matchOutcome =
                { HomeScore = homeScore ; AwayScore = awayScore ; HomeTotalTries = homeTotalTries ; AwayTotalTries = awayTotalTries ; HomePenaltyTries = homePenaltyTries ; AwayPenaltyTries = awayPenaltyTries }
            (homeSquadId, awaySquadId, matchOutcome) |> Some
        else None
    | _ -> None

let private cards (matchEventDic:MatchEventDic) =
    matchEventDic |> List.ofSeq |> List.choose (fun (KeyValue (_, matchEvent)) ->
        match matchEvent with
        | YellowCard (squadId, playerId) -> ((squadId, playerId), Yellow) |> Some
        | RedCard (squadId, playerId) -> ((squadId, playerId), Red) |> Some
        | _ -> None)
    |> List.groupBy fst
    |> List.map (fun (pair, cards) ->
        let cards = cards |> List.map snd |> List.sortBy (fun card -> match card with | Yellow -> 0 | _ -> 1)
        let yellowCount = cards |> List.filter (fun card -> match card with | Yellow -> true | _ -> false) |> List.length
        if yellowCount < 2 then pair, cards
        else
            // Note: Assume never more than two Yellows.
            let cards = cards |> List.mapi (fun i card -> match card, i with | Yellow, 0 -> Yellow | Yellow, _ -> SecondYellow | _ -> card)
            pair, cards)

let private teamScoreEvents fixture role forSquadId againstSquadId (cards:((SquadId * PlayerId) * Card list) list) matchOutcome (squadDic:SquadDic) =
    let isTop8 squadId = if squadId |> squadDic.ContainsKey then match squadDic.[squadId] |> fst with | Some seeding -> seeding <= Seeding 8 | None -> false else false
    let forIsTop8, againstIsTop8 = forSquadId |> isTop8, againstSquadId |> isTop8
    let matchResult =
        if matchOutcome.HomeScore > matchOutcome.AwayScore then HomeWin, matchOutcome.HomeScore - matchOutcome.AwayScore
        else if matchOutcome.HomeScore < matchOutcome.AwayScore then AwayWin, matchOutcome.AwayScore - matchOutcome.HomeScore
        else Draw, 0u
    let matchResultEvent =
        match role, matchResult with
        | Home, (HomeWin, _) | Away, (AwayWin, _) -> [ MatchWon, match forIsTop8, againstIsTop8 with | true, false -> 8<point> | false, true -> 12<point> | _ -> 10<point> ]
        | _, (Draw, _) ->
            match fixture.Stage with
            | Group _ -> [ MatchDrawn, match forIsTop8, againstIsTop8 with | true, false -> 4<point> | false, true -> 6<point> | _ -> 5<point> ]
            | _ -> [] // note: no draws for knockout matches
        | Home, (AwayWin, margin) | Away, (HomeWin, margin) ->
            if margin <= 7u then [ LosingBonusPoint, match forIsTop8, againstIsTop8 with | true, false -> 2<point> | false, true -> 4<point> | _ -> 3<point> ]
            else []
    let triesBonusPointEvent =
        let hasBonus =
            match role, matchOutcome.HomeTotalTries, matchOutcome.AwayTotalTries with
            | Home, homeTotalTries, _ when homeTotalTries >= 4u -> true
            | Away, _, awayTotalTries when awayTotalTries >= 4u -> true
            | _ -> false
        if hasBonus then [ TriesBonusPoint, match forIsTop8, againstIsTop8 with | true, false -> 2<point> | false, true -> 4<point> | _ -> 3<point> ]
        else []
    let penaltyTryEvents =
        let penaltyTries = match role with | Home -> matchOutcome.HomePenaltyTries | Away -> matchOutcome.AwayPenaltyTries
        if penaltyTries > 0u then [ for _ in 1u..penaltyTries do yield PenaltyTryScored, 6<point> ]
        else []
    let cardEvents =
        cards |> List.collect (fun ((squadId, playerId), cards) ->
            if squadId = forSquadId then
                cards |> List.map (fun card ->
                    match card with
                    | Yellow -> (playerId, Yellow) |> PlayerCard, -2<point>
                    | SecondYellow -> (playerId, SecondYellow) |> PlayerCard, -2<point>
                    | Red -> (playerId, Red) |> PlayerCard, -4<point>)
            else [])
    matchResultEvent @ triesBonusPointEvent @ penaltyTryEvents @ cardEvents

let private playerScoreEvents forSquadId (cards:((SquadId * PlayerId) * Card list) list) (squadDic:SquadDic) (matchEventDic:MatchEventDic) =
    let playerType playerId =
        if forSquadId |> squadDic.ContainsKey then
            let playerDic = squadDic.[forSquadId] |> snd
            if playerId |> playerDic.ContainsKey then playerDic.[playerId] |> Some else None
        else None
    let nonCardEvents =
        matchEventDic |> List.ofSeq |> List.collect (fun (KeyValue (_, matchEvent)) ->
            match matchEvent with
            | Try (squadId, playerId) when squadId = forSquadId ->
                let points = match playerType playerId with | Some Forward -> 12<point> | Some Back | None -> 9<point>
                [ playerId, (TryScored, points) ]
            | PenaltyKick (squadId, playerId, Successful) when squadId = forSquadId -> [ playerId, (PenaltyKickSuccessful, 3<point>) ]
            | PenaltyKick (squadId, playerId, Missed) when squadId = forSquadId -> [ playerId, (PenaltyKickMissed, -2<point>) ]
            | Conversion (squadId, playerId, Successful) when squadId = forSquadId -> [ playerId, (ConversionSuccessful, 2<point>) ]
            | Conversion (squadId, playerId, Missed) when squadId = forSquadId -> [ playerId, (ConversionMissed, -1<point>) ]
            | DropGoal (squadId, playerId) when squadId = forSquadId -> [ playerId, (DropGoalSuccessful, 3<point>) ]
            | ManOfTheMatch (squadId, playerId) when squadId = forSquadId ->
                let points = match playerType playerId with | Some Forward -> 13<point> | Some Back | None -> 10<point>
                [ playerId, (ManOfTheMatchAwarded, points) ]
            | _ -> [])
    let cardEvents =
        cards |> List.collect (fun ((squadId, playerId), cards) ->
            if squadId = forSquadId then
                cards |> List.map (fun card ->
                    match card with
                    | Yellow -> playerId, (Yellow |> Card, -3<point>)
                    | SecondYellow -> playerId, (SecondYellow |> Card, -3<point>)
                    | Red -> playerId, (Red |> Card, -6<point>))
            else [])
    nonCardEvents @ cardEvents
    |> List.groupBy fst
    |> List.map (fun (playerId, events) -> playerId, events |> List.map snd)

let private fixtureDto (squadDic:SquadDic) (fixtureId, fixture:Fixture) : FixtureDto =
    let matchResult =
        match fixture |> matchOutcome with
        | Some (homeSquadId, awaySquadId, matchOutcome) ->
            let matchEventDic = fixture.MatchEventDic
            let cards = matchEventDic |> cards
            let homeTeamScoreEvents = teamScoreEvents fixture Home homeSquadId awaySquadId cards matchOutcome squadDic
            let homePlayerScoreEvents = playerScoreEvents homeSquadId cards squadDic matchEventDic
            let awayTeamScoreEvents = teamScoreEvents fixture Away awaySquadId homeSquadId cards matchOutcome squadDic
            let awayPlayerScoreEvents = playerScoreEvents awaySquadId cards squadDic matchEventDic
            let homeScoreEvents = { TeamScoreEvents = homeTeamScoreEvents ; PlayerScoreEvents = homePlayerScoreEvents }
            let awayScoreEvents = { TeamScoreEvents = awayTeamScoreEvents ; PlayerScoreEvents = awayPlayerScoreEvents }
            let matchEvents = matchEventDic |> List.ofSeq |> List.map (fun (KeyValue (matchEventId, matchEvent)) -> matchEventId, matchEvent)
            { MatchOutcome = matchOutcome ; HomeScoreEvents = homeScoreEvents ; AwayScoreEvents = awayScoreEvents ; MatchEvents = matchEvents } |> Some
        | None -> None
    { FixtureId = fixtureId ; Rvn = fixture.Rvn ; Stage = fixture.Stage ; HomeParticipant = fixture.HomeParticipant ; AwayParticipant = fixture.AwayParticipant ; KickOff = fixture.KickOff
      MatchResult = matchResult }

let private fixtureDtoDic (squadDic:SquadDic) (fixtureDic:FixtureDic) =
    let fixtureDtoDic = FixtureDtoDic ()
    fixtureDic |> List.ofSeq |> List.iter (fun (KeyValue (fixtureId, fixture)) ->
        let fixtureDto = (fixtureId, fixture) |> fixtureDto squadDic
        (fixtureDto.FixtureId, fixtureDto) |> fixtureDtoDic.Add)
    fixtureDtoDic

let private fixtureDtos state = state.FixtureDic |> List.ofSeq |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture) |> fixtureDto state.SquadDic)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendFixtureDtoDelta (projecteeDic:ProjecteeDic) fixtureDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendFixtureDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, fixtureDtoDelta) |> FixturesDeltaMsg |> FixturesProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization (fixtureDic, squadDic) ->
            sprintf "%s -> initialized" source |> Info |> log
            { FixtureDic = FixtureDic fixtureDic ; SquadDic = squadDic } // note: no need to copy squadDic since never changes
        | FixtureChange (fixtureDic, state) ->
            let squadDic = state.SquadDic
            let previousFixtureDtoDic = state.FixtureDic |> fixtureDtoDic squadDic
            let fixtureDtoDic = fixtureDic |> fixtureDtoDic squadDic
            let fixtureDtoDelta = fixtureDtoDic |> delta previousFixtureDtoDic
            if fixtureDtoDelta |> isEmpty |> not then
                sprintf "%s -> FixtureDto delta %A -> %i projectee/s" source fixtureDtoDelta projecteeDic.Count |> Info |> log
                fixtureDtoDelta |> sendFixtureDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with FixtureDic = FixtureDic fixtureDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private ifAllRead source (fixturesRead:(FixtureRead list) option, squadsRead:(SquadRead list) option) =
    match fixturesRead, squadsRead with
    | Some fixturesRead, Some squadsRead ->
        let fixtureDic = FixtureDic ()
        fixturesRead |> List.iter (fun fixtureRead ->
            let matchEventDic = MatchEventDic ()
            fixtureRead.MatchEventsRead |> List.iter (fun matchEventRead ->
                if matchEventRead.MatchEventId |> matchEventDic.ContainsKey |> not then (matchEventRead.MatchEventId, matchEventRead.MatchEvent) |> matchEventDic.Add)
            let fixture = { Rvn = fixtureRead.Rvn ; Stage = fixtureRead.Stage ; HomeParticipant = fixtureRead.HomeParticipant ; AwayParticipant = fixtureRead.AwayParticipant
                            KickOff = fixtureRead.KickOff ; MatchEventDic = matchEventDic }
            (fixtureRead.FixtureId, fixture) |> fixtureDic.Add)
        let squadDic = SquadDic ()
        squadsRead |> List.iter (fun squadRead ->
            let playerDic = PlayerDic ()
            squadRead.PlayersRead |> List.iter (fun playerRead -> (playerRead.PlayerId, playerRead.PlayerType) |> playerDic.Add)
            if squadRead.SquadId |> squadDic.ContainsKey |> not then (squadRead.SquadId, (squadRead.Seeding, playerDic)) |> squadDic.Add)
        let projecteeDic = ProjecteeDic ()
        let state = (fixtureDic, squadDic) |> Initialization |> updateState source projecteeDic
        (state, fixtureDic, projecteeDic) |> Some
    | _ -> None

type Fixtures () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 users) (0 posts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None
            | OnFixturesRead _ -> "OnFixturesRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnMatchEventAdded _ -> "OnMatchEventAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnMatchEventRemoved _ -> "OnMatchEventRemoved when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead fixturesRead squadsRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead
            | OnFixturesRead fixturesRead ->
                let source = "OnFixturesRead"
                sprintf "%s (%i fixture/s) when pendingAllRead" source fixturesRead.Length |> Info |> log
                let fixturesRead = fixturesRead |> Some
                match (fixturesRead, squadsRead) |> ifAllRead source with
                | Some (state, fixtureDic, projecteeDic) ->
                    return! projectingFixtures state fixtureDic projecteeDic
                | None -> return! pendingAllRead fixturesRead squadsRead
            | OnParticipantConfirmed _ -> "OnParticipantConfirmed when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead
            | OnMatchEventAdded _ -> "OnMatchEventAdded when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead
            | OnMatchEventRemoved _ -> "OnMatchEventRemoved when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squad/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squadsRead = squadsRead |> Some
                match (fixturesRead, squadsRead) |> ifAllRead source with
                | Some (state, fixtureDic, projecteeDic) ->
                    return! projectingFixtures state fixtureDic projecteeDic
                | None -> return! pendingAllRead fixturesRead squadsRead
            | RemoveConnections _ -> "RemoveConnections when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead
            | HandleInitializeFixturesProjectionQry _ -> "HandleInitializeFixturesProjectionQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead fixturesRead squadsRead }
        and projectingFixtures state fixtureDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state fixtureDic projecteeDic
            | OnFixturesRead _ -> "OnFixturesRead when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state fixtureDic projecteeDic
            | OnParticipantConfirmed (fixtureId, rvn, role, squadId) ->
                let source = "OnParticipantConfirmed"
                sprintf "%s (%A %A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source fixtureId rvn fixtureDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if fixtureId |> fixtureDic.ContainsKey then // note: silently ignore unknown fixtureId (should never happen)
                        let fixture = fixtureDic.[fixtureId]
                        match role with
                        | Home ->
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; HomeParticipant = squadId |> Confirmed }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                        | Away ->
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn ; AwayParticipant = squadId |> Confirmed }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                    else state
                return! projectingFixtures state fixtureDic projecteeDic
            | OnMatchEventAdded (fixtureId, rvn, matchEventId, matchEvent) ->
                let source = "OnMatchEventAdded"
                sprintf "%s (%A %A %A %A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source fixtureId rvn matchEventId matchEvent fixtureDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if fixtureId |> fixtureDic.ContainsKey then // note: silently ignore unknown fixtureId (should never happen)
                        let fixture = fixtureDic.[fixtureId]
                        let matchEventDic = fixture.MatchEventDic
                        if matchEventId |> matchEventDic.ContainsKey |> not then // note: silently ignore already-known matchEventId (should never happen)
                            (matchEventId, matchEvent) |> matchEventDic.Add
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingFixtures state fixtureDic projecteeDic
            | OnMatchEventRemoved (fixtureId, rvn, matchEventId) ->
                let source = "OnMatchEventRemoved"
                sprintf "%s (%A %A %A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source fixtureId rvn matchEventId fixtureDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if fixtureId |> fixtureDic.ContainsKey then // note: silently ignore unknown fixtureId (should never happen)
                        let fixture = fixtureDic.[fixtureId]
                        let matchEventDic = fixture.MatchEventDic
                        if matchEventId |> matchEventDic.ContainsKey then // note: silently ignore unknown matchEventId (should never happen)
                            matchEventId |> matchEventDic.Remove |> ignore
                            fixtureDic.[fixtureId] <- { fixture with Rvn = rvn }
                            (fixtureDic, state) |> FixtureChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingFixtures state fixtureDic projecteeDic
            | OnSquadsRead _ -> "OnSquadsRead when projectingFixtures" |> IgnoredInput |> Agent |> log ; return! projectingFixtures state fixtureDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingFixtures (%i fixture/s) (%i projectee/s)" source connectionIds fixtureDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingFixtures state fixtureDic projecteeDic
            | HandleInitializeFixturesProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeFixturesProjectionQry"
                sprintf "%s for %A when projectingFixtures (%i fixture/s) (%i projectee/s)" source connectionId fixtureDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingFixtures -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> fixtureDtos |> Ok
                result |> logResult source (fun fixtureDtos -> sprintf "%i fixture/s" fixtureDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingFixtures state fixtureDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Fixtures |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | FixturesRead fixturesRead -> fixturesRead |> OnFixturesRead |> agent.Post
            | FixtureEventWritten (rvn, fixtureEvent) ->
                match fixtureEvent with
                | FixtureCreated _ -> () // note: no need to handle since cannot happen once FixturesRead
                | ParticipantConfirmed (fixtureId, role, squadId) -> (fixtureId, rvn, role, squadId) |> OnParticipantConfirmed |> agent.Post
                | MatchEventAdded (fixtureId, matchEventId, matchEvent) -> (fixtureId, rvn, matchEventId, matchEvent) |> OnMatchEventAdded |> agent.Post
                | MatchEventRemoved (fixtureId, matchEventId) -> (fixtureId, rvn, matchEventId) |> OnMatchEventRemoved |> agent.Post
            | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to FixturesRead | FixtureEventWritten | SquadsRead | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeFixturesProjectionQryAsync connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeFixturesProjectionQry) |> agent.PostAndAsyncReply

let fixtures = Fixtures ()
