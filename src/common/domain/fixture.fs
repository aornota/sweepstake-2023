module Aornota.Sweepstake2023.Common.Domain.Fixture

open Aornota.Sweepstake2023.Common.Domain.Core
open Aornota.Sweepstake2023.Common.Domain.Squad
open Aornota.Sweepstake2023.Common.Revision
open Aornota.Sweepstake2023.Common.UnitsOfMeasure

open System

type FixtureId = | FixtureId of guid : Guid with static member Create () = Guid.NewGuid () |> FixtureId

type Role = | Home | Away

type Stage =
    | Group of group : Group
    | QuarterFinal of quarterFinalOrdinal : uint32
    | SemiFinal of semiFinalOrdinal : uint32
    | BronzeFinal
    | Final

type Unconfirmed =
    | GroupRunnerUp of group : Group
    | StageWinner of stage : Stage
    | SemiFinalLoser of semiFinalOrdinal : uint32

type Participant =
    | Confirmed of squadId : SquadId
    | Unconfirmed of unconfirmed : Unconfirmed

type MatchEventId = | MatchEventId of guid : Guid with static member Create () = Guid.NewGuid () |> MatchEventId

type KickOutcome =
    | Successful
    | Missed

type MatchEvent =
    | Try of squadId : SquadId * playerId : PlayerId
    | PenaltyTry of squadId : SquadId
    | PenaltyKick of squadId : SquadId * playerId : PlayerId * kickOutcome : KickOutcome
    | Conversion of squadId : SquadId * playerId : PlayerId * kickOutcome : KickOutcome
    | DropGoal of squadId : SquadId * playerId : PlayerId
    | YellowCard of squadId : SquadId * playerId : PlayerId
    | RedCard of squadId : SquadId * playerId : PlayerId
    | ManOfTheMatch of squadId : SquadId * playerId : PlayerId

type MatchOutcome = { HomeScore : uint32 ; AwayScore : uint32 ; HomeTotalTries : uint32 ; AwayTotalTries : uint32 ; HomePenaltyTries : uint32 ; AwayPenaltyTries : uint32 }

type Card =
    | Yellow
    | SecondYellow
    | Red

type TeamScoreEvent =
    | MatchWon
    | MatchDrawn
    | TriesBonusPoint
    | LosingBonusPoint
    | PenaltyTryScored
    | PlayerCard of playerId : PlayerId * card : Card

type PlayerScoreEvent =
    | TryScored
    | PenaltyKickSuccessful
    | PenaltyKickMissed
    | ConversionSuccessful
    | ConversionMissed
    | DropGoalSuccessful
    | Card of card : Card
    | ManOfTheMatchAwarded

type ScoreEvents = { TeamScoreEvents : (TeamScoreEvent * int<point>) list ; PlayerScoreEvents : (PlayerId * (PlayerScoreEvent * int<point>) list) list }

type MatchResult = { MatchOutcome : MatchOutcome ; HomeScoreEvents : ScoreEvents ; AwayScoreEvents : ScoreEvents ; MatchEvents : (MatchEventId * MatchEvent) list }

type FixtureDto =
    { FixtureId : FixtureId ; Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset ; MatchResult : MatchResult option }
