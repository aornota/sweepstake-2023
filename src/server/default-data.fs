module Aornota.Sweepstake2023.Server.DefaultData

open Aornota.Sweepstake2023.Common.Domain.Core
open Aornota.Sweepstake2023.Common.Domain.Draft
open Aornota.Sweepstake2023.Common.Domain.Fixture
open Aornota.Sweepstake2023.Common.Domain.Squad
open Aornota.Sweepstake2023.Common.Domain.User
open Aornota.Sweepstake2023.Common.IfDebug
open Aornota.Sweepstake2023.Common.WsApi.ServerMsg
open Aornota.Sweepstake2023.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2023.Server.Agents.Entities.Drafts
open Aornota.Sweepstake2023.Server.Agents.Entities.Fixtures
open Aornota.Sweepstake2023.Server.Agents.Entities.Squads
open Aornota.Sweepstake2023.Server.Agents.Entities.Users
open Aornota.Sweepstake2023.Server.Agents.Persistence
open Aornota.Sweepstake2023.Server.Authorization
open Aornota.Sweepstake2023.Server.Common.Helpers

open System
open System.IO

let private deleteExistingUsersEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingSquadsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingFixturesEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingDraftsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)

let private log category = (Host, category) |> consoleLogger.Log

let private logResult shouldSucceed scenario result =
    match shouldSucceed, result with
    | true, Ok _ -> sprintf "%s -> succeeded (as expected)" scenario |> Verbose |> log
    | true, Error error -> sprintf "%s -> unexpectedly failed -> %A" scenario error |> Danger |> log
    | false, Ok _ -> sprintf "%s -> unexpectedly succeeded" scenario |> Danger |> log
    | false, Error error -> sprintf "%s -> failed (as expected) -> %A" scenario error |> Verbose |> log
let private logShouldSucceed scenario result = result |> logResult true scenario
let private logShouldFail scenario result = result |> logResult false scenario

let private delete dir =
    Directory.GetFiles dir |> Array.iter File.Delete
    Directory.Delete dir

let private ifToken fCmdAsync token = async { return! match token with | Some token -> token |> fCmdAsync | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync }

let private superUser = SuperUser
let private nephId = Guid.Empty |> UserId
let private nephTokens = permissions nephId superUser |> UserTokens

// #region SquadIds
let private newZealandId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let private franceId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let private italyId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let private uruguayId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId
let private namibiaId = Guid "00000015-0000-0000-0000-000000000000" |> SquadId
let private southAfricaId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let private irelandId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let private scotlandId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let private tongaId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId
let private romaniaId = Guid "00000025-0000-0000-0000-000000000000" |> SquadId
let private walesId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let private australiaId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let private fijiId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let private georgiaId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId
let private portugalId = Guid "00000035-0000-0000-0000-000000000000" |> SquadId
let private englandId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let private japanId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let private argentinaId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let private samoaId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId
let private chileId = Guid "00000045-0000-0000-0000-000000000000" |> SquadId
// #endregion

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    // #region: Force re-creation of initial User/s events if directory already exists (if requested)
    if deleteExistingUsersEvents && Directory.Exists usersDir then
        sprintf "deleting existing User/s events -> %s" usersDir |> Info |> log
        delete usersDir
    // #endregion

    if Directory.Exists usersDir then sprintf "preserving existing User/s events -> %s" usersDir |> Info |> log
    else
        sprintf "creating initial User/s events -> %s" usersDir |> Info |> log
        "starting Users agent" |> Info |> log
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnUsersEventsRead to Users agent" |> Info |> log
        [] |> users.OnUsersEventsRead

        // #region: Create initial SuperUser | Administators - and a couple of Plebs
        let neph = UserName "neph"
        let dummyPassword = Password "password"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nephId, neph, dummyPassword, superUser) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph)
        let administrator = Administrator
        let rosieId, rosie = Guid "ffffffff-0001-0000-0000-000000000000" |> UserId, UserName "rosie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, rosieId, rosie, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie)
        let hughId, hugh = Guid "ffffffff-0002-0000-0000-000000000000" |> UserId, UserName "hugh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, hughId, hugh, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh)
        let pleb = Pleb
        let chrisId, chris = Guid.NewGuid() |> UserId, UserName "chris"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, chrisId, chris, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" chris)
        let damianId, damian = Guid.NewGuid() |> UserId, UserName "damian"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, damianId, damian, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" damian)
        let denisId, denis = Guid.NewGuid() |> UserId, UserName "denis"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, denisId, denis, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" denis)
        let jemId, jem = Guid.NewGuid() |> UserId, UserName "jem"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jemId, jem, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jem)
        let joshId, josh = Guid.NewGuid() |> UserId, UserName "josh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, joshId, josh, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" josh)
        let robId, rob = Guid.NewGuid() |> UserId, UserName "rob"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, robId, rob, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rob)
        let steveMId, steveM = Guid.NewGuid() |> UserId, UserName "steve m"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveMId, steveM, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveM)
        let steveSId, steveS = Guid.NewGuid() |> UserId, UserName "steve s"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveSId, steveS, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveS)
        let susieId, susie = Guid.NewGuid() |> UserId, UserName "susie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, susieId, susie, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" susie)
        let tomId, tom = Guid.NewGuid() |> UserId, UserName "tom"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, tomId, tom, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" tom)
        let willId, will = Guid.NewGuid() |> UserId, UserName "will"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, willId, will, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will)
        let callumId, callum = Guid.NewGuid() |> UserId, UserName "callum"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, callumId, callum, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" callum)
        let chadId, chad = Guid.NewGuid() |> UserId, UserName "chad"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, chadId, chad, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" chad)
        let jackId, jack = Guid.NewGuid() |> UserId, UserName "jack"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jackId, jack, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jack)
        let martynId, martyn = Guid.NewGuid() |> UserId, UserName "martyn"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, martynId, martyn, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" martyn)
        let mikeId, mike = Guid.NewGuid() |> UserId, UserName "mike"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mikeId, mike, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" mike)
        let sueId, sue = Guid.NewGuid() |> UserId, UserName "sue"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, sueId, sue, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" sue)
        let highnamId, highnam = Guid.NewGuid() |> UserId, UserName "highnam"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, highnamId, highnam, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" highnam)
        let mollyId, molly = Guid.NewGuid() |> UserId, UserName "molly"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mollyId, molly, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" molly)
        let nourdineId, nourdine = Guid.NewGuid() |> UserId, UserName "nourdine"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nourdineId, nourdine, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" nourdine)
        // #endregion

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let private createInitialSquadsEventsIfNecessary = async {
    let squadsDir = directory EntityType.Squads

    // #region: Force re-creation of initial Squad/s events if directory already exists (if requested)
    if deleteExistingSquadsEvents && Directory.Exists squadsDir then
        sprintf "deleting existing Squad/s events -> %s" squadsDir |> Info |> log
        delete squadsDir
    // #endregion

    if Directory.Exists squadsDir then sprintf "preserving existing Squad/s events -> %s" squadsDir |> Info |> log
    else
        sprintf "creating initial Squad/s events -> %s" squadsDir |> Info |> log
        "starting Squads agent" |> Info |> log
        () |> squads.Start
        // Note: Send dummy OnSquadsEventsRead to Squads agent to ensure that it transitions [from pendingOnSquadsEventsRead] to managingSquads; otherwise HandleCreateSquadCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnSquadsEventsRead to Squads agent" |> Info |> log
        [] |> squads.OnSquadsEventsRead

        // #region: Create initial Squads.

        // #region: Group A - New Zealand | France | Italy | Uruguay | Namibia
        let newZealand = SquadName "New Zealand"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, newZealandId, newZealand, GroupA, Some (Seeding 2), CoachName "Ian Foster") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" newZealand)
        let france = SquadName "France"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupA, Some (Seeding 7), CoachName "Fabien Galthié") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let italy = SquadName "Italy"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, italyId, italy, GroupA, Some (Seeding 12), CoachName "Kieran Crowley") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" italy)
        let uruguay = SquadName "Uruguay"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, uruguayId, uruguay, GroupA, None, CoachName "Esteban Meneses") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" uruguay)
        let namibia = SquadName "Namibia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, namibiaId, namibia, GroupA, None, CoachName "Allister Coetzee") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" namibia)
        // #endregion
        // #region: Group B - South Africa | Ireland | Scotland | Tonga | Romania
        let southAfrica = SquadName "South Africa"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, southAfricaId, southAfrica, GroupB, Some (Seeding 1), CoachName "Jacques Nienaber") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" southAfrica)
        let ireland = SquadName "Ireland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, irelandId, ireland, GroupB, Some (Seeding 5), CoachName "Andy Farrell") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ireland)
        let scotland = SquadName "Scotland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, scotlandId, scotland, GroupB, Some (Seeding 9), CoachName "Gregor Townsend") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" scotland)
        let tonga = SquadName "Tonga"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, tongaId, tonga, GroupB, None, CoachName "Toutai Kefu") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" tonga)
        let romania = SquadName "Romania"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, romaniaId, romania, GroupB, None, CoachName "Eugen Apjok") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" romania)
        // #endregion
        // #region: Group C - Wales | Australia | Fiji | Georgia | Portugal
        let wales = SquadName "Wales"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, walesId, wales, GroupC, Some (Seeding 4), CoachName "Warren Gatland") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" wales)
        let australia = SquadName "Australia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, australiaId, australia, GroupC, Some (Seeding 6), CoachName "Eddie Jones") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" australia)
        let fiji = SquadName "Fiji"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, fijiId, fiji, GroupC, Some (Seeding 11), CoachName "Simon Raiwalui") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" fiji)
        let georgia = SquadName "Georgia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, georgiaId, georgia, GroupC, None, CoachName "Levan Maisashvili") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" georgia)
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupC, None, CoachName "Patrice Lagisquet") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" portugal)
        // #endregion
        // #region: Group D - England | Japan | Argentina | Samoa | Chile
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupD, Some (Seeding 3), CoachName "Steve Borthwick") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" england)
        let japan = SquadName "Japan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, japanId, japan, GroupD, Some (Seeding 8), CoachName "Jamie Joseph") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" japan)
        let argentina = SquadName "Argentina"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, argentinaId, argentina, GroupD, Some (Seeding 10), CoachName "Michael Cheika") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" argentina)
        let samoa = SquadName "Samoa"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, samoaId, samoa, GroupD, None, CoachName "Seilala Mapusua") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" samoa)
        let chile = SquadName "Chile"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, chileId, chile, GroupD, None, CoachName "Pablo Lemoine") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" chile)
        // #endregion

        // #endregion

        // Note: Reset Squads agent [to pendingOnSquadsEventsRead] so that it handles subsequent SquadsEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Squads agent" |> Info |> log
        () |> squads.Reset
    return () }

let private createInitialFixturesEventsIfNecessary = async {
    let fixtureId matchNumber =
        if matchNumber < 10u then sprintf "00000000-0000-0000-0000-00000000000%i" matchNumber |> Guid |> FixtureId
        else if matchNumber < 100u then sprintf "00000000-0000-0000-0000-0000000000%i" matchNumber |> Guid |> FixtureId
        else FixtureId.Create ()

    let fixturesDir = directory EntityType.Fixtures

    // #region: Force re-creation of initial Fixture/s events if directory already exists (if requested)
    if deleteExistingFixturesEvents && Directory.Exists fixturesDir then
        sprintf "deleting existing Fixture/s events -> %s" fixturesDir |> Info |> log
        delete fixturesDir
    // #endregion

    if Directory.Exists fixturesDir then sprintf "preserving existing Fixture/s events -> %s" fixturesDir |> Info |> log
    else
        sprintf "creating initial Fixture/s events -> %s" fixturesDir |> Info |> log
        "starting Fixtures agent" |> Info |> log
        () |> fixtures.Start
        // Note: Send dummy OnFixturesEventsRead to Users agent to ensure that it transitions [from pendingOnFixturesEventsRead] to managingFixtures; otherwise HandleCreateFixtureCmdAsync would be ignored (and block).
        "sending dummy OnFixturesEventsRead to Fixtures agent" |> Info |> log
        [] |> fixtures.OnFixturesEventsRead

        // #region: Group A
        let franceVsNewZealand = (2023, 09, 08, 19, 15) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 1u, Group GroupA, Confirmed franceId, Confirmed newZealandId, franceVsNewZealand) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 1u)
        let italyVsNamibia = (2023, 09, 09, 11, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 2u, Group GroupA, Confirmed italyId, Confirmed namibiaId, italyVsNamibia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 2u)
        let franceVsUruguay = (2023, 09, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 3u, Group GroupA, Confirmed franceId, Confirmed uruguayId, franceVsUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 3u)
        let newZealandVsNamibia = (2023, 09, 15, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 4u, Group GroupA, Confirmed newZealandId, Confirmed namibiaId, newZealandVsNamibia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 4u)
        let italyVsUruguay = (2023, 09, 20, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 5u, Group GroupA, Confirmed italyId, Confirmed uruguayId, italyVsUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 5u)
        let franceVsNamibia = (2023, 09, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 6u, Group GroupA, Confirmed franceId, Confirmed namibiaId, franceVsNamibia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 6u)
        let uruguayVsNamibia = (2023, 09, 27, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 7u, Group GroupA, Confirmed uruguayId, Confirmed namibiaId, uruguayVsNamibia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 7u)
        let newZealandVsItaly = (2023, 09, 29, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 8u, Group GroupA, Confirmed newZealandId, Confirmed italyId, newZealandVsItaly) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 8u)
        let newZealandVsItalyUruguay = (2023, 10, 05, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 9u, Group GroupA, Confirmed newZealandId, Confirmed uruguayId, newZealandVsItalyUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 9u)
        let franceVsItaly = (2023, 10, 06, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 10u, Group GroupA, Confirmed franceId, Confirmed italyId, franceVsItaly) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 10u)
        // #endregion
        // #region: Group B
        let irelandVsRomania = (2023, 09, 09, 13, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 11u, Group GroupB, Confirmed irelandId, Confirmed romaniaId, irelandVsRomania) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 11u)
        let southAfricaVsScotland = (2023, 09, 10, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 12u, Group GroupB, Confirmed southAfricaId, Confirmed scotlandId, southAfricaVsScotland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 12u)
        let irelandVsTonga = (2023, 09, 16, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 13u, Group GroupB, Confirmed irelandId, Confirmed tongaId, irelandVsTonga) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 13u)
        let southAfricaVsRomania = (2023, 09, 17, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 14u, Group GroupB, Confirmed southAfricaId, Confirmed romaniaId, southAfricaVsRomania) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 14u)
        let southAfricaVsIreland = (2023, 09, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 15u, Group GroupB, Confirmed southAfricaId, Confirmed irelandId, southAfricaVsIreland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 15u)
        let scotlandVsTonga = (2023, 09, 24, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 16u, Group GroupB, Confirmed scotlandId, Confirmed tongaId, scotlandVsTonga) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 16u)
        let scotlandVsRomania = (2023, 09, 30, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 17u, Group GroupB, Confirmed scotlandId, Confirmed romaniaId, scotlandVsRomania) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 17u)
        let southAfricaVsTonga = (2023, 10, 01, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 18u, Group GroupB, Confirmed southAfricaId, Confirmed tongaId, southAfricaVsTonga) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 18u)
        let irelandVsScotland = (2023, 10, 07, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 19u, Group GroupB, Confirmed irelandId, Confirmed scotlandId, irelandVsScotland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 19u)
        let tongaVsRomania = (2023, 10, 08, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 20u, Group GroupB, Confirmed tongaId, Confirmed romaniaId, tongaVsRomania) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 20u)
        // #endregion
        // #region: Group C
        let australiaVsGeorgia = (2023, 09, 09, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 21u, Group GroupC, Confirmed australiaId, Confirmed georgiaId, australiaVsGeorgia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 21u)
        let walesVsFiji = (2023, 09, 10, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 22u, Group GroupC, Confirmed walesId, Confirmed fijiId, walesVsFiji) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 22u)
        let walesVsPortugal = (2023, 09, 16, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 23u, Group GroupC, Confirmed walesId, Confirmed portugalId, walesVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 23u)
        let australiaVsFiji = (2023, 09, 17, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 24u, Group GroupC, Confirmed australiaId, Confirmed fijiId, australiaVsFiji) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 24u)
        let georgiaVsPortugal = (2023, 09, 23, 12, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 25u, Group GroupC, Confirmed georgiaId, Confirmed portugalId, georgiaVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 25u)
        let walesVsAustralia = (2023, 09, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 26u, Group GroupC, Confirmed walesId, Confirmed australiaId, walesVsAustralia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 26u)
        let fijiVsGeorgia = (2023, 09, 30, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 27u, Group GroupC, Confirmed fijiId, Confirmed georgiaId, fijiVsGeorgia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 27u)
        let australiaVsPortugal = (2023, 10, 01, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 28u, Group GroupC, Confirmed australiaId, Confirmed portugalId, australiaVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 28u)
        let walesVsGeorgia = (2023, 10, 07, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 29u, Group GroupC, Confirmed walesId, Confirmed georgiaId, walesVsGeorgia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 29u)
        let fijiVsPortugal = (2023, 10, 08, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 30u, Group GroupC, Confirmed fijiId, Confirmed portugalId, fijiVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 30u)
        // #endregion
        // #region: Group D
        let englandVsArgentina = (2023, 09, 09, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 31u, Group GroupD, Confirmed englandId, Confirmed argentinaId, englandVsArgentina) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 31u)
        let japanVsChile = (2023, 09, 10, 11, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 32u, Group GroupD, Confirmed japanId, Confirmed chileId, japanVsChile) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 32u)
        let samoaVsChile = (2023, 09, 16, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 33u, Group GroupD, Confirmed samoaId, Confirmed chileId, samoaVsChile) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 33u)
        let englandVsJapan = (2023, 09, 17, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 34u, Group GroupD, Confirmed englandId, Confirmed japanId, englandVsJapan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 34u)
        let argentinaVsSamoa = (2023, 09, 22, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 35u, Group GroupD, Confirmed argentinaId, Confirmed samoaId, argentinaVsSamoa) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 35u)
        let englandVsChile = (2023, 09, 23, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 36u, Group GroupD, Confirmed englandId, Confirmed chileId, englandVsChile) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 36u)
        let japanVsSamoa = (2023, 09, 28, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 37u, Group GroupD, Confirmed japanId, Confirmed samoaId, japanVsSamoa) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 37u)
        let argentinaVaChile = (2023, 09, 30, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, Group GroupD, Confirmed argentinaId, Confirmed chileId, argentinaVaChile) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)
        let englandVsSamoa = (2023, 10, 07, 15, 45) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 39u, Group GroupD, Confirmed englandId, Confirmed samoaId, englandVsSamoa) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 39u)
        let japanVsArgentina = (2023, 10, 08, 11, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 40u, Group GroupD, Confirmed japanId, Confirmed argentinaId, japanVsArgentina) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 40u)
        // #endregion
        // #region: Quarter-finals
        let winnerCVsRunnerUpDKO = (2023, 10, 14, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 41u, QuarterFinal 1u, Unconfirmed (StageWinner (Group GroupC)), Unconfirmed (GroupRunnerUp GroupD), winnerCVsRunnerUpDKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 41u)
        let winnerBVsRunnerUpAKO = (2023, 10, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 42u, QuarterFinal 2u, Unconfirmed (StageWinner (Group GroupB)), Unconfirmed (GroupRunnerUp GroupA), winnerBVsRunnerUpAKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 42u)
        let winnerDVsRunnerUpCKO = (2023, 10, 15, 15, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 43u, QuarterFinal 3u, Unconfirmed (StageWinner (Group GroupD)), Unconfirmed (GroupRunnerUp GroupC), winnerDVsRunnerUpCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 43u)
        let winnerAVsRunnerUpBKO = (2023, 10, 15, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 44u, QuarterFinal 4u, Unconfirmed (StageWinner (Group GroupA)), Unconfirmed (GroupRunnerUp GroupB), winnerAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 44u)
        // #endregion
        // #region: Semi-finals
        let winnerQF1VsWinnerQF2KO = (2023, 10, 20, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 45u, SemiFinal 1u, Unconfirmed (StageWinner (QuarterFinal 1u)), Unconfirmed (StageWinner (QuarterFinal 2u)), winnerQF1VsWinnerQF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)
        let winnerQF3VsWinnerQF4KO = (2023, 10, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, SemiFinal 2u, Unconfirmed (StageWinner (QuarterFinal 3u)), Unconfirmed (StageWinner (QuarterFinal 4u)), winnerQF3VsWinnerQF4KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 46u)
        // #endregion
        // #region: "Bronze" final
        let loserSF1VsLoserSF2KO = (2023, 10, 27, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 47u, BronzeFinal, Unconfirmed (SemiFinalLoser 1u), Unconfirmed (SemiFinalLoser 2u), loserSF1VsLoserSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 47u)
        // #endregion
        // #region: Final
        let winnerSF1VsWinnerSF2KO = (2023, 10, 28, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 48u, Final, Unconfirmed (StageWinner (SemiFinal 1u)), Unconfirmed (StageWinner (SemiFinal 2u)), winnerSF1VsWinnerSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 48u)
        // #endregion

        // Note: Reset Fixtures agent [to pendingOnFixturesEventsRead] so that it handles subsequent FixturesEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Fixtures agent" |> Info |> log
        () |> fixtures.Reset
    return () }

let private createInitialDraftsEventsIfNecessary = async {
    let draftsDir = directory EntityType.Drafts

    // #region: Force re-creation of initial Draft/s events if directory already exists (if requested)
    if deleteExistingDraftsEvents && Directory.Exists draftsDir then
        sprintf "deleting existing Draft/s events -> %s" draftsDir |> Info |> log
        delete draftsDir
    // #endregion

    if Directory.Exists draftsDir then sprintf "preserving existing Draft/s events -> %s" draftsDir |> Info |> log
    else
        sprintf "creating initial Draft/s events -> %s" draftsDir |> Info |> log
        "starting Drafts agent" |> Info |> log
        () |> drafts.Start
        // Note: Send dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent to ensure that it transitions [from pendingAllRead] to managingDrafts; otherwise HandleCreateDraftCmdAsync would be ignored (and block).
        "sending dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent" |> Info |> log
        [] |> drafts.OnSquadsRead
        [] |> drafts.OnDraftsEventsRead
        [] |> drafts.OnUserDraftsEventsRead

        let draft1Id, draft1Ordinal = Guid "00000000-0000-0000-0000-000000000001" |> DraftId, DraftOrdinal 1
        let draft1Starts, draft1Ends = (2023, 08, 25, 09, 00) |> dateTimeOffsetUtc, (2023, 08, 31, 19, 00) |> dateTimeOffsetUtc
        let draft1Type = (draft1Starts, draft1Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft1Id, draft1Ordinal, draft1Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft1Id draft1Ordinal)
        let draft2Id, draft2Ordinal = Guid "00000000-0000-0000-0000-000000000002" |> DraftId, DraftOrdinal 2
        let draft2Starts, draft2Ends = (2023, 09, 01, 09, 00) |> dateTimeOffsetUtc, (2023, 09, 05, 19, 00) |> dateTimeOffsetUtc
        let draft2Type = (draft2Starts, draft2Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft2Id, draft2Ordinal, draft2Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft2Id draft2Ordinal)
        let draft3Id, draft3Ordinal = Guid "00000000-0000-0000-0000-000000000003" |> DraftId, DraftOrdinal 3
        let draft3Starts, draft3Ends = (2023, 09, 06, 09, 00) |> dateTimeOffsetUtc, (2023, 09, 07, 19, 00) |> dateTimeOffsetUtc
        let draft3Type = (draft3Starts, draft3Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft3Id, draft3Ordinal, draft3Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft3Id draft3Ordinal)
        let draft4Id, draft4Ordinal = Guid "00000000-0000-0000-0000-000000000004" |> DraftId, DraftOrdinal 4
        let draft4Type = Unconstrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft4Id, draft4Ordinal, draft4Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft4Id draft4Ordinal)

        // Note: Reset Drafts agent [to pendingAllRead] so that it handles subsequent DraftsEventsRead event (&c.) appropriately (i.e. from readPersistedEvents).
        "resetting Drafts agent" |> Info |> log
        () |> drafts.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialSquadsEventsIfNecessary // note: although this can cause various events to be broadcast (SquadsRead | SquadEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialFixturesEventsIfNecessary // note: although this can cause various events to be broadcast (FixturesRead | FixtureEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialDraftsEventsIfNecessary // note: although this can cause various events to be broadcast (DraftsRead | DraftEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
