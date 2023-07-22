module Aornota.Sweepstake2023.Ui.Theme.Shared

open Aornota.Sweepstake2023.Ui.Theme.Light
open Aornota.Sweepstake2023.Ui.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeLight else themeDark
