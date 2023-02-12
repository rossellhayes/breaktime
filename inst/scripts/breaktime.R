#!/usr/bin/env Rscript

'breaktime

Usage:
  breaktime.R [-w <m>] [-b <m>] [-l <m>] [-s <str> | -q] [--start_color <str>] [--end_color <str>]
  breaktime.R pomodoro [-w <m>] [-b <m>] [-l <m>] [-s <str> | -q] [--start_color <str>] [--end_color <str>]
  breaktime.R (-h | --help)

Options:
  -h, --help                Show this screen.

  -w <m>, --work <m>        The length in minutes of each working session.
                            Defaults to 25.

  -b <m>, --break <m>       The length in minutes of each short break.
                            Defaults to one-fifth of the length of the
                            previous working session.

  -l <m>, --long_break <m>  The length in minutes of each long break.
                            Long breaks occur every fourth break.
                            Defaults to one-fifth of the length of the
                            previous working session, plus the length of
                            the previous three short breaks.

  -s <str>, --sound <str>   The sound played by `beepr::beep()` when a
                            session ends. Defaults to "ping". If you do not
                            interact with breaktime after a session has ended,
                            the sound will play again each time one-fifth
                            of the target time passes (for example, if a
                            session lasts 25 minutes, a reminder sound will
                            play every 5 minutes after the session ends).

  -q, --quiet               Disable sound.

  --start_color <str>       Color used to display the time. The time
                            is displayed in start_color when there is
                            time remaining in the current session.
                            Color specifications are handled by
                            `cli::make_ansi_style()`.

  --end_color <str>         Color used to display the time. The time is
                            displayed in end_color when the current session
                            has expired. Color specifications are handled
                            by `cli::make_ansi_style()`.

' -> doc

args <- docopt::docopt(doc, version = 'breaktime')

fun <- if (args[["pomodoro"]]) {
  breaktime::pomodoro
} else {
  breaktime::timer
}

args["work_time"] <- args["work"]
args["break_time"] <- args["break"]
args["long_break_time"] <- args["long_break"]

args <- args[
  names(args) %in% c(
    "work_time", "break_time", "long_break_time",
    "quiet", "sound",
    "start_color", "end_color"
  )
]

args <- args[lengths(args) > 0]

if (args[["quiet"]]) {
  args["sound"] <- list(NULL)
}

args[["quiet"]] <- NULL

do.call(breaktime::timer, args)
