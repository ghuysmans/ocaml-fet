# ocaml-fet

This library allows reading and writing CSV files for FET, a free and open-source school scheduling software. An iCalendar conversion tool is provided as an illustration.

```sh
$ bin/ical_of_timetable.exe --help
NAME
       ical_of_timetable - generate iCal schedules using CSV files exported
       from FET

SYNOPSIS
       ical_of_timetable [OPTION]... timetable.csv

OPTIONS
       -c, --show-classes
           show classes in student schedules

       -d VAL, --duration=VAL (absent=60)
           slot duration (in minutes) when Hour isn't an hh:mm-hh:mm range

       --from=VAL
           start from a given date

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --no-groups
           don't generate group schedules

       --no-subgroups
           don't generate subgroup schedules

       --only=VAL
           generate a single schedule

       --output-dir=VAL (absent=.)
           output directory

       -r, --rooms
           generate room schedules

       -s, --students
           generate student schedules

       -T VAL, --timezone=VAL (absent=Europe/Brussels)
           timezone

       -t, --teachers
           generate teacher schedules

       -u VAL, --until=VAL, --repeat-until=VAL
           repeat events weekly before a given date

```
