# ocaml-fet

This library allows reading and writing CSV files for FET, a free and open-source school scheduling software. An iCalendar conversion tool is provided as an illustration.

```
NAME
       ical_of_timetable - generate iCal schedules using CSV files exported from FET

SYNOPSIS
       ical_of_timetable [OPTION]... CSV...

OPTIONS
       -1, --once
           don't repeat courses weekly

       -c, --show-classes
           show classes in student schedules

       -d VAL, --duration=VAL (absent=60)
           slot duration (in minutes) when Hour isn't an hh:mm-hh:mm range

       -f VAL, --first-day=VAL
           first day

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --only=VAL
           generate a single schedule

       --output-dir=VAL (absent=.)
           output directory

       -r, --rooms
           generate room schedules

       -s, --students
           generate student schedules

       -t, --teachers
           generate teacher schedules
```
