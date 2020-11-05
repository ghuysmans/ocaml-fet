  $ ../bin/ical_of_timetable.exe --students *.csv 2>&1 |grep -v ignored
  ./3info.ics
  ./3socio.ics
  ./gui.ics
  ./lai.ics
  ./tho.ics
  $ grep -v DTSTAMP 3info.ics
  BEGIN:VCALENDAR
  PRODID:ical_of_timetable
  VERSION:2.0
  X-WR-TIMEZONE:Europe/Brussels
  BEGIN:VEVENT
  UID:2
  DTSTART:20201109T110000
  DURATION:PT1H
  RRULE:FREQ=WEEKLY
  LOCATION:B
  SUMMARY:x
  END:VEVENT
  END:VCALENDAR
  $ grep -v DTSTAMP 3socio.ics
  BEGIN:VCALENDAR
  PRODID:ical_of_timetable
  VERSION:2.0
  X-WR-TIMEZONE:Europe/Brussels
  BEGIN:VEVENT
  UID:1
  DTSTART:20201113T110000
  DURATION:PT1H
  RRULE:FREQ=WEEKLY
  LOCATION:A
  SUMMARY:x
  END:VEVENT
  END:VCALENDAR
