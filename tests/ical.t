  $ ../bin/ical_of_timetable.exe --students *.csv 2>&1 |grep -v ignored
  writing "./3info.ics"...
  writing "./3socio.ics"...
  writing "./gui.ics"...
  writing "./lai.ics"...
  writing "./tho.ics"...
  done.
  $ grep -v DTSTAMP 3info.ics
  BEGIN:VCALENDAR
  PRODID:ical_of_timetable
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
  BEGIN:VEVENT
  UID:1
  DTSTART:20201113T110000
  DURATION:PT1H
  RRULE:FREQ=WEEKLY
  LOCATION:A
  SUMMARY:x
  END:VEVENT
  END:VCALENDAR
