(datatype days
  if (element? Day [monday tuesday wednesday thursday friday saturday sunday])
  ____________________________________________________________________________
  Day : day;)

(define next-day
  { day --> day }
  monday -> tuesday
  tuesday -> wednesday
  wednesday -> thursday
  thursday -> friday
  friday -> saturday
  saturday -> sunday
  sunday -> monday)
