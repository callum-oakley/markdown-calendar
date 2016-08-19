module Calendar exposing (..)


import Date exposing (Month(..))
import Date.Extra as Date
import List.Extra
import Time


-- displayCalendar : Date.Date -> Date.Date -> String
-- displayCalendar from to =



dateRange : Date.Date -> Date.Date -> List Date.Date
dateRange from to =
  let
    maybeNext date =
      if next date == to then
        Nothing
      else
        Just (next date)

  in
    List.Extra.iterate maybeNext from


next : Date.Date -> Date.Date
next =
    Date.fromTime << (+) (24 * Time.hour) << Date.toTime


sayDate : Date.Date -> String
sayDate date =
  let
    dayOfWeek =
      sayDayOfWeek <| Date.dayOfWeek date

    day =
      sayDay <| Date.day date

    month =
      sayMonth <| Date.month date

    year =
      toString <| Date.year date

  in
    dayOfWeek ++ " " ++ day ++ " " ++ month ++ " " ++ year


sayDayOfWeek : Date.Day -> String
sayDayOfWeek dayOfWeek =
  case dayOfWeek of
    Date.Mon ->
      "Monday"

    Date.Tue ->
      "Tuesday"

    Date.Wed ->
      "Wednesday"

    Date.Thu ->
      "Thursday"

    Date.Fri ->
      "Friday"

    Date.Sat ->
      "Saturday"

    Date.Sun ->
      "Sunday"


sayDay : Int -> String
sayDay day =
  let
    suffix =
      if day == 1 || day == 21 || day == 31 then
        "st"

      else if day == 2 || day == 22 then
        "nd"

      else if day == 3 || day == 23 then
        "rd"

      else
        "th"

  in
    toString day ++ suffix


sayMonth : Date.Month -> String
sayMonth month =
  case month of
    Date.Jan ->
      "January"

    Date.Feb ->
      "February"

    Date.Mar ->
      "March"

    Date.Apr ->
      "April"

    Date.May ->
      "May"

    Date.Jun ->
      "June"

    Date.Jul ->
      "July"

    Date.Aug ->
      "August"

    Date.Sep ->
      "September"

    Date.Oct ->
      "October"

    Date.Nov ->
      "November"

    Date.Dec ->
      "December"
