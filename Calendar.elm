module Calendar exposing (display)


import Date
import Date.Extra
import Date.Extra.Facts
import List exposing (..)
import List.Extra
import Maybe
import String


display : Bool -> Date.Date -> Date.Date -> Int -> String
display includeDays from to level =
  Date.Extra.range Date.Extra.Day 1 from to
    |> groupByMonth
    |> List.map (formatGroup includeDays level)
    |> concatenate level


groupByMonth : List Date.Date -> List (Date.Date, List Date.Date)
groupByMonth dates =
  let
    extractHeader group =
      Maybe.withDefault
        (Date.Extra.fromCalendarDate 1994 Date.Feb 2)
        (head group)

  in
    List.Extra.groupWhile (\x y -> Date.month x == Date.month y) dates
      |> List.map (\group -> (extractHeader group, group))


formatGroup
  :  Bool
  -> Int
  -> (Date.Date, List Date.Date)
  -> (String, List String)

formatGroup includeDays level (header, dates) =
  let
    formatHeader =
      Date.Extra.toFormattedString
        <| String.repeat (level + 1) "#" ++ " MMMM y\n"

    monthOverview' =
      monthOverview (Date.year header) (Date.month header)

    formatDate =
      if includeDays then
        Date.Extra.toFormattedString
          <| String.repeat (level + 2) "#" ++ " EEEE ddd MMMM y\n\n"
      else
        always ""

  in
    (formatHeader header ++ monthOverview', List.map formatDate dates)


monthOverview : Int -> Date.Month -> String
monthOverview year month =
  let
    weekdayNumber =
      Date.Extra.weekdayNumber <| Date.Extra.fromCalendarDate year month 1

    daysInMonth =
      Date.Extra.Facts.daysInMonth year month

    template =
      List.Extra.greedyGroupsOf 7
        <| repeat (weekdayNumber - 1) 0 ++ [1..daysInMonth]

    showDay day =
      if day == 0 then
        "  "
      else if day < 10 then
        " " ++ toString day
      else
        toString day

    showWeek =
        flip (++) "\n"  << String.concat << intersperse " " << map showDay

    showMonth =
      String.concat << map showWeek
  in
    "```\nMo Tu We Th Fr Sa Su\n" ++ showMonth template ++ "```\n\n"


concatenate : Int -> List (String, List String) -> String
concatenate level =
  foldl
    (\(header, days) acc -> acc ++ header ++ foldr (++) "" days)
    (String.repeat level "#" ++ " Calendar\n\n")
