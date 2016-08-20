module Calendar exposing (display)


import Date
import Date.Extra
import List exposing (..)
import List.Extra
import Maybe


display : Date.Date -> Date.Date -> String
display from to =
  Date.Extra.range Date.Extra.Day 1 from to
    |> groupByMonth
    |> List.map formatGroup
    |> concatenate


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


formatGroup : (Date.Date, List Date.Date) -> (String, List String)
formatGroup (header, dates) =
  let
    formatHeader =
      Date.Extra.toFormattedString "### MMMM y\n\n"

    formatDate =
      Date.Extra.toFormattedString "#### EEEE ddd MMMM y\n\n"

  in
    (formatHeader header, List.map formatDate dates)


concatenate : List (String, List String) -> String
concatenate =
  foldr (\(header, dates) acc -> header ++ foldr (++) "" dates ++ acc) ""
