module Calendar exposing (display)


import Date
import Date.Extra
import List exposing (..)
import List.Extra
import Maybe
import String


display : Date.Date -> Date.Date -> Int -> String
display from to level =
  Date.Extra.range Date.Extra.Day 1 from to
    |> groupByMonth
    |> List.map (formatGroup level)
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


formatGroup : Int -> (Date.Date, List Date.Date) -> (String, List String)
formatGroup level (header, dates) =
  let
    formatHeader =
      Date.Extra.toFormattedString
        <| String.repeat (level + 1) "#" ++ " MMMM y\n\n"

    formatDate =
      Date.Extra.toFormattedString
        <| String.repeat (level + 2) "#" ++ " EEEE ddd MMMM y\n\n"

  in
    (formatHeader header, List.map formatDate dates)


concatenate : Int -> List (String, List String) -> String
concatenate level =
  foldl (\(header, dates) acc -> acc ++ header ++ foldr (++) "" dates) (String.repeat level "#" ++ " Calendar\n\n")
