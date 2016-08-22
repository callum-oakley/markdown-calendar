import Calendar
import Date
import Date.Extra
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import Result
import String


main =
  App.beginnerProgram
    { model = model
    , view = view
    , update = update
    }



-- MODEL


type alias Model =
  { from : Result String Date.Date
  , to : Result String Date.Date
  , level : Result String Int
  , includeDays : Bool
  , calendar : Result String String
  }


model : Model
model =
  let
    from =
      Ok <| Date.Extra.fromCalendarDate 2017 Date.Jan 1

    to =
      Ok <| Date.Extra.fromCalendarDate 2018 Date.Jan 1

    level =
      Ok 2

    includeDays =
      False

  in
    Model
      from
      to
      level
      includeDays
      (Result.map3 (Calendar.display includeDays) from to level)



-- UPDATE


type Msg
  = From String
  | To String
  | Level String
  | IncludeDays Bool
  | Submit


update : Msg -> Model -> Model
update msg ({ from, to, level, includeDays } as model) =
  case msg of
    From newFrom ->
      { model | from = Date.fromString newFrom }

    To newTo ->
      { model | to = Date.fromString newTo }

    Level newLevel ->
      { model | level = String.toInt newLevel }

    IncludeDays checked ->
      { model | includeDays = checked }

    Submit ->
      { model | calendar =
          Result.map3 (Calendar.display includeDays) from to level
      }



-- VIEW


view : Model -> Html Msg
view ({ from, to, calendar } as model) =
  div []
    [ div []
      [ label [ for "from" ] [ text " from: " ]
      , input
        [ id "from"
        , type' "text"
        , placeholder "yyyy-mm-dd"
        , onInput From
        ] []

      , label [ for "to" ] [ text " to: " ]
      , input
        [ id "to"
        , type' "text"
        , placeholder "yyyy-mm-dd"
        , onInput To
        ] []

      , label [ for "level" ] [ text " header level: " ]
      , input
        [ id "level"
        , type' "number"
        , placeholder "2"
        , Html.Attributes.min "1"
        , Html.Attributes.max "5"
        , size 20
        , onInput Level
        ] []

      , label [ for "days" ] [ text " include days: " ]
      , input
        [ id "days"
        , type' "checkbox"
        , checked False
        , onCheck IncludeDays
        ] []

      , button [ onClick Submit ] [ text "Submit" ]

      ]

    , div []
      [ calendarView calendar
      ]

    ]


calendarView : Result String String -> Html Msg
calendarView calendar =
  let
    (content, colour) =
      case calendar of
        Ok ok ->
          (ok, "black")

        Err err ->
          (err, "red")

  in
    textarea
      [ value content, cols 64, rows 50, style [ ("color", colour) ] ] []
