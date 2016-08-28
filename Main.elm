import Calendar
import Date
import Date.Extra
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import Result
import String
import Task


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { from : Result String Date.Date
  , to : Result String Date.Date
  , level : Result String Int
  , includeDays : Bool
  , calendar : Result String String
  }


init : (Model, Cmd Msg)
init =
  let
    from =
      Err ""

    to =
      Err ""

    level =
      Ok 2

    includeDays =
      False

    model =
      Model
        from
        to
        level
        includeDays
        (Result.map3 (Calendar.display includeDays) from to level)

  in
    (model, Task.perform (always NoOp) CurrentDate Date.now)



-- UPDATE


type Msg
  = From String
  | To String
  | Level String
  | IncludeDays Bool
  | Submit
  | CurrentDate Date.Date
  | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ from, to, level, includeDays } as model) =
  case msg of
    From newFrom ->
      ( { model | from = Date.fromString newFrom }
      , Cmd.none
      )

    To newTo ->
      ( { model | to = Date.fromString newTo }
      , Cmd.none
      )

    Level newLevel ->
      ( { model | level = String.toInt newLevel }
      , Cmd.none
      )

    IncludeDays checked ->
      ( { model | includeDays = checked }
      , Cmd.none
      )

    Submit ->
      ( { model | calendar = updateCalendar from to level includeDays }
      , Cmd.none
      )

    CurrentDate date ->
      let
        newFrom =
          Ok <| date

        newTo =
          Ok <| Date.Extra.add Date.Extra.Year 2 date

        newCalendar =
          updateCalendar newFrom newTo level includeDays

      in
        ( { model | from = newFrom, to = newTo, calendar = newCalendar }
        , Cmd.none
        )

    NoOp ->
      ( model
      , Cmd.none
      )


updateCalendar
  :  Result String Date.Date
  -> Result String Date.Date
  -> Result String Int
  -> Bool
  -> Result String String
updateCalendar from to level includeDays =
  Result.map3 (Calendar.display includeDays) from to level



-- SUBSCRIPTIONS


subscriptions :  Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view ({ from, to, calendar } as model) =
  div [ class "wrapper" ]
    [ div [ class "form-inline header" ]
      [ div [ class "form-group" ]
        [ label [ for "from" ] [ text "From" ]
        , input
          [ id "from"
          , class "form-control"
          , type' "text"
          , placeholder "yyyy-mm-dd"
          , onInput From
          ] []

        ]

      , div [ class "form-group" ]
        [ label [ for "to" ] [ text "To" ]
        , input
          [ id "to"
          , class "form-control"
          , type' "text"
          , placeholder "yyyy-mm-dd"
          , onInput To
          ] []

        ]

      , div [ class "form-group" ]
        [ label [ for "level" ] [ text "Header level" ]
        , input
          [ id "level"
          , class "form-control"
          , type' "number"
          , placeholder "2"
          , Html.Attributes.min "1"
          , Html.Attributes.max "5"
          , size 20
          , onInput Level
          ] []

        ]

      , div [ class "checkbox" ]
        [ label []
        [ input
          [ type' "checkbox"
          , checked False
          , onCheck IncludeDays
          ] []
          , text " Include days "
          ]

        ]

      , button [ class "btn btn-default", onClick Submit ] [ text "Submit" ]
      ]

    , div [ class "calendar"]
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
      [ value content
      , readonly True
      , class "form-control"
      , style [ ("color", colour) ]
      ] []
