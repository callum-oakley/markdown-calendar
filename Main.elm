import Calendar
import Date
import Date.Extra
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Result


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
  }


model : Model
model =
  Model
    (Ok <| Date.Extra.fromCalendarDate 2017 Date.Jan 1)
    (Ok <| Date.Extra.fromCalendarDate 2018 Date.Jan 1)



-- UPDATE


type Msg
  = From String
  | To String


update : Msg -> Model -> Model
update msg model =
  case msg of
    From newFrom ->
      { model | from = Date.fromString newFrom }

    To newTo ->
      { model | to = Date.fromString newTo }



-- VIEW


view : Model -> Html Msg
view ({ from, to } as model) =
  div []
    [ input [ type' "text", placeholder "from (yyyy-mm-dd)", onInput From ] []
    , input [ type' "text", placeholder "to (yyyy-mm-dd)", onInput To ] []
    , p [] []
    , span [ style [ ("white-space", "pre-wrap") ] ] [text (calendarView model)]
    ]


calendarView : Model -> String
calendarView { from, to } =
  let
    calendarOrErr = Result.map2 Calendar.display from to

  in
    case calendarOrErr of
      Ok calendar ->
        calendar

      Err _ ->
        "Please enter two dates conforming to ISO 8601."
