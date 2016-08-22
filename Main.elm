import Calendar
import Date
import Date.Extra
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
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
  , calendar : Result String String
  }


model : Model
model =
  let
    from =
      Ok <| Date.Extra.fromCalendarDate 2017 Date.Jan 1

    to =
      Ok <| Date.Extra.fromCalendarDate 2018 Date.Jan 1

  in
    Model from to (Result.map2 Calendar.display from to)



-- UPDATE


type Msg
  = From String
  | To String
  | Submit


update : Msg -> Model -> Model
update msg model =
  case msg of
    From newFrom ->
      { model | from = Date.fromString newFrom }

    To newTo ->
      { model | to = Date.fromString newTo }

    Submit ->
      { model | calendar = Result.map2 Calendar.display model.from model.to }



-- VIEW


view : Model -> Html Msg
view ({ from, to, calendar } as model) =
  div []
    [ input [ type' "text", placeholder "from (yyyy-mm-dd)", onInput From ] []
    , input [ type' "text", placeholder "to (yyyy-mm-dd)", onInput To ] []
    , button [ onClick Submit ] [ text "Submit" ]
    , p [] []
    , calendarView calendar
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
      [ value content, cols 64, rows 50, style [ ("color", colour) ] ]
      [ ]
