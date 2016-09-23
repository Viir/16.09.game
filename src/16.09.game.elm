import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


viewportWidth = 400
viewportHeight = 300

playerSize = 30
playerLocationY = viewportHeight - playerSize

-- MODEL

type alias Model = { time : Time, playerLocationX : Int }


init : (Model, Cmd Msg)
init =
  ({time = 0, playerLocationX = 0}, Cmd.none)



-- UPDATE


type Msg
  = Tick Time |
    KeyDown Keyboard.KeyCode

offsetFromKeyCode keyCode =
  case keyCode of
    37 -> -1
    39 -> 1
    _ -> 0

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({time = newTime, playerLocationX = model.playerLocationX}, Cmd.none)
    KeyDown keyCode ->
      ({time = model.time, playerLocationX = model.playerLocationX + (offsetFromKeyCode keyCode)}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Time.every second Tick,
          Keyboard.downs KeyDown
        ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    angle =
      turns (Time.inMinutes model.time)

    handX =
      toString (50 + 40 * cos angle)

    handY =
      toString (50 + 40 * sin angle)

    viewportWidthString = toString viewportWidth
    viewportHeightString = toString viewportHeight

  in
    svg [ viewBox ("0 0 " ++ viewportWidthString ++ " " ++ viewportHeightString), width "800px" ]
      [
          rect [x "0", y "0", width viewportWidthString, height viewportHeightString, fill "black"] [],
          circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] [],
          line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] [],
          rect [
            x (toString ((toFloat model.playerLocationX) + (viewportWidth - playerSize) / 2)),
            y (toString (playerLocationY - playerSize / 2)),
            width (toString playerSize),
            height (toString playerSize),
            fill "DarkGreen"
            ] []
      ]
