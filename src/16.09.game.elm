import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
import Set



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

type alias PlayerShip =
  { locationX : Int
  }

type alias Model =
  { time : Time
  , playerShip : PlayerShip
  , setKeyDown : Set.Set Int
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , playerShip = {locationX = 0}
    , setKeyDown = Set.empty
    }, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode

offsetFromKeyCode keyCode =
  case keyCode of
    37 -> -1
    39 -> 1
    _ -> 0

offsetFromSetKeyDown setKeyDown =
  setKeyDown
  |> Set.toList
  |> List.map offsetFromKeyCode
  |> List.sum

playerShipUpdate playerShip setKeyDown =
  { playerShip | locationX = playerShip.locationX + (offsetFromSetKeyDown setKeyDown)}

updateModel msg model =
  case msg of
    Tick newTime ->
      { model | time = newTime, playerShip = playerShipUpdate model.playerShip model.setKeyDown }
    KeyDown keyCode ->
      { model | setKeyDown = Set.insert keyCode model.setKeyDown }
    KeyUp keyCode ->
      { model | setKeyDown = Set.remove keyCode model.setKeyDown }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updateModel msg model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
          Time.every (16 * Time.millisecond) Tick
          , Keyboard.downs KeyDown
          , Keyboard.ups KeyUp
        ]



-- VIEW


view : Model -> Html Msg
view model =
  let
    playerShip = model.playerShip
    viewportWidthString = toString viewportWidth
    viewportHeightString = toString viewportHeight

  in
    svg [ viewBox ("0 0 " ++ viewportWidthString ++ " " ++ viewportHeightString), width "800px" ]
      [
          rect [x "0", y "0", width viewportWidthString, height viewportHeightString, fill "black"] [],
          rect [
            x (toString ((toFloat playerShip.locationX) + (viewportWidth - playerSize) / 2)),
            y (toString (playerLocationY - playerSize / 2)),
            width (toString playerSize),
            height (toString playerSize),
            fill "DarkGreen"
            ] []
      ]
