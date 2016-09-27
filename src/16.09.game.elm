import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
import Set


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


viewportWidth : number
viewportWidth = 400

viewportHeight : number
viewportHeight = 300

playerShipSize : number
playerShipSize = 30

playerProjectileSize : number
playerProjectileSize = 4

playerShipLocationY : number
playerShipLocationY = viewportHeight - playerShipSize

playerInputFireKeyCode : Int
playerInputFireKeyCode = 32

-- MODEL

type alias PlayerShip =
  { locationX : Int
  }

type alias PlayerProjectile =
  { locationX : Int
  , locationY : Int
  }

type alias Model =
  { time : Time
  , playerShip : PlayerShip
  , setKeyDown : Set.Set Int
  , setPlayerProjectile : List PlayerProjectile
  }


init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , playerShip = {locationX = 0}
    , setKeyDown = Set.empty
    , setPlayerProjectile = []
    }, Cmd.none)



-- UPDATE


type Msg
  = Tick Time
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode

offsetFromKeyCode : Keyboard.KeyCode -> Int
offsetFromKeyCode keyCode =
  case keyCode of
    37 -> -1
    39 -> 1
    _ -> 0

offsetFromSetKeyDown : Set.Set Keyboard.KeyCode -> Int
offsetFromSetKeyDown setKeyDown =
  setKeyDown
  |> Set.toList
  |> List.map offsetFromKeyCode
  |> List.sum

updatePlayerShipLocation : PlayerShip -> Set.Set Keyboard.KeyCode -> PlayerShip
updatePlayerShipLocation playerShip setKeyDown =
  { playerShip | locationX = playerShip.locationX + (offsetFromSetKeyDown setKeyDown)}

updatePlayerProjectile : PlayerProjectile -> PlayerProjectile
updatePlayerProjectile playerProjectile =
  { playerProjectile | locationY = playerProjectile.locationY - 1 }

playerProjectileFromPlayerShip : PlayerShip -> PlayerProjectile
playerProjectileFromPlayerShip playerShip =
  { locationX = playerShip.locationX, locationY = round (playerShipLocationY - playerShipSize / 2)}

updatePlayerShipFire : Model -> List PlayerProjectile
updatePlayerShipFire model =
  let
    fireInput = Set.member playerInputFireKeyCode model.setKeyDown
  in
    if fireInput then [playerProjectileFromPlayerShip model.playerShip] else []

updateModel : Msg -> Model -> Model
updateModel msg model =
  case msg of
    Tick newTime ->
      { model |
         time = newTime
         , playerShip = updatePlayerShipLocation model.playerShip model.setKeyDown
         , setPlayerProjectile =
            model.setPlayerProjectile
            |> List.map updatePlayerProjectile
            |> List.append (updatePlayerShipFire model) }
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

svgFromPlayerProjectile : PlayerProjectile -> Svg a
svgFromPlayerProjectile playerProjectile =
  circle
    [ cx (toString ((toFloat playerProjectile.locationX) + viewportWidth / 2))
    , cy (toString (toFloat playerProjectile.locationY))
    , r (toString (playerProjectileSize / 2))
    , fill "orange"][]

view : Model -> Html Msg
view model =
  let
    playerShip = model.playerShip
    viewportWidthString = toString viewportWidth
    viewportHeightString = toString viewportHeight

    setPlayerProjectileVisual =
      model.setPlayerProjectile
      |> List.map svgFromPlayerProjectile

  in
    svg [ viewBox ("0 0 " ++ viewportWidthString ++ " " ++ viewportHeightString), width "800px" ]
      [
        rect [x "0", y "0", width viewportWidthString, height viewportHeightString, fill "black"] [],
        rect [
          x (toString ((toFloat playerShip.locationX) + (viewportWidth - playerShipSize) / 2)),
          y (toString (playerShipLocationY - playerShipSize / 2)),
          width (toString playerShipSize),
          height (toString playerShipSize),
          fill "DarkGreen"
          ] [],
        g [] setPlayerProjectileVisual,
        g [] [
          text' [x "0", y "10", fill "white", fontFamily "arial", fontSize "10"]
            [text (toString (model.setPlayerProjectile |> List.length))]
        ]
      ]
