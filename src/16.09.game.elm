import Html exposing (Html)
import Html.App as Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Keyboard
import Set
import Vector2Int as Vec2
import ListTool


main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


viewportWidth : Int
viewportWidth = 400

viewportHeight : Int
viewportHeight = 300

playerShipSize : Int
playerShipSize = 30

playerShipWeaponReloadDelay : Int
playerShipWeaponReloadDelay = 100

playerProjectileSize : number
playerProjectileSize = 4

playerProjectileSpeed : Vec2.Vec2
playerProjectileSpeed = Vec2.vec2 0 -4

playerShipLocationY : Int
playerShipLocationY = viewportHeight - playerShipSize

playerInputFireKeyCode : Int
playerInputFireKeyCode = 32

enemySize : number
enemySize = 16

-- MODEL

type alias PlayerShip =
  { locationX : Int
  , fireLastTime : Int
  }

type alias PlayerProjectile = Vec2.Vec2

type alias Enemy = Vec2.Vec2


type alias Model =
  { time : Time
  , playerShip : PlayerShip
  , playerScore : Int
  , setKeyDown : Set.Set Int
  , setPlayerProjectile : List PlayerProjectile
  , setEnemy : List Enemy
  }

listEnemyLocationSeed : List Int
listEnemyLocationSeed = [ 2342, 6672, 8561, 4729, 9394 ]

enemySpreadWith : Int
enemySpreadWith = (viewportWidth * 3) // 4

listEnemy : List Enemy
listEnemy = [0..99] |> List.map (\enemyIndex ->
  { x = (Maybe.withDefault 0 (ListTool.elementAtIndexWrapped enemyIndex listEnemyLocationSeed) * (enemyIndex % 7)) % enemySpreadWith - enemySpreadWith // 2
  , y = -enemyIndex * 100 })

init : (Model, Cmd Msg)
init =
  (
    { time = 0
    , playerShip = {locationX = 0, fireLastTime = 0}
    , playerScore = 0
    , setKeyDown = Set.empty
    , setPlayerProjectile = []
    , setEnemy = listEnemy
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

updateModelSequence : List (Model -> Model) -> Model -> Model
updateModelSequence sequence model =
 case sequence of
    [] -> model
    head::tail -> updateModelSequence tail (head model)

updatePlayerShipLocation : PlayerShip -> Set.Set Keyboard.KeyCode -> PlayerShip
updatePlayerShipLocation playerShip setKeyDown =
  { playerShip | locationX = playerShip.locationX + (offsetFromSetKeyDown setKeyDown)}

updatePlayerProjectile : PlayerProjectile -> Maybe PlayerProjectile
updatePlayerProjectile playerProjectile =
  if playerProjectile.y < 0 then Nothing else
    Just (Vec2.add playerProjectile playerProjectileSpeed)

updateSetPlayerProjectile : Model -> Model
updateSetPlayerProjectile model =
  { model | setPlayerProjectile =
      model.setPlayerProjectile
      |> List.map updatePlayerProjectile
      |> List.filterMap identity }

playerProjectileFromPlayerShip : PlayerShip -> PlayerProjectile
playerProjectileFromPlayerShip playerShip =
  Vec2.vec2 playerShip.locationX (playerShipLocationY - playerShipSize // 2)

updatePlayerShipFire : Model -> Model
updatePlayerShipFire model =
  let
    playerShip = model.playerShip
    fireInput = Set.member playerInputFireKeyCode model.setKeyDown
    fireLastAge = round model.time - playerShip.fireLastTime 
    weaponReady = playerShipWeaponReloadDelay <= fireLastAge
    fire = fireInput && weaponReady
    setProjectileNew = if fire then [playerProjectileFromPlayerShip model.playerShip] else []
    playerShipNew = if fire then { playerShip | fireLastTime = round model.time } else playerShip 
  in
    {model
      | setPlayerProjectile = model.setPlayerProjectile |> List.append setProjectileNew
      , playerShip = playerShipNew
      }

updatePlayerShip : Model -> Model
updatePlayerShip model =
  updatePlayerShipFire
      { model | playerShip = updatePlayerShipLocation model.playerShip model.setKeyDown }

updateCollision : Model -> Model
updateCollision model =
  let
    setCollision =
      model.setEnemy
      |> List.map (\enemy ->
        (enemy, model.setPlayerProjectile
          |> List.filterMap (\projectile ->
            if Vec2.length (Vec2.sub enemy projectile) < ((enemySize + playerProjectileSize) // 2)
              then Just projectile
              else Nothing))
        )

    setEnemyDestroy =
      setCollision
      |> List.filterMap (\(enemy, setProjectile) -> if 0 < (setProjectile |> List.length) then Just enemy else Nothing)

    setEnemy  =
      model.setEnemy |> ListTool.except setEnemyDestroy

    setProjectileCollided =
      setCollision |> List.map snd |> List.concat

    setPlayerProjectile =
      model.setPlayerProjectile |> List.filter (\projectile -> not (List.member projectile setProjectileCollided))
  in
    { model
      | setEnemy = setEnemy
      , setPlayerProjectile = setPlayerProjectile
      , playerScore = model.playerScore + (setEnemyDestroy |> List.length) }

updateSetEnemy : Model -> Model
updateSetEnemy model =
  { model | setEnemy = model.setEnemy |> List.map (\enemy -> Vec2.add enemy (Vec2.vec2 0 1))}

updateModel : Msg -> Model -> Model
updateModel msg model =
  case msg of
    Tick newTime ->
      updateModelSequence
        [ updatePlayerShip
        , updateSetPlayerProjectile
        , updateCollision
        , updateSetEnemy
        ] { model | time = newTime}
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

svgCircleFromLocation : String -> number -> Vec2.Vec2 -> Svg a
svgCircleFromLocation fillColorString radius location =
  circle
    [ cx (toString (location.x + viewportWidth // 2))
    , cy (toString location.y)
    , r (toString radius)
    , fill fillColorString][]

svgFromPlayerProjectile : PlayerProjectile -> Svg a
svgFromPlayerProjectile playerProjectile =
  svgCircleFromLocation "orange" (playerProjectileSize / 2) playerProjectile

svgFromEnemy : Enemy -> Svg a
svgFromEnemy enemy =
  svgCircleFromLocation "grey" (enemySize / 2) enemy

view : Model -> Html Msg
view model =
  let
    playerShip = model.playerShip
    viewportWidthString = toString viewportWidth
    viewportHeightString = toString viewportHeight

    setPlayerProjectileVisual =
      model.setPlayerProjectile
      |> List.map svgFromPlayerProjectile

    setEnemyVisual =
      model.setEnemy
      |> List.map svgFromEnemy

  in
    svg [ viewBox ("0 0 " ++ viewportWidthString ++ " " ++ viewportHeightString), width "800px" ]
      [
        rect [x "0", y "0", width viewportWidthString, height viewportHeightString, fill "black"] [],
        rect [
          x (toString (playerShip.locationX + (viewportWidth - playerShipSize) // 2)),
          y (toString (playerShipLocationY - playerShipSize // 2)),
          width (toString playerShipSize),
          height (toString playerShipSize),
          fill "DarkGreen"
          ] [],
        g [] setEnemyVisual,
        g [] setPlayerProjectileVisual,
        g [] [
          text' [x "0", y "10", fill "white", fontFamily "arial", fontSize "10"]
            [text ("score: " ++ (toString model.playerScore))]
        ]
      ]
