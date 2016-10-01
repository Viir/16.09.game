module Vector2Int exposing (..)

type alias Vec2 = { x : Int, y : Int }

vec2 : Int -> Int -> Vec2 
vec2 x y = { x = x, y = y }

add : Vec2 -> Vec2 -> Vec2
add a b = vec2 (a.x + b.x) (a.y + b.y)

