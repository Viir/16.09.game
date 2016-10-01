module ListTool exposing (..)

except : List a -> List a -> List a
except subtrahend minuend =
  minuend
  |> List.filter (\elem -> not (subtrahend |> List.member elem))

elementAtIndex : Int -> List a -> Maybe a
elementAtIndex index list =
  if index < 0 then Nothing
  else if List.length list <= index then Nothing
  else List.drop index list |> List.head

elementAtIndexWrapped : Int -> List a -> Maybe a
elementAtIndexWrapped index list =
  let
    listLength = list |> List.length
    indexWrapped = (((index % listLength) + listLength) % listLength)
  in
    elementAtIndex indexWrapped list
