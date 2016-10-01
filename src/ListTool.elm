module ListTool exposing (..)

except : List a -> List a -> List a
except subtrahend minuend =
  minuend
  |> List.filter (\elem -> not (subtrahend |> List.member elem))
