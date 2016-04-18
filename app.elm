import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String
import Debug exposing (..)

main =
  StartApp.start { model = init, view = view, update = update }

-- MODEL

type alias Model =
    { word1 : String
    , word2 : String
    , display : Html
    }

init : Model
init =
  let
    initialWord1 = "abc"
    initialWord2 = "abdc"
  in
    { word1 = initialWord1
    , word2 = initialWord2
    , display = makeDisplay initialWord1 initialWord2
    }


-- VIEW

view : Address Action -> Model -> Html
view address model =
  div []
    [ input
        [ placeholder "Word 1"
        , value model.word1
        , on "input" targetValue (Signal.message address << Word1Update)
        , myStyle
        ]
        []
    , input
        [ placeholder "Word 2"
        , value model.word2
        , on "input" targetValue (Signal.message address << Word2Update)
        , myStyle
        ]
        []
    , div
        []
        [ model.display ]
    ]


-- UPDATE

type Action =
  Word1Update String | Word2Update String


update : Action -> Model -> Model
update action model =
  case action of
    Word1Update t ->
      { model | word1 = t, display = makeDisplay t model.word2 }
    Word2Update t ->
      { model | word2 = t, display = makeDisplay model.word1 t }


getCellValue : Char -> Char -> Maybe Int -> Maybe Int -> Maybe Int -> Int
getCellValue c1 c2 l lt t =
  if c1 == c2 then
    if isNothing l && isNothing lt then
      if isNothing t then 0
      else 1 + (fromJustInt t)
    else if isJust lt then fromJustInt lt
    else 0
  else
    if isJust l && isJust lt && isJust t then
      (Basics.min (fromJustInt l) <| Basics.min (fromJustInt lt) (fromJustInt t)) + 1
    else if isNothing l && isNothing lt && isJust t then
      (fromJustInt t) + 1
    else 0


getRow : Char -> List Char -> List Int -> List Int -> List Int
getRow rowChar topWord rowAbove row =
  let
    colChar = fromJustChar <| List.head topWord
    nextRowAbove = List.drop 1 rowAbove
    nextTopWord = List.drop 1 topWord
    l = List.head <| List.reverse row
    lt = List.head rowAbove
    t = List.head nextRowAbove
    nextValue = getCellValue rowChar colChar l lt t
  in
    if List.isEmpty rowAbove && List.isEmpty row then    -- this makes no sense
      []
    else if List.isEmpty nextRowAbove then               -- row 1 is empty so we've built our row
      row
    else if List.isEmpty row then                        -- The case of the first column
      getRow rowChar topWord rowAbove [1 + (fromJustInt lt)]
    else                                                 -- row 1 still has stuff so keep going
      getRow rowChar nextTopWord nextRowAbove (row ++ [nextValue])


-- Build up the matrix row-by-row going until no rowChars are left
buildMatrix : List (List Int) -> List Char -> List Char -> List Int -> List (List Int)
buildMatrix matrix topWord rowChars prevRow =
  let
    currentChar = fromJustChar <| List.head rowChars
    nextRowChars = List.drop 1 rowChars
    newRow = getRow currentChar topWord prevRow []
  in
    case rowChars of
      [] -> matrix
      _ -> buildMatrix (List.append matrix [newRow]) topWord nextRowChars newRow


getMatrix : String -> String -> List (List String)
getMatrix topWord leftWord =
  let
    topLength = String.length topWord
    leftLength = String.length leftWord
    topList = String.toList topWord
    leftList = String.toList leftWord
    topStringList = stringToStringList topWord
    leftStringList = stringToStringList leftWord
    row1 = List.append [" ", " "] topStringList
    col1 = List.map (\ltr -> [ltr]) (stringToStringList leftWord)
    -- Build inner matrix of values
    row2 = [0..topLength]
    initialInnerMatrix = [row2]
    innerMatrix = buildMatrix initialInnerMatrix topList leftList row2
    innerStringMatrix = List.map (\row -> List.map (\ele -> toString ele) row) innerMatrix
    -- Build display matrix around inner matrix (mainly string conversions)
    row2AsStrings = [" "]++(fromJust <| List.head innerStringMatrix)
    restRows = List.map2 (List.append) col1 (List.drop 1 innerStringMatrix)
  in
    [row1,row2AsStrings]++restRows


makeDisplay : String -> String -> Html
makeDisplay topWord leftWord =
  let
    m = getMatrix topWord leftWord
    toCell letter = td [] [text letter]
    toRow = tr [] << List.map toCell
  in
    table [] (List.map toRow m)


-- STYLE
(=>) = (,)
myStyle : Attribute
myStyle =
  style
    [ "width" => "50%"
    , "height" => "40px"
    , "padding" => "10px 0"
    , "font-size" => "2em"
    , "text-align" => "center"
    ]


-- UTILITIES

stringToStringList : String -> List String
stringToStringList s =
  List.map (\letter -> toString letter) (String.toList s)

isJust : Maybe a -> Bool
isJust m =
  case m of
    Nothing -> False
    Just _  -> True

isNothing : Maybe a -> Bool
isNothing m =
  case m of
    Nothing -> True
    Just _  -> False

fromJust : Maybe a -> a
fromJust x = case x of
  Just y -> y
  Nothing -> Debug.crash "error: fromJust Nothing"

fromJustChar : Maybe Char -> Char
fromJustChar x = case x of
  Just y -> y
  Nothing -> ' '

fromJustInt : Maybe Int -> Int
fromJustInt x = case x of
  Just y -> y
  Nothing -> 0
