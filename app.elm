import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import StartApp.Simple as StartApp
import String

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
    { word1 = "abcd"
    , word2 = "abced"
    , display = makeDisplay "abcd" "abced"
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


fromJust : Maybe a -> a
fromJust x = case x of
  Just y -> y
  Nothing -> Debug.crash "error: fromJust Nothing"


getCellValue : Char -> Char -> Int -> Int -> Int -> Int
getCellValue c1 c2 l lt t =
    if c1 == c2 then lt
    else
      Basics.min l <| Basics.min lt t


getRow : Char -> List Char -> List Int -> List Int -> List Int
getRow rowChar word row1 row2 =
  let
    colChar = fromJust <| List.head word
    nextRow1 = List.drop 1 row1
    nextWord = List.drop 1 word
    l = fromJust <| List.head <| List.reverse row2
    lt = fromJust <| List.head row1
    t = fromJust <| List.head nextRow1
    nextValue = getCellValue rowChar colChar l lt t
  in
    if List.isEmpty row1 && List.isEmpty row2 then    -- this makes no sense
      []
    else if List.isEmpty row1 then                    -- row 1 is empty so we've built our row
      row2
    else if List.isEmpty row2 then                    -- The case of the first column
      getRow rowChar word row1 [1 + lt]
    else                                              -- row 1 still has stuff so keep going
      getRow rowChar nextWord nextRow1 (row2 ++ [nextValue])


getInnerMatrix : String -> String -> List (List Int)
getInnerMatrix a b =
  let
    al = String.length a
    bl = String.length b
    row1 = [0..al]
    col1 = [1..bl]
    f val = [val] ++ (List.repeat al 0)
    m = List.map f col1
  in
    [row1]++m


stringToStringList : String -> List String
stringToStringList s =
  List.map (\letter -> toString letter) (String.toList s)


getMatrix : String -> String -> List (List String)
getMatrix a b =
  let
    al = String.length a
    alist = stringToStringList a
    row1 = List.append [" ", " "] alist
    col1 = List.map (\ltr -> [ltr]) (stringToStringList b)
    -- Build first row
    innerMatrix = getInnerMatrix a b
    innerStringMatrix = List.map (\row -> List.map (\ele -> toString ele) row) innerMatrix
    -- Build display matrix around inner matrix
    row2 = [" "]++(fromJust <| List.head innerStringMatrix)
    restRows = List.map2 (List.append) col1 (List.drop 1 innerStringMatrix)
  in
    [row1,row2]++restRows


makeDisplay : String -> String -> Html
makeDisplay a b =
  let
    m = getMatrix a b
    -- dm = fillMatrix m 1 1
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
