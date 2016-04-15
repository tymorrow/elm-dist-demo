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
    { word1 = ""
    , word2 = ""
    , display = text ""
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

getMatrix : String -> String -> List (List Char)
getMatrix a b =
  let
    al = String.length a
    bl = String.length b
    alist = String.toList a
    blist = String.toList b
    row1 = [' ', ' '] ++ alist
    row2 = [' ', '0'] ++ (List.repeat al '0')
    f letter = [letter, '0'] ++ (List.repeat al '0')
    brows = List.map f blist
  in
    [row1, row2] ++ brows

fillMatrix : List (List Char) -> List (List Char)
fillMatrix m =
  m

makeDisplay : String -> String -> Html
makeDisplay a b =
  let
    m = getMatrix a b
    dm = fillMatrix m
    toCell letter = td [] [text <| toString letter]
    toRow = tr [] << List.map toCell
  in
    table [] (List.map toRow dm)


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
