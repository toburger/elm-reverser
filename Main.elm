module App.Main (..) where

import Html exposing (Html, Attribute, div, span, button, input, text, hr, h4, dl, dt, dd, label, fieldset, h5)
import Html.Attributes exposing (style, type', value, class, disabled, classList)
import Html.Events exposing (onClick, on, targetValue)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Effects exposing (..)
import Dict exposing (Dict)
import Signal exposing (Signal, Address)
import StartApp
import Task
import String


-- import Debug
---- MODEL ----


type alias Color =
  String


type alias ColorMappings =
  Dict Char Color


type alias Model =
  { text : String
  , reversedText : String
  , colorMappings : ColorMappings
  , selectedChar : Maybe Char
  , selectedColor : String
  }


initialModel : Model
initialModel =
  { text = ""
  , reversedText = ""
  , colorMappings = Dict.fromList [ ( 'a', "red" ), ( 'b', "blue" ) ]
  , selectedChar = Nothing
  , selectedColor = ""
  }



---- UPDATE ----


type Action
  = UpdateText String
  | Reset
  | UpdateSelectedChar Char
  | UpdateSelectedColor String
  | AddColorMapping ( Maybe Char, String )


update : Action -> Model -> Model
update action model =
  case action of
    UpdateText text ->
      { model
        | text = text
        , reversedText = String.reverse text
      }

    Reset ->
      initialModel

    UpdateSelectedChar ' ' ->
      { model | selectedChar = Nothing }

    UpdateSelectedChar char ->
      { model | selectedChar = Just char }

    UpdateSelectedColor color ->
      { model | selectedColor = color }

    AddColorMapping ( Just char, color ) ->
      { model
        | colorMappings = Dict.insert char color model.colorMappings
        , selectedChar = Nothing
        , selectedColor = ""
      }

    AddColorMapping _ ->
      model



---- VIEW ----


onInput : Address a -> (String -> a) -> Attribute
onInput address action =
  on "input" targetValue (Signal.message address << action)


stringView : ColorMappings -> String -> Html
stringView mapping txt =
  txt
    |> String.toList
    |> List.map
        (\c ->
          span
            (textStyle mapping c
              |> Maybe.map (\c -> [ c ])
              |> Maybe.withDefault []
            )
            [ text (String.fromChar c) ]
        )
    |> div []


firstChar : String -> Char
firstChar s =
  s
    -- |> Debug.log "chars"
    |>
      String.toList
    |>
      List.head
    |>
      Maybe.withDefault ' '


addColorMappingView : Address Action -> Maybe Char -> String -> Html
addColorMappingView address selectedChar selectedColor =
  let
    cannotAdd =
      selectedColor == "" || selectedChar == Nothing

    charOrEmpty =
      selectedChar
        |> Maybe.map String.fromChar
        |> Maybe.withDefault ""
  in
    fieldset
      [ class "fieldset" ]
      [ label
          []
          [ text "char"
          , input
              [ type' "text"
              , onInput address (UpdateSelectedChar << firstChar)
              , value charOrEmpty
              ]
              []
          ]
      , label
          []
          [ text "color"
          , input
              [ type' "text"
              , onInput address UpdateSelectedColor
              , value selectedColor
              ]
              []
          ]
      , button
          [ disabled cannotAdd
          , onClick address (AddColorMapping ( selectedChar, selectedColor ))
          , classList
              [ ( "button", True )
              , ( "expanded", True )
              , ( "disabled", cannotAdd )
              ]
          ]
          [ text "add" ]
      ]


colorMappingsView : ColorMappings -> Html
colorMappingsView colorMappings =
  dl
    []
    (Dict.toList colorMappings
      |> List.concatMap
          (\( k, v ) ->
            [ dt [] [ text (String.fromChar k) ]
            , dd [ style [ ( "color", v ) ] ] [ text v ]
            ]
          )
    )


textInput : Address Action -> String -> Html
textInput address text =
  fieldset
    [ class "fieldset" ]
    [ input
        [ type' "text"
        , value text
        , onInput address UpdateText
        ]
        []
    , button
        [ onClick address Reset
        , disabled (text == "")
        , classList
            [ ( "button", True )
            , ( "expanded", True )
            , ( "warning", True )
            , ( "disabled", text == "" )
            ]
        ]
        [ Html.text "reset"
        ]
    ]


view : Address Action -> Model -> Html
view address { text, reversedText, colorMappings, selectedChar, selectedColor } =
  div
    [ class "medium-6 medium-centered large-4 large-centered columns" ]
    [ lazy (textInput address) text
    , hr [] []
    , h5 [] [ Html.text "output:" ]
    , lazy2 stringView colorMappings text
    , lazy2 stringView colorMappings reversedText
    , hr [] []
    , lazy colorMappingsView colorMappings
    , lazy3 addColorMappingView address selectedChar selectedColor
    ]


textStyle : ColorMappings -> Char -> Maybe Attribute
textStyle mapping c =
  mapping
    |> Dict.get c
    |> Maybe.map (\c -> style [ ( "color", c ) ])


actions : List (Signal Action)
actions =
  [ Signal.map (always Reset) dispatchReset
  , dispatchAddColorMapping
      |> Signal.map
          (\( charString, color ) ->
            let
              char =
                if firstChar charString /= ' ' then
                  Just (firstChar charString)
                else
                  Nothing
            in
              AddColorMapping ( char, color )
          )
  ]


app : StartApp.App Model
app =
  StartApp.start
    { init = ( initialModel, Effects.none )
    , update = (\action model -> ( update action model, Effects.none ))
    , view = view
    , inputs = actions
    }


main : Signal Html
main =
  app.html


port dispatchReset : Signal ()
port dispatchAddColorMapping : Signal ( String, String )
port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks



-- port text  : Signal String
-- port text = Signal.dropRepeats (Signal.map .text app.model)


port reversedText : Signal String
port reversedText =
  app.model
    |> Signal.map .reversedText
    |> Signal.dropRepeats



-- |> Signal.map (Debug.log "reversedText")
