module App where

import Graphics.Element exposing ( show )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Signal exposing ( Address )
import String

onInput : Address a -> (String -> a) -> Attribute
onInput address f =
  on "input" targetValue (Signal.message address << f)


onEnter : Address a -> a -> Attribute
onEnter address value =
  let
    isEnter code =
      if code == 13 then Ok () else Err "incorrect key code"
  in
    on "keydown"
      (Json.customDecoder keyCode isEnter)
      (\_ -> Signal.message address value)


type Action
  = NoOp
  | UpdateField String
  | Add


type alias Model =
  { todos : List String
  , field : String
  }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    UpdateField str ->
      { model | field = str }

    Add ->
      { model
      | field = ""
      , todos =
          let
            cleanField = String.trim model.field
          in
            if String.isEmpty cleanField
              then model.todos
              else model.todos ++ [ cleanField ]
      }


initialModel : Model
initialModel =
  { field = ""
  , todos = []
  }


todoInput : Address Action -> String -> Html
todoInput address todo =
  input
    [ placeholder "What needs to be done?"
    , autofocus True
    , value todo
    , onInput address UpdateField
    , onEnter address Add
    ]
    []


todoList : List String -> Html
todoList todos =
  ul [] (List.map todoListItem todos)


todoListItem : String -> Html
todoListItem todo =
  li [] [ text todo ]


view : Address Action -> Model -> Html
view address model =
  div []
    [ todoInput address model.field
    , todoList model.todos
    -- Useful for debugging purposes
    -- , fromElement (show model)
    ]


inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp


model : Signal Model
model =
  Signal.foldp update initialModel inbox.signal


main : Signal Html
main =
  Signal.map (view inbox.address) model
