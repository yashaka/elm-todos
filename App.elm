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
  | Check Int Bool
  | Delete Int


type alias Task =
  { description : String
  , completed : Bool
  , id : Int
  }


type alias Model =
  { tasks : List Task
  , field : String
  , uid : Int
  }


newTask : String -> Int -> Task
newTask desc id =
  { description = desc
  , completed = False
  , id = id
  }


initialModel : Model
initialModel =
  { field = ""
  , tasks = []
  , uid = 0
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
      | uid = model.uid + 1
      , field = ""
      , tasks =
          let
            desc = String.trim model.field
          in
            if String.isEmpty desc
              then model.tasks
              else model.tasks ++ [ newTask desc model.uid ]
      }

    Check id isCompleted ->
      let
        updateTask t =
          if t.id == id then { t | completed = isCompleted } else t
      in
        { model | tasks = List.map updateTask model.tasks }

    Delete id ->
      { model | tasks = List.filter (\t -> t.id /= id) model.tasks }


taskInput : Address Action -> String -> Html
taskInput address desc =
  input
    [ placeholder "What needs to be done?"
    , autofocus True
    , value desc
    , onInput address UpdateField
    , onEnter address Add
    ]
    []


taskList : Address Action -> List Task -> Html
taskList address tasks =
  ul [] (List.map (todoItem address) tasks)


todoItem : Address Action -> Task -> Html
todoItem address todo =
  li
    [ key (toString todo.id)
    , class "todo-item"
    ]
    [ input
        [ type' "checkbox"
        , onClick address (Check todo.id (not todo.completed))
        ]
        []
    , span
        [ classList [ ("is-completed", todo.completed) ]
        ]
        [ text todo.description ]
    , button
        [ onClick address (Delete todo.id)
        , class "destroy"
        ]
        [ text "x" ]
    ]


view : Address Action -> Model -> Html
view address model =
  div []
    [ taskInput address model.field
    , taskList address model.tasks
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
