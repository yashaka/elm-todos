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


pluralize : String -> String -> Int -> String
pluralize singular plural n =
  if n == 1 then singular else plural


type Action
  = NoOp
  | UpdateField String
  | Add
  | Check Int Bool
  | Delete Int
  | DeleteComplete


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

    DeleteComplete ->
      { model | tasks = List.filter (not << .completed) model.tasks }


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


footer : Address Action -> List Task -> Html
footer address tasks =
  let
    completedTasks = List.filter .completed tasks
    numTasks = List.length tasks
    numCompletedTasks = List.length completedTasks
    numRemainingTasks = numTasks - numCompletedTasks
  in
    div []
      [ itemsRemaining numTasks numRemainingTasks
      , clearCompleted address numCompletedTasks
      ]


itemsRemaining : Int -> Int -> Html
itemsRemaining m n =
  if m == 0 then
    div [] []
  else
    text <| (toString n) ++ " " ++ (pluralize "item" "items" n)  ++ " remaining"


clearCompleted : Address Action -> Int -> Html
clearCompleted address n =
  if n == 0 then
    div [] []
  else
    div []
      [ button [ onClick address DeleteComplete ]
          [ text "Clear completed" ]
      ]


view : Address Action -> Model -> Html
view address model =
  div []
    [ taskInput address model.field
    , taskList address model.tasks
    , footer address model.tasks
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
