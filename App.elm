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


type alias Visibility = String


type Action
  = NoOp
  | UpdateField String
  | EditingTask Int Bool
  | UpdateTask Int String
  | Add
  | Check Int Bool
  | CheckAll Bool
  | Delete Int
  | DeleteComplete
  | SetVisibilityFilter Visibility


type alias Task =
  { description : String
  , completed : Bool
  , editing : Bool
  , id : Int
  }


type alias Model =
  { tasks : List Task
  , field : String
  , uid : Int
  , visibilityFilter : Visibility
  }


newTask : String -> Int -> Task
newTask desc id =
  { description = desc
  , completed = False
  , editing = False
  , id = id
  }


emptyModel : Model
emptyModel =
  { field = ""
  , tasks = []
  , uid = 0
  , visibilityFilter = "All"
  }


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    UpdateField str ->
      { model | field = str }

    EditingTask id isEditing ->
      let
        updateTask t = if t.id == id then { t | editing = isEditing } else t
      in
        { model | tasks = List.map updateTask model.tasks }

    UpdateTask id newDesc ->
      let
        updateTask t = if t.id == id then { t | description = newDesc } else t
      in
        { model | tasks = List.map updateTask model.tasks }

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

    CheckAll isCompleted ->
      let
        updateTask t = { t | completed = isCompleted }
      in
        { model | tasks = List.map updateTask model.tasks }

    Delete id ->
      { model | tasks = List.filter (\t -> t.id /= id) model.tasks }

    DeleteComplete ->
      { model | tasks = List.filter (not << .completed) model.tasks }

    SetVisibilityFilter visibilityFilter ->
      { model | visibilityFilter = visibilityFilter }


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


markAllCompleted : Address Action -> List Task -> Html
markAllCompleted address tasks =
  let
    allCompleted = List.all .completed tasks
  in
    if List.isEmpty tasks then
      div [] []
    else
      div []
        [ input
            [ id "toggle-all"
            , type' "checkbox"
            , checked allCompleted
            , onClick address (CheckAll (not allCompleted))
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as completed" ]
        ]


taskList : Address Action -> Visibility -> List Task -> Html
taskList address visibilityFilter tasks =
  let
    isVisible todo =
      case visibilityFilter of
        "Completed" -> todo.completed
        "Active" -> not todo.completed
        _ -> True

  in
    ul [] (List.map (todoItem address) (List.filter isVisible tasks))


todoItem : Address Action -> Task -> Html
todoItem address todo =
  li
    [ key (toString todo.id)
    , class "todo-item"
    ]
    [ if todo.editing then
        todoItemEdit address todo
      else
        todoItemView address todo
    ]


todoItemView : Address Action -> Task -> Html
todoItemView address todo =
  div []
    [ input
        [ type' "checkbox"
        , checked todo.completed
        , onClick address (Check todo.id (not todo.completed))
        ]
        []
    , span
        [ classList [ ("is-completed", todo.completed) ]
        , onDoubleClick address (EditingTask todo.id True)
        ]
        [ text todo.description ]
    , button
        [ onClick address (Delete todo.id)
        , class "destroy"
        ]
        [ text "x" ]
    ]


todoItemEdit : Address Action -> Task -> Html
todoItemEdit address todo =
  div []
    [ input
        [ id ("todo-" ++ toString todo.id)
        , value todo.description
        , onInput address (UpdateTask todo.id)
        , onBlur address (EditingTask todo.id False)
        , onEnter address (EditingTask todo.id False)
        ]
        []
    ]


footer : Address Action -> Visibility -> List Task -> Html
footer address visibilityFilter tasks =
  let
    completedTasks = List.filter .completed tasks
    numTasks = List.length tasks
    numCompletedTasks = List.length completedTasks
    numRemainingTasks = numTasks - numCompletedTasks
  in
    div []
      [ itemsRemaining numTasks numRemainingTasks
      , clearCompleted address numCompletedTasks
      , if numTasks == 0 then
          div [] []
        else
          div []
            [ visibilityFilterLink address "#/" "All" visibilityFilter
            , text " | "
            , visibilityFilterLink address "#/active" "Active" visibilityFilter
            , text " | "
            , visibilityFilterLink address "#/completed" "Completed" visibilityFilter
            ]
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


visibilityFilterLink : Address Action -> String -> Visibility -> Visibility -> Html
visibilityFilterLink address fragmentId visibilityFilter currentVisibilityFilter =
  if visibilityFilter == currentVisibilityFilter then
    span [] [ text visibilityFilter ]
  else
    a
      [ onClick address (SetVisibilityFilter visibilityFilter)
      , href fragmentId
      ]
      [ text visibilityFilter ]


view : Address Action -> Model -> Html
view address model =
  div []
    [ taskInput address model.field
    , markAllCompleted address model.tasks
    , taskList address model.visibilityFilter model.tasks
    , footer address model.visibilityFilter model.tasks
    -- Useful for debugging purposes
    -- , fromElement (show model)
    ]


inbox : Signal.Mailbox Action
inbox =
  Signal.mailbox NoOp


actions : Signal Action
actions =
  inbox.signal


initialModel : Model
initialModel =
  Maybe.withDefault emptyModel restoreState


model : Signal Model
model =
  Signal.foldp update initialModel actions


main : Signal Html
main =
  Signal.map (view inbox.address) model


-- Tell JavaScript the id of the element we want to focus
port focus : Signal String
port focus =
  let
    needsFocus action =
      case action of
        EditingTask _ True -> True
        _ -> False

    toSelector action =
      case action of
        EditingTask id True -> "todo-" ++ toString id
        _ -> ""
  in
    actions
      -- The values on the following signal
      -- would have the form (EditingTask id True)
      |> Signal.filter needsFocus (EditingTask 0 True)
      |> Signal.map toSelector


port saveState : Signal Model
port saveState =
  model


port restoreState : Maybe Model
