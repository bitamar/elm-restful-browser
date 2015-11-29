module Table where

import Config
import Debug
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing ((:=))
import Task

-- MODEL
type alias Model =
  { path : String
  , status : Status
  , records : List Record
  }

type Status =
  Init
  | Fetching
  | Fetched
  | HttpError Http.Error

type alias Record =
  { id : Int
  , employee : String
  , start : Int
  , end : Maybe Int
  }

initialModel : Model
initialModel =
  { path = "api/v1.0/work-sessions"
  , status = Init
  , records = []
  }


init : (Model, Effects Action)
init =
  ( initialModel
  , Task.succeed Reload |> Effects.task
  )


-- UPDATE
type Action =
  Reload
  | UpdateDataFromServer (Result Http.Error (List Record))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Reload ->
      let
        url = Config.backendUrl ++ model.path
      in
        ( { model | status = Fetching }
        , getJson url Config.accessToken
        )


    UpdateDataFromServer result ->
      case result of
        Ok records ->
          let
            _ = 'x'
            -- _ = Debug.log "Reponse" records
          in
            ( { model
              | status = Fetched
              , records = records
              }
            , Effects.none
            )

        Err error ->
          let
            message = getErrorMessageFromHttpResponse error
            _ = Debug.log "Error" message
          in
            ( { model
              | status = HttpError error
              }
            , Effects.none
            )

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let
    row : Record -> Html
    row record =
      tr [ ]
        [ td [] [ text <| toString record.id ]
        , td [] [ text <| record.employee ]
        , td [] [ text <| toString record.start ]
        , td [] [ text <| toString record.end ]
        ]
  in
    div []
      [ button [ onClick address Reload ] [ text "Reload" ]
      , table [ class "table table-striped" ]
        [ thead []
          [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Employee" ]
            , th [] [ text "Start" ]
            , th [] [ text "End" ]
            ]
          ]
        , tbody [] ( List.map row model.records )
        ]
      ]


-- EFFECTS
getJson : String -> String -> Effects Action
getJson url accessToken =
  Http.send Http.defaultSettings
    { verb = "GET"
    , headers = [ ("access-token", accessToken) ]
    , url = url
    , body = Http.empty
    }
    |> Http.fromJson decodeResponse
    |> Task.toResult
    |> Task.map UpdateDataFromServer
    |> Effects.task


decodeResponse : Json.Decode.Decoder (List Record)
decodeResponse =
  Json.Decode.at ["data"]
    <| Json.Decode.list
    <| Json.Decode.object4 Record
      ("id" := Json.Decode.int)
      ("employee" := Json.Decode.string)
      ("start" := Json.Decode.int)
      (Json.Decode.maybe ("end" := Json.Decode.int))


getErrorMessageFromHttpResponse : Http.Error -> String
getErrorMessageFromHttpResponse error =
  case error of
    Http.Timeout ->
      "Connection has timed out"

    Http.BadResponse code message ->
      message

    Http.NetworkError ->
      "A network error has occured"

    Http.UnexpectedPayload message ->
      "Unexpected response: " ++ message
