module Table where

import Config
import Date exposing (..)
import Date.Format exposing (format)
import Debug
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing ((:=))
import String
import Task


-- MODEL
type alias Model =
  { path : String
  , status : Status
  , response : Response
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

type alias Response =
  { records : List Record
  , count : Int
  , next : Maybe String
  , previous : Maybe String
  }

initialModel : Model
initialModel =
  { path = "api/v1.0/work-sessions"
  , status = Init
  , response =
    { records = []
    , count = 0
    , next = Nothing
    , previous = Nothing
    }
  }


init : (Model, Effects Action)
init =
  ( initialModel
  , Task.succeed Reload |> Effects.task
  )


-- UPDATE
type Action =
  Reload
  | Previous
  | Next
  | UpdateDataFromServer (Result Http.Error Response)


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

    Previous ->
      let
        url =
          case model.response.previous of
            Just url -> url
            Nothing -> ""
      in
        ( model, getJson url Config.accessToken)

    Next ->
      let
        url =
          case model.response.next of
            Just url -> url
            Nothing -> ""
      in
        ( model, getJson url Config.accessToken)

    UpdateDataFromServer response ->
      case response of
        Ok response ->
          ( { model
            | status = Fetched
            , response = response
            }
          , Effects.none
          )

        Err error ->
          let
            message = getErrorMessageFromHttpResponse error
            _ = Debug.log "Error" message
          in
            ( { model | status = HttpError error }
            , Effects.none
            )

-- VIEW
view : Signal.Address Action -> Model -> Html
view address model =
  let
    row : Record -> Html
    row record =
      let
        timestampFormat : Int -> String
        timestampFormat time =
          Date.fromTime (toFloat time * 1000) |> Date.Format.format "%d/%m/%Y %H:%M"

        end =
          case record.end of
            Just end ->
              timestampFormat end

            Nothing ->
              ""
      in
        tr [ ]
          [ td [] [ text <| toString record.id ]
          , td [] [ text record.employee ]
          , td [] [ text <| timestampFormat record.start ]
          , td [] [ text <| end ]
          ]
  in
    div []
      [ button [ onClick address Previous ] [ text "<" ]
      , button [ onClick address Next ] [ text ">" ]
      , table [ class "table table-striped" ]
        [ thead []
          [ tr []
            [ th [] [ text "ID" ]
            , th [] [ text "Employee" ]
            , th [] [ text "Start" ]
            , th [] [ text "End" ]
            ]
          ]
        , tbody [] ( List.map row model.response.records )
        ]
      , div [] [ text <| toString model.response.next ]
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


decodeResponse : Json.Decode.Decoder Response
decodeResponse =
  Json.Decode.object4 Response
    ( Json.Decode.at ["data"]
    <| Json.Decode.list
    <| Json.Decode.object4 Record
      ("id" := Json.Decode.int)
      ("employee" := Json.Decode.string)
      ("start" := Json.Decode.int)
      (Json.Decode.maybe ("end" := Json.Decode.int))
    )
  ("count" := Json.Decode.int)
  (Json.Decode.maybe (Json.Decode.at["next"] <| ("href" := Json.Decode.string)))
  (Json.Decode.maybe (Json.Decode.at["previous"] <| ("href" := Json.Decode.string)))



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
