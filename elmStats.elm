module ElmStats exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)

import String exposing (..)
-- import List exposing (..)

type alias Player = {
    id: Int
  , name: String
}
type alias Players = List Player

type alias Model = {
    url: String
  , area: String
  , comment: String
  , players: Players
  }

type Msg = 
    -- HttpInfo (Result Http.Error String) 
    HttpInfo (Result Http.Error (List String)) 
  | Change String 
  | ChangeArea String

main = Html.program { init = init, view = view, update = updateModel, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

view : Model -> Html Msg
view model =
  let 
      s = model.comment
      ps = model.players
  in
  div [
    class "container"
  ] [
    h2 [] [text "Elm Basic"]
    
  , input [ onInput Change ] []
  , br [] []
  , input [ onInput ChangeArea ] []
  
  , br [] [], br [] [], text model.comment

  , ul [] <| List.map playerLi ps
  ]

playerLi : Player -> Html msg
playerLi p =
  let name = p.name
  in
  li [] [text name]

updateModel : Msg -> Model -> (Model, Cmd Msg)
updateModel update model = 
  case update of
    Change s   ->    
      (   {model | url = s}
        -- , Cmd.none
        , getHttp s model.area
        )

    ChangeArea s -> 
      (   { model | area = s }
        , getHttp model.url s
      )

    -- HttpInfo _  ->    ( model, Cmd.none)

    HttpInfo (Ok ps)  ->    
      let 
        firstPlayer ps= Maybe.withDefault "" <| List.head ps
        mapPlayers ps = List.map (\p -> { id = 1, name = p }) ps 
      in
      -- ( {model | comment = firstPlayer ps}, Cmd.none)
      ( {model | players = mapPlayers ps}, Cmd.none)

    HttpInfo (Result.Err error) -> 
      let 
        errInfo = httpErrorToString error
      in
      ( {model | comment = errInfo}, Cmd.none)

httpErrorToString error = 
  case error of 
    BadUrl url -> "bad url"

    BadPayload message response -> 
      "bad payload: " ++
      message ++ " " ++ toString response.status.code

    NetworkError -> "nw error"

    BadStatus response ->
      "http error, status code: " ++ toString response.status.code

    _ -> "some other problem"

initialUrl = 
  "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" 
  -- https://tennis-stats.herokuapp.com/teams/

initialItem = 
  -- "cats"
  ""
  -- "5"

initialModelState : Model
initialModelState = {
      url = initialUrl
    , area = initialItem
    , comment = "hello, FullStack Bytes"
    , players = [
      {
          id = 1
        , name = "Mike"
      }
    , {
          id = 2
        , name = "Bill"
      }]
  } 

init = (initialModelState, 
    -- Cmd.none 
    getHttp initialUrl initialItem
  )

getHttp : String -> String -> Cmd Msg
getHttp urlStub item =
  let
      url = urlStub ++ item
  in
    Http.send HttpInfo <| 
      -- httpGetPics url
      httpGetPlayers url
    
httpGetPics : String -> Request String
httpGetPics url = 
  Http.get url decodeGifUrl
  -- corsGetReq url decodeGifUrl

httpGetPlayers : String -> Request (List String)
httpGetPlayers url = 
  Http.get url 
    -- (Json.Decode.list decodePlayer)
    (Json.Decode.list decodePlayerName)
  
decodeGifUrl : Json.Decode.Decoder String
decodeGifUrl =
  Json.Decode.at ["data", "image_url"] Json.Decode.string

getx : String -> Json.Decode.Decoder a -> Request a
getx url decoder =
  request
    { method = "GET"
    , headers = []
    , url = url
    , body = emptyBody
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

corsGetReq url decoder = 
 request
    { method = "GET"
    , headers = [
    --     Http.header "Access-Control-Allow-Origin" "http://localhost:8000"
    --     res.header('Access-Control-Allow-Origin', 'example.com');
    -- res.header('Access-Control-Allow-Methods', 'GET,PUT,POST,DELETE');
    -- res.header('Access-Control-Allow-Headers', 'Content-Type');

      ]
    , url = url
    , body = emptyBody
    -- , expect = expectStringResponse (\_ -> Ok ())
    , expect = expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }

--  { verb = "GET"
--     , headers = [("Origin", origin)]
--     , url = url
--     , body = empty
--     }

-- https://tennis-stats.herokuapp.com/teams/5

-- field "Rank" int

decodePlayer : Json.Decode.Decoder String
-- decodePlayer = field "Rank" int
decodePlayer = field "Rank" string

decodePlayerName : Json.Decode.Decoder String
decodePlayerName = field "Name" string

-- from elm repl
-- decodeString decodePlayer """{"Rank":"1","Name":"Robin Goldman","Won":"3","Played":"9","Sets":"3 - 8","Games":"40 - 66"}"""
-- Ok "1" : Result.Result String String

-- decodeString (list decodePlayer) """[{"Rank":"1","Name":"Robin Goldman","Won":"3","Played":"9","Sets":"3 - 8","Games":"40 - 66"}]"""


