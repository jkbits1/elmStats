module ElmStats exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)

import Http exposing (..)
import Json.Decode exposing (..)

import String exposing (..)
import String exposing (split, join)
-- import List exposing (..)
-- import List exposing (filter)

type alias Player = {
    id: Int
  , rank: String
  , name: String
  , won: String 
  , played: String 
  , sets: String 
  , games: String
  , setsWon: String
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
    -- HttpInfo (Result Http.Error (List String)) 
    HttpInfo (Result Http.Error (List PlayerJson)) 
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
  li [] 
  [
    text 
      <| "id: " ++ (toString p.id) ++ " " ++ p.name ++ " Rank: " ++ p.rank 
          ++ " Won: " ++  p.won
          ++ " Played: " ++  p.played
          ++ " Sets: " ++  p.sets
          ++ " Games: " ++  p.games
          ++ " setsWon: " ++  p.setsWon
  ]

playerFromJson : PlayerJson -> Player
playerFromJson pj = { 
    id = 1
  , name = pj.name
  , rank = pj.rank 
  , won = pj.won 
  , played = pj.played
  , sets = pj.sets
  , games = pj.games
  , setsWon = toString <| getSetsCountPF True pj
  }

getSetsCount won player = 
  let 
    setsChars = String.split "" player.sets 
    setsCharsNoSpacess = List.filter (\s -> s /= " ") setsChars
    setsInfo = String.join "" setsCharsNoSpacess
    setsWonLost = String.split "-" setsInfo
  in
    case won of 
      True -> 
        Maybe.withDefault "000" <| List.head setsWonLost
      False ->
        Maybe.withDefault "000" <| List.head <| List.drop 1 setsWonLost
            
getSetsCountPF won player = 
  let 
    fpf = split "-" << join "" << List.filter (\s -> s /= " ") << split ""

    setsWonLost = fpf player.sets
  in
    case won of 
      True -> 
        Maybe.withDefault "000" <| List.head setsWonLost
      False ->
        Maybe.withDefault "000" <| List.head <| List.drop 1 setsWonLost

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
        -- firstPlayer ps  = Maybe.withDefault "" <| List.head ps
        mapPlayers ps   = List.map playerFromJson ps 
        -- mapPlayers ps   = List.map (\p -> { id = 1, name = p.name }) ps 
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
        , rank = "1"
        , won = "1" 
        , played = "1" 
        , sets = "1-2"
        , games = "3-4"
        , setsWon = "1,2,2"
      }
    , {
          id = 2
        , name = "Bill"
        , rank = "2"
        , won = "1" 
        , played = "1" 
        , sets = "1-2"
        , games = "3-4"
        , setsWon = "1,2,2"
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
    
-- httpGetPics : String -> Request String
-- httpGetPics url = 
--   Http.get url decodeGifUrl

-- httpGetPlayers : String -> Request (List String)
httpGetPlayers : String -> Request (List PlayerJson)
httpGetPlayers url = 
  Http.get url 
    -- (Json.Decode.list decodePlayer)
    (Json.Decode.list decodePlayerFlexible)
    -- (Json.Decode.list decodePlayerName)
  
-- decodeGifUrl : Json.Decode.Decoder String
-- decodeGifUrl =
--   Json.Decode.at ["data", "image_url"] Json.Decode.string

-- https://tennis-stats.herokuapp.com/teams/5

-- field "Rank" int

-- decodePlayer : Json.Decode.Decoder String
-- -- decodePlayer = field "Rank" int
-- decodePlayer = field "Rank" string

-- decodePlayer : Json.Decode.Decoder PlayerJson
-- decodePlayer = 
--   map2 PlayerJson 
--     decodePlayerRank decodePlayerName

-- NOTE: 1) no max on fields supported
-- 2) mismatch between this fn and json record has errors that aren't so clear.
-- This may or may not be what's meant in comment on Decode page about extended
-- library not having such good error explanations.
-- 3) order is important
decodePlayerFlexible : Decoder PlayerJson
decodePlayerFlexible =
  required "Games"  string <| 
  required "Sets"   string <| 
  required "Played" string <| 
  required "Won"    string <| 
  required "Name"   string <| 
  required "Rank"   string <| 
  Json.Decode.succeed PlayerJson

-- decodePlayerRank : Json.Decode.Decoder String
-- decodePlayerRank = field "Rank" string

-- decodePlayerName : Json.Decode.Decoder String
-- decodePlayerName = field "Name" string

type alias PlayerJson = { 
    rank: String
  , name: String 
  , won: String 
  , played: String 
  , sets: String 
  , games: String
}

-- from elm repl
-- decodeString decodePlayer """{"Rank":"1","Name":"Robin Goldman","Won":"3","Played":"9","Sets":"3 - 8","Games":"40 - 66"}"""
-- Ok "1" : Result.Result String String

-- decodeString (list decodePlayer) """[{"Rank":"1","Name":"Robin Goldman","Won":"3","Played":"9","Sets":"3 - 8","Games":"40 - 66"}]"""

-- NOTE: standard Elm Decode has a max of 7 fields (map7) so, custom, required 
-- have been taken from:
-- http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest
-- Aim here is to understand and adapt the FP code, rather than merely use it.
custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom = Json.Decode.map2 (|>)

required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required key valDecoder decoder =
    custom (Json.Decode.field key valDecoder) decoder








type alias User =
  { id : String
  , name : String
  -- , email : String
  }
userDecoder : Decoder User
userDecoder =
-- working out where the precedence is here
  ((Json.Decode.succeed User
      |> required "id" string)
    -- |> required "id" int
      |> required "name" string)
    -- |> required "email" string

decodeUserId : Json.Decode.Decoder String
decodeUserId = field "Id" string

requiredUserId : Decoder (String -> User) -> Decoder User
requiredUserId decoder =
    custom (Json.Decode.field "Id" string) decoder

-- types from repl
x1 : Json.Decode.Decoder (String -> String -> User)
x1 = Json.Decode.succeed User

fn1 : Json.Decode.Decoder (String -> b) -> Json.Decode.Decoder b
fn1 = required "id" string

x2 : Json.Decode.Decoder (String -> User)
x2 = fn1 x1

fn2 : Json.Decode.Decoder (String -> b) -> Json.Decode.Decoder b
fn2 = required "name" string

usrDcdr2 : Json.Decode.Decoder User
usrDcdr2 = fn2 x2

usrDcdr2a = fn2 <| fn1 x1

usrDcdr2b = fn2 <| fn1 <| Json.Decode.succeed User

-- required : String -> Decoder a -> Decoder (a -> b) -> Decoder b

usrDcdr2c = fn2 <| required "id" string <| Json.Decode.succeed User

usrDcdr2d = required "name" string <| required "id" string <| Json.Decode.succeed User

usrDcdr3 = required "name" string <| requiredString "id" <| Json.Decode.succeed User
usrDcdr3a = requiredString "name" <| requiredString "id" <| Json.Decode.succeed User

-- compiles and works
usrDcdr3bit : Json.Decode.Decoder (String -> b) -> Json.Decode.Decoder b
usrDcdr3bit = requiredString "id"
usrDcdr3b = requiredString "name" <| usrDcdr3bit <| Json.Decode.succeed User

usrDcdr3bit2 : Json.Decode.Decoder (String -> String -> User) -> Json.Decode.Decoder (String -> User)
usrDcdr3bit2 = requiredString "id"
usrDcdr3c = requiredString "name" <| usrDcdr3bit2 <| Json.Decode.succeed User

-- NOTE: this fn acts like both requiredUserString and requiredUserString2, due 
--       to flexible Haskell types
requiredString : String -> Decoder (String -> b) -> Decoder b
requiredString key decoder =
    custom (Json.Decode.field key string) decoder

requiredUserString : String -> Decoder (String -> String -> User) -> Decoder (String -> User)
requiredUserString key decoder =
    custom (Json.Decode.field key string) decoder

requiredUserString2 : String -> Decoder (String -> User) -> Decoder User
requiredUserString2 key decoder =
    custom (Json.Decode.field key string) decoder

usrDcdr4 : Json.Decode.Decoder (String -> User)
usrDcdr4 = requiredUserString "id" <| Json.Decode.succeed User

usrDcdr4a = required "name" string <| requiredUserString "id" <| Json.Decode.succeed User
usrDcdr4b = requiredUserString2 "name" <| requiredUserString "id" <| Json.Decode.succeed User

-- usrDcdr4 : Json.Decode.Decoder (String -> User) -> Json.Decode.Decoder User
-- usrDcdr4 = requiredUserString "id" 

usrDcdr4bit : Json.Decode.Decoder (String -> String -> User)
usrDcdr4bit = Json.Decode.succeed User

-- usrDcdr4a = requiredString "name" <| requiredString "id" <| Json.Decode.succeed User

-- type alias User =
--   { id : String
--   , name : String
--   -- , email : String
--   }

-- NOTE: same as usrDcdr2d
userDecoderRev : Decoder User
userDecoderRev =
  required "name" string  <| 
  required "id" string    <| 
  Json.Decode.succeed User

