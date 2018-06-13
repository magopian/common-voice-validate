module Main exposing (..)

import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


clips_url : String
clips_url =
    --"https://voice.mozilla.org/api/v1/fr/clips?count=10"
    "https://voice-proxy.herokuapp.com/api/v1/fr/clips?count=10"


user_id : String
user_id =
    "d04a2398-b5f5-44c4-8082-5198975656a6"


type alias AudioClip =
    { id : Int
    , glob : String
    , text : String
    , sound : String
    }


getClips : Cmd Msg
getClips =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "uid" user_id
            ]
        , url = clips_url
        , body = Http.emptyBody
        , expect = Http.expectJson (Decode.list decodeClip)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.send NewClips


decodeClip : Decode.Decoder AudioClip
decodeClip =
    Pipeline.decode AudioClip
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "glob" Decode.string
        |> Pipeline.required "text" Decode.string
        |> Pipeline.required "sound" Decode.string



---- MODEL ----


type alias Model =
    { clips : List AudioClip }


init : ( Model, Cmd Msg )
init =
    ( Model [], getClips )



---- UPDATE ----


type Msg
    = NewClips (Result Http.Error (List AudioClip))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewClips (Ok clipList) ->
            let
                _ =
                    Debug.log "clipList" clipList
            in
                ( { model | clips = model.clips ++ clipList }, Cmd.none )

        NewClips (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
                ( model, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    if List.length model.clips /= 0 then
        Html.div []
            [ Html.h1 [] [ Html.text "Here's a list of audio clips to validate" ]
            , viewClips model.clips
            ]
    else
        Html.div []
            [ Html.h1 [] [ Html.text "Loading audio clips, please wait..." ]
            ]


viewClips : List AudioClip -> Html.Html Msg
viewClips clips =
    clips
        |> List.map
            (\clip ->
                Html.li []
                    [ Html.audio
                        [ Html.Attributes.preload "auto"
                        , Html.Attributes.controls True
                        , Html.Attributes.src clip.sound
                        ]
                        []
                    , Html.text clip.text
                    ]
            )
        |> Html.ul []



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
