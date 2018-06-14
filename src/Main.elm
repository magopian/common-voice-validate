module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
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


type Validating
    = NotLoaded
    | NoClips
    | Playing AudioClip (List AudioClip)


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
    { validating : Validating
    , duration : Float
    , currentTime : Float
    }


init : ( Model, Cmd Msg )
init =
    ( Model NotLoaded 0 0, getClips )



---- UPDATE ----


type Vote
    = Good
    | Bad


type Msg
    = NewClips (Result Http.Error (List AudioClip))
    | SendVote Vote AudioClip
    | DurationChange Float
    | Play
    | Ended
    | TimeUpdate Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewClips (Ok clipList) ->
            case model.validating of
                Playing currentClip currentClipList ->
                    ( { model | validating = Playing currentClip (currentClipList ++ clipList) }, Cmd.none )

                _ ->
                    case clipList of
                        firstClip :: rest ->
                            ( { model | validating = Playing firstClip rest }, Cmd.none )

                        _ ->
                            ( { model | validating = NoClips }, Cmd.none )

        NewClips (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
                ( model, Cmd.none )

        SendVote vote clip ->
            let
                _ =
                    Debug.log ("Sending vote for clip " ++ clip.glob) vote
            in
                case model.validating of
                    Playing currentClip currentClipList ->
                        case currentClipList of
                            firstClip :: clipList ->
                                ( { model | validating = Playing firstClip clipList }, Cmd.none )

                            _ ->
                                ( { model | validating = NoClips }, Cmd.none )

                    _ ->
                        Debug.crash <| "Wait, SendVote (" ++ (toString vote) ++ ") without any current clip playing?"

        DurationChange duration ->
            ( { model | duration = duration }, Cmd.none )

        Play ->
            ( model, Cmd.none )

        Ended ->
            ( model, Cmd.none )

        TimeUpdate currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        (case model.validating of
            Playing clip clipList ->
                [ Html.h1 [] [ Html.text "Is this sentence pronounced correctly?" ]
                , viewClip clip model.duration model.currentTime
                ]

            NotLoaded ->
                [ Html.h1 [] [ Html.text "Loading audio clips, please wait..." ]
                ]

            NoClips ->
                [ Html.h1 [] [ Html.text "No audio clips to validate" ]
                ]
        )


viewClip : AudioClip -> Float -> Float -> Html.Html Msg
viewClip clip duration currentTime =
    let
        percentage =
            if duration /= 0 then
                currentTime
                    * 100
                    / duration
            else
                0

        percentageAttributes =
            (toString percentage)
                ++ "%"
    in
        Html.div []
            [ Html.audio
                [ Html.Attributes.autoplay True
                , Html.Attributes.src clip.sound

                -- , Html.Attributes.controls True
                , onDurationChange DurationChange
                , onPlay Play
                , onEnded Ended
                , onTimeUpdate TimeUpdate
                ]
                []
            , Html.div
                [ Html.Attributes.style
                    [ ( "padding", "10px 0" )
                    , ( "position", "relative" )
                    ]
                ]
                [ Html.div
                    [ Html.Attributes.style
                        [ ( "background-color", "#b7d43f" )
                        , ( "height", "100%" )
                        , ( "left", "0" )
                        , ( "position", "absolute" )
                        , ( "top", "0" )
                        , ( "width", percentageAttributes )
                        , ( "z-index", "-1000" )
                        ]
                    ]
                    []
                , Html.text clip.text
                ]
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick <| SendVote Bad clip ]
                    [ Html.text "No, pronounced incorrectly" ]
                , Html.button
                    [ Html.Events.onClick <| SendVote Good clip ]
                    [ Html.text "Yes, pronounced correctly" ]
                ]
            ]


onDurationChange : (Float -> Msg) -> Html.Attribute Msg
onDurationChange msg =
    Html.Events.on "durationchange" (Decode.map msg targetDurationChange)


targetDurationChange : Decode.Decoder Float
targetDurationChange =
    Decode.at [ "target", "duration" ] Decode.float


onPlay : Msg -> Html.Attribute Msg
onPlay msg =
    Html.Events.on "play" (Decode.succeed msg)


onEnded : Msg -> Html.Attribute Msg
onEnded msg =
    Html.Events.on "ended" (Decode.succeed msg)


onTimeUpdate : (Float -> Msg) -> Html.Attribute Msg
onTimeUpdate msg =
    Html.Events.on "timeupdate" (Decode.map msg targetCurrentTime)


targetCurrentTime : Decode.Decoder Float
targetCurrentTime =
    Decode.at [ "target", "currentTime" ] Decode.float



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
