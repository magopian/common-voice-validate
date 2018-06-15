port module Main exposing (..)

import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Touch


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
    , gesture : Touch.Gesture
    , gestureStart : Touch.Position
    , gesturePosition : Touch.Position
    }


init : ( Model, Cmd Msg )
init =
    ( Model NotLoaded 0 0 Touch.blanco initPosition initPosition, getClips )


initPosition : Touch.Position
initPosition =
    { x = 0, y = 0 }



---- UPDATE ----


type Vote
    = Good
    | Bad


type Msg
    = NewClips (Result Http.Error (List AudioClip))
    | SendVote Vote
    | DurationChange Float
    | Play
    | Ended
    | TimeUpdate Float
    | SwipeStart Touch.Event
    | Swipe Touch.Event
    | SwipeEnd Touch.Event
    | ResetPlay


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

        SendVote vote ->
            let
                _ =
                    Debug.log "Sending vote" vote
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

        SwipeStart touch ->
            let
                position =
                    Touch.locate touch
            in
                ( { model | gestureStart = position, gesturePosition = position }, Cmd.none )

        Swipe touch ->
            let
                gesture : Touch.Gesture
                gesture =
                    Touch.record touch model.gesture
            in
                ( { model | gesture = gesture, gesturePosition = Touch.locate touch }, Cmd.none )

        SwipeEnd touch ->
            let
                gesture : Touch.Gesture
                gesture =
                    Touch.record touch model.gesture

                updatedModel =
                    { model | gesture = Touch.blanco, gestureStart = initPosition, gesturePosition = initPosition }
            in
                if Touch.isLeftSwipe 100 gesture then
                    update (SendVote Bad) updatedModel
                else if Touch.isRightSwipe 100 gesture then
                    update (SendVote Good) updatedModel
                else if Touch.isTap gesture then
                    update ResetPlay updatedModel
                else
                    ( updatedModel, Cmd.none )

        ResetPlay ->
            ( model, resetPlay () )



---- VIEW ----


view : Model -> Html.Html Msg
view model =
    Html.div []
        (case model.validating of
            Playing clip clipList ->
                [ Html.h1 []
                    [ Html.text "Is this sentence pronounced correctly?" ]
                , viewClip clip model.duration model.currentTime (model.gesturePosition.x - model.gestureStart.x)
                ]

            NotLoaded ->
                [ Html.h1 [] [ Html.text "Loading audio clips, please wait..." ]
                ]

            NoClips ->
                [ Html.h1 [] [ Html.text "No audio clips to validate" ]
                ]
        )


viewClip : AudioClip -> Float -> Float -> Float -> Html.Html Msg
viewClip clip duration currentTime deltaX =
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
        Html.div
            [ Html.Attributes.style
                [ ( "overflow", "hidden" )
                , ( "padding-top", "4px" )
                ]
            ]
            [ Html.div
                [ Html.Attributes.style
                    [ ( "position", "relative" )
                    , ( "background-color"
                      , if deltaX >= 0 then
                            "#b7d43f"
                        else if deltaX <= 0 then
                            "#f00"
                        else
                            "#eee"
                      )
                    , ( "margin-bottom", "20px" )
                    ]
                ]
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
                        [ ( "background-color", "white" )
                        , ( "opacity", toString ((100 - (abs deltaX)) / 100) )
                        , ( "padding", "10px 0" )
                        , ( "position", "relative" )
                        ]
                    , Touch.onStart SwipeStart
                    , Touch.onMove Swipe
                    , Touch.onEnd SwipeEnd
                    , Html.Events.onClick ResetPlay
                    ]
                    [ Html.div
                        [ Html.Attributes.style
                            [ ( "border-bottom", "4px solid #b7d43f" )
                            , ( "border-top", "4px solid #b7d43f" )
                            , ( "height", "100%" )
                            , ( "left", "0" )
                            , ( "position", "absolute" )
                            , ( "top", "-4px" )
                            , ( "width", percentageAttributes )
                            ]
                        ]
                        []
                    , Html.div
                        [ Html.Attributes.style
                            [ ( "position", "relative" )
                            , ( "left", (toString deltaX) ++ "px" )
                            ]
                        ]
                        [ Html.text clip.text
                        ]
                    ]
                ]
            , Html.div []
                [ Html.button
                    [ Html.Events.onClick <| SendVote Bad ]
                    [ Html.text "Bad, pronounced incorrectly" ]
                , Html.button
                    [ Html.Events.onClick <| SendVote Good ]
                    [ Html.text "Good, pronounced correctly" ]
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



---- PORTS ----


port resetPlay : () -> Cmd msg



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
