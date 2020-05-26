module Snakey exposing (main)

import Browser
import Browser.Events exposing(onKeyDown)
import Html.Attributes as Attr exposing (id)
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Json.Decode as Decode
import Time
import Random

type alias Model =
    { gameStarted: Bool
    , food : Maybe Position
    , snake : Snake
    , direction :  Maybe Direction
    , speed : Float
    , score : Int }

type alias Snake = Maybe (List Position)

type alias Position =
    { xPos : Int, yPos : Int }

thingRadius : Int
thingRadius = 5

square : Int
square = 10

numCols : Int
numCols = 100

numRows : Int
numRows = 60

snakeColour : String
snakeColour = "red"

foodColour : String
foodColour = "yellow"

width : Int
width = 1000

height : Int
height = 600

type Direction
    = Up
    | Down
    | Left
    | Right


initialModel : Model
initialModel =
    { gameStarted = False
    , food = Nothing
    , snake = Nothing
    , direction = Nothing
    , speed = 450
    , score = 0 }


type Msg
    = Tick Time.Posix
    | CharacterKey Char
    | ControlKey String
    | StartGame
    | GotNewFood Position
    | GotNewDirection Direction
    | GotSnakeHead Position
    | ChangeDirection Direction


getFoodCmd : Cmd Msg
getFoodCmd = Random.generate GotNewFood positionGenerator

getSnakeHeadCmd : Cmd Msg
getSnakeHeadCmd = Random.generate GotSnakeHead positionGenerator

getDirectionCmd : Cmd Msg
getDirectionCmd = Random.generate GotNewDirection directionGenerator

initSnake : Model -> Position -> Maybe (List Position)
initSnake model position =
    Just
        [ Position (position.xPos * square + thingRadius) (position.yPos * square + thingRadius)
        , (snakePos model position 1)
        , (snakePos model position 2)
        ]


snakePos : Model -> Position -> Int -> Position
snakePos model position section =
    case model.direction of
        Just Up ->
            Position (position.xPos * square + thingRadius) (position.yPos * square + (section * square) + thingRadius)
        Just Down ->
            Position (position.xPos * square + thingRadius) (position.yPos * square - (section * square) + thingRadius)
        Just Left ->
            Position (position.xPos * square + (section * square) + thingRadius) (position.yPos * square + thingRadius)
        Just Right ->
            Position (position.xPos * square - (section * square) + thingRadius) (position.yPos * square + thingRadius)
        _ ->
            Position (position.xPos * square + square + thingRadius) (position.yPos * square + thingRadius)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
             let
                 dir =
                     Maybe.withDefault Right model.direction

                 crashed =
                    crash model

                 snacked =
                     snack model

                 newSpeed =
                     model.speed - 15

                 newScore =
                     model.score + 1

                 ( newSnake, removedPart ) =
                     moveySnakey dir model.snake
             in
                case model.gameStarted of
                    True ->
                        case snacked of
                            True ->
                               ( { model | snake = Just (newSnake ++ removedPart), gameStarted = not crashed, speed = newSpeed, score = newScore }, getFoodCmd )
                            False ->
                               ( { model | snake = Just newSnake, gameStarted = not crashed }, Cmd.none )
                               --(model, Cmd.none)

                    False ->
                        ( model, Cmd.none )

        ChangeDirection direction ->
             ( { model | direction = Just direction }, Cmd.none )

        --keeping this for pause etc
        CharacterKey _ ->
             ( model, Cmd.none )

        ControlKey _ ->
             ( model, Cmd.none )

        StartGame ->
            ( { model | gameStarted = True }
                                    , Cmd.batch [getSnakeHeadCmd, getFoodCmd] )
        GotNewFood position ->
            ( { model | food = Just (Position (position.xPos * square + square + thingRadius) (position.yPos * square + thingRadius)) }, Cmd.none)

        GotSnakeHead position ->
            ( { model | snake =  initSnake model position }, Cmd.none)

        GotNewDirection direction ->
            ({ model | direction = Just direction }, Cmd.none)


crash : Model -> Bool
crash model =
    let
        actualSnake = Maybe.withDefault [] model.snake
        head = Maybe.withDefault (Position -10 -10) (List.head actualSnake)
        tail = Maybe.withDefault ([Position -10 -10]) (List.tail actualSnake)

    in
        if head.xPos < 5 || head.xPos > 995 || head.yPos < 5 || head.yPos > 595 || List.member head tail then
            True
        else
            False

--else if List.member h food then
snack : Model -> Bool
snack model =
    let
        actualSnake = Maybe.withDefault [] model.snake
        actualFood = Maybe.withDefault (Position -10 -10) model.food

    in
        if (List.head actualSnake) == Just actualFood then
            True
        else
            False


moveySnakey : Direction -> Maybe (List Position) -> (List Position, List Position)
moveySnakey direction snake =
    let
        actualSnake =
            Maybe.withDefault [] snake

        h =
            Maybe.withDefault (Position -10 -10) (List.head actualSnake)

        newHead =
            case direction of
                Up ->
                    Position h.xPos (h.yPos - square)
                Down ->
                    Position h.xPos (h.yPos + square)
                Left ->
                    Position (h.xPos - square) h.yPos
                Right ->
                    Position (h.xPos + square) h.yPos

        newBody =
            List.take ((List.length actualSnake) - 1) actualSnake

        removedPart =
            List.drop ((List.length actualSnake) - 1) actualSnake

    in
        ( [ newHead ] ++ newBody, removedPart )

positionGenerator : Random.Generator Position
positionGenerator =
    Random.map2
        (\xPos yPos -> Position xPos yPos)
        (Random.int 4 ( numCols - 4 ))
        (Random.int 4 ( numRows - 4 ))


directionGenerator : Random.Generator Direction
directionGenerator =
    Random.uniform Up [ Down, Left, Right ]


view : Model -> Html Msg
view model =
    let
        scoreboard = "Score: " ++ (String.fromInt model.score)
    in
        div []
        [ button [ onClick StartGame ]  [ Html.text "start game" ]
        , div [ Attr.id "scoreboard" ] [ Html.text scoreboard ]
        ,
        div [ Attr.id "game-container" ]
        [ svg
            [ Svg.Attributes.width (String.fromInt width)
            , Svg.Attributes.height (String.fromInt height)
            , viewBox <| "0 0 " ++ (String.fromInt width) ++ " " ++ (String.fromInt height)
            ]
            [ Svg.defs []
                [ Svg.filter
                    [ Svg.Attributes.id "glow"
                    , Svg.Attributes.width "200%"
                    , Svg.Attributes.height "200%"
                    , x "-50%"
                    , y "-50%"
                    ]
                    [ Svg.feGaussianBlur
                        [ Svg.Attributes.in_ "StrokePaint"
                        , stdDeviation "3"
                        ]
                        []
                    ]
                ]
              , drawThing foodColour (Maybe.withDefault (Position -10 -10) model.food)
              , Svg.g [] (List.map (drawThing snakeColour) (Maybe.withDefault [] model.snake) )
            ]
          ]
        ]




drawThing : String -> Position -> Svg.Svg msg
drawThing colour position =
    Svg.g []
        [ Svg.circle
            [ cx <| String.fromInt position.xPos
            , cy <| String.fromInt position.yPos
            , r (String.fromInt (thingRadius + 2))
            , Svg.Attributes.filter "url(#glow)"
            , fill colour
            ]
            []
        , Svg.circle
            [ cx <| String.fromInt position.xPos
            , cy <| String.fromInt position.yPos
            , r (String.fromInt thingRadius)
            , fill colour
            ]
            []
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->

            case keyValue of
                "ArrowRight" ->
                    ChangeDirection Right

                "ArrowLeft" ->
                    ChangeDirection Left

                "ArrowUp" ->
                    ChangeDirection Up

                "ArrowDown" ->
                    ChangeDirection Down

                _ ->
                    ControlKey keyValue



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
            [ Time.every model.speed Tick
            , onKeyDown keyDecoder
            ]

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, getDirectionCmd)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- 16. show message when game ends
-- 17. multicolours
-- 18. difficulty levels
-- 19. obstacles
-- 20. speed change gets lower as score goes up
-- 21. make the scoreboard look a bit nicer
