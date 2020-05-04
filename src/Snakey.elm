module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text, p)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import Random

type alias Model =
    { gameStarted: Bool
    , food : Maybe Position
    , snake : Snake
    , direction :  Maybe Direction }

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


snakePos : Model -> Position -> Int -> Position
snakePos model position section =
    case model.direction of
        Just Up ->
            Position (position.xPos * square + thingRadius) (position.yPos * square - (section * square) + thingRadius)
        Just Down ->
            Position (position.xPos * square + thingRadius) (position.yPos * square + (section * square) + thingRadius)
        Just Left ->
            Position (position.xPos * square - (section * square) + thingRadius) (position.yPos * square + thingRadius)
        Just Right ->
            Position (position.xPos * square + (section * square) + thingRadius) (position.yPos * square + thingRadius)
        _ ->
            Position (position.xPos * square + square + thingRadius) (position.yPos * square + thingRadius)



initialModel : Model
initialModel =
    { gameStarted = False
    , food = Nothing
    , snake = Nothing
    , direction = Nothing }


type Msg
    = Tick Time.Posix
    | StartGame
    | GotNewPosition Position
    | GotNewDirection Direction
    | GotSnakeHead Position



getPositionCmd : Cmd Msg
getPositionCmd = Random.generate GotNewPosition positionGenerator

getSnakeHeadCmd : Cmd Msg
getSnakeHeadCmd = Random.generate GotSnakeHead positionGenerator

getDirectionCmd : Cmd Msg
getDirectionCmd = Random.generate GotNewDirection directionGenerator

initSnake : Model -> Position -> Maybe (List Position)
initSnake model position =
    Just
        [ (Position (position.xPos * square + thingRadius) (position.yPos * square + thingRadius))
        , (snakePos model position 1)
        , (snakePos model position 2)
        ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick _ ->
             let
                 dir =
                     Maybe.withDefault Right model.direction

                 ( newSnake, removedPart ) =
                     moveySnakey dir model.snake
             in
                case model.gameStarted of
                    True ->
                        ( { model | snake = Just newSnake }
                            , Cmd.none )
                    False ->
                        ( model, Cmd.none )

        StartGame ->
                ( { model | gameStarted = True }
                                    , Cmd.batch [ getPositionCmd, getDirectionCmd, getSnakeHeadCmd ] )
        GotNewPosition position ->
            ( { model | food = Just (Position (position.xPos * square + square + thingRadius) (position.yPos * square + thingRadius)) }, Cmd.none)

        GotSnakeHead position ->
            ( { model | snake =  initSnake model position }, Cmd.none)

        GotNewDirection direction ->
                    ({ model | direction = Just direction }, Cmd.none)


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
    div []
        [ div []
        [ Html.text <| Debug.toString model.direction ++ " " ++ Debug.toString model.food ++ Debug.toString model.snake ]
        , button [ onClick StartGame ]  [ Html.text "start game"]
        , svg
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
              , drawThing foodColour (Maybe.withDefault (Position 0 0) model.food)
              , Svg.g [] (List.map (drawThing snakeColour) (Maybe.withDefault [] model.snake) )
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

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 600 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- 3. sort out directions
-- 4. stop at the edge
-- 5. keyboard input
-- 6. eat
-- 7. grow when eaten
-- 8. collision with self
-- 9. start game button either outside board or on a start page.
