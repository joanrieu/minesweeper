module Main exposing (Model, Msg, update, view, subscriptions, init)

import Minesweeper as M exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = TitleScreen
    | GameScreen Game


type Msg
    = GameCommand Command
    | NewGame Difficulty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        TitleScreen ->
            case msg of
                NewGame difficulty ->
                    newGame difficulty

                _ ->
                    ( model, Cmd.none )

        GameScreen game ->
            case msg of
                GameCommand command ->
                    let
                        ( updatedGame, commands ) =
                            handle command game
                    in
                        ( GameScreen updatedGame
                        , commands |> Cmd.map GameCommand
                        )

                NewGame difficulty ->
                    init


view : Model -> Html Msg
view model =
    let
        contents =
            case model of
                TitleScreen ->
                    [ button [ onClick (NewGame beginnerDifficulty) ] [ text "Beginner" ]
                    , button [ onClick (NewGame intermediateDifficulty) ] [ text "Intermediate" ]
                    , button [ onClick (NewGame expertDifficulty) ] [ text "Expert" ]
                    ]

                GameScreen game ->
                    [ button [ onClick (NewGame noDifficulty) ] [ text "New Game" ]
                    , viewCells game
                    , node "style"
                        []
                        [ "td {"
                            ++ "width: 1em;"
                            ++ "height: 1em;"
                            ++ "text-align: center;"
                            ++ "vertical-align: middle;"
                            ++ "line-height: 1;"
                            ++ "}"
                            |> text
                        ]
                    ]
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                ]
            ]
            ((h1 [] [ text "Minesweeper" ])
                :: (node "style"
                        []
                        [ "button {"
                            ++ "margin: 1em;"
                            ++ "padding: .2em .5em;"
                            ++ "}"
                            |> text
                        ]
                   )
                :: contents
            )


viewCells : Game -> Html Msg
viewCells game =
    let
        cols =
            List.range
                1
                (gameDifficulty game).width

        rows =
            List.range
                1
                (gameDifficulty game).height
    in
        table [ style [ ( "font-size", "2em" ) ] ]
            (List.map
                (\y ->
                    tr
                        []
                        (List.map
                            (\x -> viewCell ( x, y ) game)
                            cols
                        )
                )
                rows
            )


viewCell : Position -> Game -> Html Msg
viewCell position game =
    if isVisible position game then
        if isMine position game then
            bombCell
        else
            let
                count =
                    neighbourMineCount position game
            in
                if count > 0 then
                    neighbourCell count
                else
                    emptyCell
    else
        unknownCell position


bombCell : Html Msg
bombCell =
    td
        [ style [ ( "background", "red" ) ]
        ]
        [ text "X" ]


emptyCell : Html Msg
emptyCell =
    td [ style [ ( "background", "lightgrey" ) ] ]
        [ text " " ]


neighbourCell : Int -> Html Msg
neighbourCell count =
    let
        color =
            case count of
                1 ->
                    "blue"

                2 ->
                    "green"

                3 ->
                    "red"

                4 ->
                    "purple"

                5 ->
                    "brown"

                6 ->
                    "turquoise"

                7 ->
                    "black"

                _ ->
                    "grey"
    in
        td
            [ style [ ( "color", color ), ( "background", "lightgrey" ) ] ]
            [ count |> toString |> text ]


unknownCell : Position -> Html Msg
unknownCell position =
    td
        [ position |> RevealCell |> GameCommand |> onClick
        , style [ ( "background", "grey" ) ]
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( TitleScreen, Cmd.none )


newGame : Difficulty -> ( Model, Cmd Msg )
newGame difficulty =
    let
        ( game, commands ) =
            handle (StartGame difficulty) noGame
    in
        ( GameScreen game
        , commands |> Cmd.map GameCommand
        )
