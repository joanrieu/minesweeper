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


type alias Model =
    { game : M.Game
    , difficulty : M.Difficulty
    }


type Msg
    = GameCommand M.Command


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameCommand command ->
            let
                ( game, commands ) =
                    M.handle command model.game
            in
                ( { model | game = game }
                , commands |> Cmd.map GameCommand
                )


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ h1 [] [ text "Minesweeper" ]
        , viewCells model
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


viewCells : Model -> Html Msg
viewCells model =
    let
        cols =
            List.range
                1
                model.difficulty.width

        rows =
            List.range
                1
                model.difficulty.height
    in
        table [ style [ ( "font-size", "2em" ) ] ]
            (List.map
                (\y ->
                    tr
                        []
                        (List.map
                            (\x -> viewCell ( x, y ) model.game)
                            cols
                        )
                )
                rows
            )


viewCell : M.Position -> M.Game -> Html Msg
viewCell position game =
    if M.isVisible position game then
        if M.isMine position game then
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


unknownCell : M.Position -> Html Msg
unknownCell position =
    td
        [ position |> M.RevealCell |> GameCommand |> onClick
        , style [ ( "background", "grey" ) ]
        ]
        []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    let
        difficulty =
            beginnerDifficulty

        ( game, commands ) =
            M.handle (StartGame difficulty) M.noGame
    in
        ( { game = game
          , difficulty = difficulty
          }
        , commands |> Cmd.map GameCommand
        )
