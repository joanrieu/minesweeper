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
    div []
        (List.map
            (\y ->
                tr
                    []
                    (List.map
                        (\x -> viewCell ( x, y ) model.game)
                        (List.range
                            1
                            model.difficulty.width
                        )
                    )
            )
            (List.range
                1
                model.difficulty.height
            )
        )


viewCell : M.Position -> M.Game -> Html Msg
viewCell position game =
    if M.isVisible position game then
        if M.isMine position game |> Maybe.withDefault False then
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
        []


neighbourCell : Int -> Html Msg
neighbourCell count =
    td
        [ style [ ( "color", "blue" ) ] ]
        [ count |> toString |> text ]


unknownCell : M.Position -> Html Msg
unknownCell position =
    td
        [ position |> M.RevealCell |> GameCommand |> onClick
        , style [ ( "background", "grey" ) ]
        ]
        [ text "?" ]


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
