module Minesweeper exposing (..)

import Random exposing (..)
import Set exposing (..)


type alias Difficulty =
    { width : Int
    , height : Int
    , mines : Int
    }


beginnerDifficulty : Difficulty
beginnerDifficulty =
    { width = 9
    , height = 9
    , mines = 10
    }


type alias Position =
    ( Int, Int )


type Event
    = GameStarted Difficulty (Set Position)
    | CellRevealed Position
    | CellFlagged Position
    | GameWon
    | GameLost


type alias Game =
    List Event


noGame : Game
noGame =
    []


type Command
    = StartGame Difficulty
    | StartGameDeterministically Difficulty (Set Position)
    | RevealCell Position
    | FlagCell Position


handle : Command -> Game -> ( Game, Cmd Command )
handle command game =
    case command of
        StartGame difficulty ->
            ( game
            , Random.generate
                (StartGameDeterministically difficulty)
                (Random.map
                    Set.fromList
                    (Random.list
                        difficulty.mines
                        (Random.map2
                            (\x -> \y -> ( x, y ))
                            (Random.int 1 difficulty.width)
                            (Random.int 1 difficulty.height)
                        )
                    )
                )
            )

        StartGameDeterministically difficulty positions ->
            ( [ GameStarted difficulty positions ]
            , Cmd.none
            )

        RevealCell position ->
            ( revealCell position game
            , Cmd.none
            )

        FlagCell position ->
            ( (CellFlagged position) :: game
            , Cmd.none
            )


revealCell : Position -> Game -> Game
revealCell position game =
    (CellRevealed position) :: game


isMine : Position -> Game -> Maybe Bool
isMine position game =
    case game of
        event :: events ->
            case event of
                GameStarted _ positions ->
                    Just (Set.member position positions)

                _ ->
                    isMine position events

        _ ->
            Nothing


neighbourMineCount : Position -> Game -> Int
neighbourMineCount position game =
    let
        ( x, y ) =
            position

        deltas =
            [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]

        neighbours =
            deltas
                |> List.map (\( dx, dy ) -> ( x + dx, y + dy ))
                |> List.map (\pos -> isMine pos game)
                |> List.filterMap identity
    in
        neighbours
            |> List.filter ((==) True)
            |> List.length


isVisible : Position -> Game -> Bool
isVisible position game =
    case game of
        event :: events ->
            case event of
                CellRevealed revealedPosition ->
                    if revealedPosition == position then
                        True
                    else
                        isVisible position events

                _ ->
                    isVisible position events

        _ ->
            False
