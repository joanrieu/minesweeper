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
    if isFinished game then
        game
    else if isMine position game then
        allMines game
            |> Set.toList
            |> List.map CellRevealed
            |> List.foldr (::) (GameLost :: game)
    else
        let
            propagate =
                neighbourMineCount position game == 0

            ( x, y ) =
                position

            deltas =
                if propagate then
                    neighbourPositionDeltas
                else
                    []

            positions =
                deltas
                    |> List.map (\( dx, dy ) -> ( x + dx, y + dy ))
                    |> List.filter (\pos -> not (isVisible pos game))
        in
            List.foldr revealCell (CellRevealed position :: game) positions


allMines : Game -> Set Position
allMines game =
    case game of
        event :: events ->
            case event of
                GameStarted _ positions ->
                    positions

                _ ->
                    allMines events

        _ ->
            Set.empty


isMine : Position -> Game -> Bool
isMine position game =
    Set.member position (allMines game)


neighbourPositionDeltas : List Position
neighbourPositionDeltas =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]


neighbourMineCount : Position -> Game -> Int
neighbourMineCount position game =
    let
        ( x, y ) =
            position

        neighbours =
            neighbourPositionDeltas
                |> List.map (\( dx, dy ) -> ( x + dx, y + dy ))
                |> List.map (\pos -> isMine pos game)
                |> List.filter identity
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

                GameStarted difficulty _ ->
                    let
                        ( x, y ) =
                            position

                        insideBounds =
                            (x > 0)
                                && (x <= difficulty.width)
                                && (y > 0)
                                && (y <= difficulty.height)
                    in
                        not insideBounds

                _ ->
                    isVisible position events

        _ ->
            False


isFinished : Game -> Bool
isFinished game =
    case game of
        event :: events ->
            case event of
                GameWon ->
                    True

                GameLost ->
                    True

                _ ->
                    isFinished events

        _ ->
            False
