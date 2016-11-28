module Components.T10 exposing (..)

import Html exposing (Html, table, tbody, tr, td, text, div, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Utils
import Components.TicTacToe as TicTacToe


type alias Board =
    Array (Array TicTacToe.Model)


type alias Model =
    { currentPos : Maybe ( Int, Int )
    , board : Board
    , winner : Maybe TicTacToe.Spot
    , player : TicTacToe.Spot
    }


initModel : Model
initModel =
    { board = Array.repeat 3 (Array.repeat 3 TicTacToe.initModel)
    , winner = Nothing
    , player = TicTacToe.X
    , currentPos = Nothing
    }


update : TicTacToe.Msg -> Model -> ( Model, Cmd TicTacToe.Msg )
update msg model =
    case msg of
        TicTacToe.Move x y ->
            case model.currentPos of
                Just ( mx, my ) ->
                    let
                        newBoard =
                            performMove model.board mx my x y (Just model.player)
                    in
                        ( { model
                            | board = newBoard
                            , winner = TicTacToe.checkWinner (gridWinners newBoard)
                            , player = TicTacToe.nextPlayer model.player
                            , currentPos = mainSpot newBoard x y
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( { model | currentPos = mainSpot model.board x y }, Cmd.none )

        TicTacToe.Reset ->
            ( initModel, Cmd.none )


mainSpot : Board -> Int -> Int -> Maybe ( Int, Int )
mainSpot board x y =
    case Utils.get2 y x board of
        Just subModel ->
            if TicTacToe.boardFull subModel.board then
                Nothing
            else
                Just ( x, y )

        Nothing ->
            Nothing


gridWinners : Board -> TicTacToe.Board
gridWinners board =
    Array.map (\grid -> Array.map .winner grid) board


performMove : Board -> Int -> Int -> Int -> Int -> Maybe TicTacToe.Spot -> Board
performMove board mx my x y spot =
    case Utils.get2 my mx board of
        Just subModel ->
            let
                newSubBoard =
                    TicTacToe.performMove subModel.board x y spot
            in
                TicTacToe.performMove board
                    mx
                    my
                    { subModel
                        | board = newSubBoard
                        , winner = Utils.otherwise subModel.winner (TicTacToe.checkWinner newSubBoard)
                    }

        Nothing ->
            board


view : Model -> Html TicTacToe.Msg
view model =
    div []
        [ div []
            [ button [ onClick TicTacToe.Reset ] [ text "Reset" ] ]
        , div []
            [ text ("Player: " ++ toString model.player)
            ]
        , div []
            [ text
                ("Winner: "
                    ++ case model.winner of
                        Just spot ->
                            toString spot

                        Nothing ->
                            "None"
                )
            ]
        , table [ class "t10" ]
            [ tbody [] (Array.toList <| Array.indexedMap (tRow model.currentPos) model.board)
            ]
        ]


tRow : Maybe ( Int, Int ) -> Int -> Array (TicTacToe.Model) -> Html TicTacToe.Msg
tRow pos y row =
    tr [] (Array.toList <| Array.indexedMap (tCell pos y) row)


tCell : Maybe ( Int, Int ) -> Int -> Int -> TicTacToe.Model -> Html TicTacToe.Msg
tCell pos y x grid =
    let
        winner =
            case grid.winner of
                Just player ->
                    div [ class "winner visible" ] [ text (toString player) ]

                Nothing ->
                    div [ class "winner hidden" ] []

        sel =
            case pos of
                Just ( sx, sy ) ->
                    if x == sx && y == sy then
                        "selected"
                    else
                        ""

                Nothing ->
                    ""
    in
        td [ class sel ]
            [ winner
            , TicTacToe.tBody grid
            ]
