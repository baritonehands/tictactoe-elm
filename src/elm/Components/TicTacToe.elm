module Components.TicTacToe exposing (..)

import Html exposing (Html, table, tbody, tr, td, text, div, button)
import Html.Events exposing (onClick)
import Array exposing (Array)


-- MODEL


type alias Board =
    Array (Array (Maybe Spot))


type alias Model =
    { board : Board
    , winner : Maybe Spot
    , player : Spot
    }


initModel : Model
initModel =
    { board = Array.repeat 3 (Array.repeat 3 Nothing)
    , winner = Nothing
    , player = X
    }


type Spot
    = X
    | O


type Msg
    = Move Int Int
    | Reset



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move x y ->
            let
                newBoard =
                    performMove model.board x y model.player
            in
                ( { model
                    | board = newBoard
                    , winner = checkWinner newBoard
                    , player = nextPlayer model.player
                  }
                , Cmd.none
                )

        Reset ->
            ( initModel, Cmd.none )


nextPlayer : Spot -> Spot
nextPlayer player =
    case player of
        X ->
            O

        O ->
            X


performMove : Board -> Int -> Int -> Spot -> Board
performMove board x y spot =
    case Array.get y board of
        Just row ->
            Array.set y (Array.set x (Just spot) row) board

        Nothing ->
            board


winningCombo : List (Maybe Spot) -> Maybe Spot
winningCombo combo =
    case combo of
        [ Just a, Just b, Just c ] ->
            if a == b && b == c then
                Just a
            else
                Nothing

        _ ->
            Nothing


checkWinner : Board -> Maybe Spot
checkWinner board =
    case Array.toList <| Array.map Array.toList board of
        [ [ a, b, c ], [ d, e, f ], [ g, h, i ] ] ->
            List.map winningCombo
                [ [ a, b, c ]
                , [ d, e, f ]
                , [ g, h, i ]
                , [ a, d, g ]
                , [ b, e, h ]
                , [ c, f, i ]
                , [ a, e, i ]
                , [ c, e, g ]
                ]
                |> List.filter (\w -> w /= Nothing)
                |> List.head
                |> Maybe.withDefault Nothing

        _ ->
            Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Reset ] [ text "Reset" ] ]
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
        , table []
            [ tbody [] (Array.toList <| Array.indexedMap tRow model.board)
            ]
        ]


tRow : Int -> Array (Maybe Spot) -> Html Msg
tRow y row =
    tr [] (Array.toList <| Array.indexedMap (tCell y) row)


tCell : Int -> Int -> Maybe Spot -> Html Msg
tCell y x spot =
    case spot of
        Just x ->
            td [] [ text <| toString x ]

        Nothing ->
            td [ onClick <| Move x y ] []
