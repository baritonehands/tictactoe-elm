module Components.TicTacToe exposing (..)

import Html exposing (Html, table, tbody, tr, td, text, div, button)
import Html.Events exposing (onClick)


-- MODEL


type alias Board =
    List (List (Maybe Spot))


type alias Model =
    { board : Board
    , winner : Maybe Spot
    , player : Spot
    }


initModel : Model
initModel =
    { board = List.repeat 3 (List.repeat 3 Nothing)
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
                    , player =
                        case model.player of
                            X ->
                                O

                            O ->
                                X
                  }
                , Cmd.none
                )

        Reset ->
            ( initModel, Cmd.none )


performMove : Board -> Int -> Int -> Spot -> Board
performMove board x y spot =
    let
        replace n list v =
            List.take n list ++ v :: List.drop (n + 1) list

        get n list default =
            Maybe.withDefault default <| List.head <| List.drop n list
    in
        replace y board <| replace x (get y board []) (Just spot)


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
    case board of
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
            [ tbody [] <| List.indexedMap tRow model.board
            ]
        ]


tRow : Int -> List (Maybe Spot) -> Html Msg
tRow y row =
    tr [] <| List.indexedMap (tCell y) row


tCell : Int -> Int -> Maybe Spot -> Html Msg
tCell y x spot =
    case spot of
        Just x ->
            td [] [ text <| toString x ]

        Nothing ->
            td [ onClick <| Move x y ] []
