module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Components.TicTacToe as TicTacToe


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { tttModel : TicTacToe.Model }


init : ( Model, Cmd Msg )
init =
    ( { tttModel = TicTacToe.initModel }, Cmd.none )



-- UPDATE


type Msg
    = TicTacToeMsg TicTacToe.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TicTacToeMsg subMsg ->
            let
                ( tttModel, tttCmd ) =
                    TicTacToe.update subMsg model.tttModel
            in
                ( { model | tttModel = tttModel }, Cmd.map TicTacToeMsg tttCmd )



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ -- inline CSS (literal)
          div [ class "row" ]
            [ div [ id "main", class "col-xs-12" ]
                [ Html.map TicTacToeMsg (TicTacToe.view model.tttModel) ]
            ]
        ]
