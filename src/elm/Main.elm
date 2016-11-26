module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- component import example

import Components.Hello exposing (hello)
import Components.TicTacToe as TicTacToe


-- APP


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }
