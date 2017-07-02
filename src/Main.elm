module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (value, selected)
import Html.Events exposing (..)
import Array exposing (Array)
import Json.Decode as Json


monthToDays : Array Int
monthToDays =
    Array.fromList
        [ 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]


months : List String
months =
    [ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec" ]


days : Int -> List String
days month =
    let
        dayCount =
            monthToDays
                |> Array.get month
                |> Maybe.withDefault 1
    in
        List.range 1 dayCount |> List.map toString


years : List String
years =
    List.range 1926 2009 |> List.map toString


main : Program Never Model Msg
main =
    beginnerProgram
        { model = Model (Date 1996 1 1)
        , view = view
        , update = update
        }


type Msg
    = SetDate Date


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Model =
    { date : Date }


view : Model -> Html Msg
view ({ date } as model) =
    div []
        [ div [] [ text (toString model) ]
        , viewDate date
        ]


viewDate : Date -> Html Msg
viewDate date =
    div []
        [ select
            [ onChange
                (\month ->
                    String.toInt month
                        |> Result.withDefault date.month
                        |> (\selectedMonth -> SetDate { date | month = selectedMonth })
                )
            ]
            (List.indexedMap
                (\index month ->
                    option
                        [ value (toString (index + 1))
                        , selected ((index + 1) == date.month)
                        ]
                        [ text month ]
                )
                months
            )
        , select
            [ onChange
                (\day ->
                    String.toInt day
                        |> Result.withDefault date.day
                        |> (\selectedDay -> SetDate { date | day = selectedDay })
                )
            ]
            (List.indexedMap
                (\index day ->
                    option
                        [ value (toString (index + 1))
                        , selected (date.day == (index + 1))
                        ]
                        [ text day ]
                )
                (days date.month)
            )
        , select
            [ onChange
                (\year ->
                    String.toInt year
                        |> Result.withDefault date.year
                        |> (\selectedYear -> SetDate { date | year = selectedYear })
                )
            ]
            (List.map
                (\year ->
                    option
                        [ value year, selected (year == toString date.year) ]
                        [ text year ]
                )
                years
            )
        ]


onChange : (String -> Msg) -> Attribute Msg
onChange message =
    on "change" (Json.map message targetValue)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetDate date ->
            Model date
