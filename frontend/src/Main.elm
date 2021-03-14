module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html, br, button, datalist, div, input, label, li, option, text, ul)
import Html.Attributes exposing (href, id, list, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as E
import Set exposing (Set)
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


type Msg
    = OnUrlRequest
    | OnUrlChange
    | OnChangeNewMedicineInput String
    | SubmitNewMedicineInput
    | DeleteMedicine String
    | OnChangeNewIncompatibleListDrug String
    | SubmitNewIncompatibleListDrug
    | DeleteNewIncompatibleListDrug Int
    | OnChangeNewIncompatibleDrug Int String
    | SubmitNewIncompatibleDrug Int
    | DeleteNewIncompatibleDrug Int String
    | OnChangeNewInputDrug String
    | SubmitNewInputDrug
    | DeleteNewInputDrug String


type alias Model =
    { medicines : Set MedicineType
    , incompatibleDrug : IncompatibleDrugListType
    , inputDrug : Set MedicineType
    , newMedicineInput : String
    , newIncompatibleDrug : String
    , newInputDrug : String
    }


type alias MedicineType =
    String


type alias IncompatibleDrugListType =
    List IncompatibleDrugType


type alias IncompatibleDrugType =
    { medicines : Set MedicineType
    , newIncompatibleDrug : String
    }


initializeModel : Url -> Model
initializeModel _ =
    { medicines = Set.empty
    , incompatibleDrug = []
    , inputDrug = Set.empty
    , newMedicineInput = ""
    , newIncompatibleDrug = ""
    , newInputDrug = ""
    }


init : () -> Url -> Key -> ( Model, Cmd msg )
init _ url _ =
    ( initializeModel url, Cmd.none )


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ div []
            [ text "新しい薬の登録"
            , br [] []
            , input
                [ onInput OnChangeNewMedicineInput
                , value model.newMedicineInput
                ]
                []
            , button [ onClick SubmitNewMedicineInput ] [ text "登録" ]
            , br [] []
            , ul []
                (model.medicines
                    |> Set.toList
                    |> List.map (\s -> li [] [ text s, button [ onClick <| DeleteMedicine s ] [ text "削除" ] ])
                )
            ]
        , br [] []
        , div []
            [ text "組み合わせてはいけない薬のリストを登録"
            , br [] []
            , input
                [ type_ "search"
                , list "newList"
                , onInput OnChangeNewIncompatibleListDrug
                , value model.newIncompatibleDrug
                ]
                []
            , datalist [ id "newList" ]
                (model.medicines
                    |> Set.toList
                    |> List.map (\s -> option [] [ text s ])
                )
            , button [ onClick SubmitNewIncompatibleListDrug ] [ text "登録" ]
            , ul []
                (model.incompatibleDrug
                    |> List.indexedMap
                        (\index ->
                            \incompatibleDrug ->
                                Set.toList incompatibleDrug.medicines
                                    |> (\medicine ->
                                            li []
                                                [ ul []
                                                    (medicine
                                                        |> List.map
                                                            (\now ->
                                                                li []
                                                                    [ text now
                                                                    , button
                                                                        [ onClick <| DeleteNewIncompatibleDrug index now
                                                                        ]
                                                                        [ text "削除" ]
                                                                    ]
                                                            )
                                                    )
                                                , input
                                                    [ type_ "search"
                                                    , list <| "IncompatibleDrugList_" ++ String.fromInt index
                                                    , onInput <| OnChangeNewIncompatibleDrug index
                                                    , value incompatibleDrug.newIncompatibleDrug
                                                    ]
                                                    []
                                                , datalist [ id <| "IncompatibleDrugList_" ++ String.fromInt index ]
                                                    (Set.diff model.medicines incompatibleDrug.medicines
                                                        |> Set.toList
                                                        |> List.map (\medicineData -> option [] [ text medicineData ])
                                                    )
                                                , button [ onClick <| SubmitNewIncompatibleDrug index ] [ text "登録" ]
                                                , button [ onClick <| DeleteNewIncompatibleListDrug index ] [ text "削除" ]
                                                ]
                                       )
                        )
                )
            ]
        , br [] []
        , div []
            [ text "判定する薬"
            , br [] []
            , input
                [ type_ "search"
                , list "setList"
                , onInput OnChangeNewInputDrug
                , value model.newInputDrug
                ]
                []
            , datalist [ id "setList" ]
                (Set.diff model.medicines model.inputDrug
                    |> Set.toList
                    |> List.map (\s -> option [] [ text s ])
                )
            , button [ onClick SubmitNewInputDrug ] [ text "登録" ]
            , br [] []
            , ul []
                (model.inputDrug
                    |> Set.toList
                    |> List.map (\s -> li [] [ text s, button [ onClick <| DeleteNewInputDrug s ] [ text "削除" ] ])
                )
            ]
        , br [] []
        , div []
            [ text "判定結果:"
            , let
                result =
                    List.any (\ng -> Set.diff ng.medicines model.inputDrug |> Set.isEmpty) model.incompatibleDrug
              in
              text
                (if result then
                    "ng"

                 else
                    "ok"
                )
            ]
        ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnChangeNewMedicineInput newMedicineString ->
            ( { model | newMedicineInput = newMedicineString }, Cmd.none )

        SubmitNewMedicineInput ->
            ( { model
                | newMedicineInput = ""
                , medicines = Set.insert model.newMedicineInput model.medicines
              }
            , Cmd.none
            )

        DeleteMedicine medicine ->
            ( { model | medicines = Set.remove medicine model.medicines }, Cmd.none )

        OnChangeNewIncompatibleListDrug newIncompatibleDrug ->
            ( { model | newIncompatibleDrug = newIncompatibleDrug }, Cmd.none )

        SubmitNewIncompatibleListDrug ->
            ( { model
                | incompatibleDrug =
                    model.incompatibleDrug
                        ++ [ { medicines = Set.singleton model.newIncompatibleDrug, newIncompatibleDrug = "" }
                           ]
                , newIncompatibleDrug = ""
              }
            , Cmd.none
            )

        DeleteNewIncompatibleListDrug index ->
            let
                newIncompatibleDrug =
                    List.take index model.incompatibleDrug
                        ++ List.drop (index + 1) model.incompatibleDrug
            in
            ( { model | incompatibleDrug = newIncompatibleDrug }, Cmd.none )

        OnChangeNewIncompatibleDrug index newIncompatibleDrugString ->
            let
                newIncompatibleDrug =
                    List.take index model.incompatibleDrug
                        ++ List.map (\m -> { m | newIncompatibleDrug = newIncompatibleDrugString }) (List.drop index model.incompatibleDrug |> List.take 1)
                        ++ List.drop (index + 1) model.incompatibleDrug
            in
            ( { model | incompatibleDrug = newIncompatibleDrug }, Cmd.none )

        SubmitNewIncompatibleDrug index ->
            let
                newIncompatibleDrug =
                    List.take index model.incompatibleDrug
                        ++ List.map (\m -> { m | medicines = Set.insert m.newIncompatibleDrug m.medicines, newIncompatibleDrug = "" }) (List.drop index model.incompatibleDrug |> List.take 1)
                        ++ List.drop (index + 1) model.incompatibleDrug
            in
            ( { model | incompatibleDrug = newIncompatibleDrug }, Cmd.none )

        DeleteNewIncompatibleDrug index s ->
            let
                newIncompatibleDrug =
                    List.take index model.incompatibleDrug
                        ++ List.map (\m -> { m | medicines = Set.remove s m.medicines }) (List.drop index model.incompatibleDrug |> List.take 1)
                        ++ List.drop (index + 1) model.incompatibleDrug
            in
            ( { model | incompatibleDrug = newIncompatibleDrug }, Cmd.none )

        OnChangeNewInputDrug s ->
            ( { model | newInputDrug = s }, Cmd.none )

        SubmitNewInputDrug ->
            ( { model | inputDrug = Set.insert model.newInputDrug model.inputDrug }, Cmd.none )

        DeleteNewInputDrug s ->
            ( { model | inputDrug = Set.remove s model.inputDrug }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


onUrlRequest : UrlRequest -> Msg
onUrlRequest _ =
    OnUrlRequest


onUrlChange : Url -> Msg
onUrlChange _ =
    OnUrlChange
