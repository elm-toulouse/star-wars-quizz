module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http as Http exposing (expectJson)
import Json.Decode as Decode exposing (Decoder)
import Random as Random exposing (Generator, Seed)


main : Program () Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



{-
   Model / Msg
-}


type alias Model =
    { questionnaire : Questionnaire
    , seed : Seed
    , notification : Maybe Notification
    }


type Notification
    = Error String
    | Info String


type Questionnaire
    = PreparingQuestion
    | AskingQuestion Question
    | Errored


type alias Question =
    { question : String
    , goodAnswer : String
    , wrongAnswers : List String
    }


type Msg
    = GotNewQuestion { seed : Seed, question : Question }
    | GotHttpError Http.Error
    | GotAnswer String
    | DismissNotification
    | Retry



{-
   Update
-}


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { questionnaire = PreparingQuestion
            , seed = Random.initialSeed 14
            , notification = Nothing
            }
    in
    ( model, nextQuestion model.seed )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.questionnaire ) of
        ( Retry, Errored ) ->
            ( { model | questionnaire = PreparingQuestion }, nextQuestion model.seed )

        ( DismissNotification, _ ) ->
            ( { model | notification = Nothing }, Cmd.none )

        ( GotNewQuestion { question, seed }, PreparingQuestion ) ->
            ( { model | questionnaire = AskingQuestion question, seed = seed }, Cmd.none )

        ( GotAnswer answer, AskingQuestion { goodAnswer } ) ->
            let
                notification =
                    if answer == goodAnswer then
                        Just <| Info <| "Correct! ⊂(✰‿✰)つ"

                    else
                        Just <| Error <| "Incorrect! (✖╭╮✖)"
            in
            ( { model | questionnaire = PreparingQuestion, notification = notification }, nextQuestion model.seed )

        ( GotHttpError err, PreparingQuestion ) ->
            let
                notification =
                    Just <| Error <| "Ho crap! We got an error when reaching out the server: " ++ httpErrorToString err
            in
            ( { model | questionnaire = Errored, notification = notification }, Cmd.none )

        ( _, _ ) ->
            let
                notification =
                    Just <| Error <| """
                    Something went seriously wrong. We've reached an impossible branch of our model. It probably means that in wasn't that impossible.
                    Note that this error isn't especially useful as it stands because it gives us no clear way to debug what has happened. However,
                    it's 18:31 and I clearly have no time to deal with that correctly before the meetup get started ¯\\_(ツ)_/¯.
                  """
            in
            ( { model | notification = notification }, Cmd.none )



{-
   Effects
-}


type alias QuestionBuilder a =
    ( a, List a ) -> Question


fetchPeople : Seed -> QuestionBuilder People -> Cmd Msg
fetchPeople seed builder =
    Http.get
        { url = "https://swapi.co/api/people/"
        , expect = expectJson (handleHttpResult seed builder) (Decode.field "results" (Decode.list peopleDecoder))
        }


nextQuestion : Seed -> Cmd Msg
nextQuestion seed =
    let
        ( category, newSeed ) =
            Random.step arbitraryCategory seed
    in
    case category of
        CPeople Height ->
            fetchPeople newSeed <|
                \( right, wrongs ) ->
                    { question = "How tall is " ++ right.name ++ "?"
                    , goodAnswer = right.height
                    , wrongAnswers = List.map .height wrongs
                    }

        CPeople Mass ->
            fetchPeople newSeed <|
                \( right, wrongs ) ->
                    { question = "How much does " ++ right.name ++ " weight?"
                    , goodAnswer = right.mass
                    , wrongAnswers = List.map .mass wrongs
                    }

        CPeople Gender ->
            fetchPeople newSeed <|
                \( right, wrongs ) ->
                    { question = "What is the gender of " ++ right.name ++ " ?"
                    , goodAnswer = right.gender
                    , wrongAnswers = List.map .gender wrongs
                    }



{-
   HTTP Layer
-}


handleHttpResult : Seed -> QuestionBuilder a -> Result Http.Error (List a) -> Msg
handleHttpResult seed builder result =
    case result of
        Ok (h :: q) ->
            let
                -- NOTE
                -- With the following approach, there's a chance that we yield the same
                -- element multiple time. Though in practice, most resources have _many_
                -- elements and we only select 4 of them. Hence, we shouldn't end up
                -- with that many duplicates over time.
                arbitraryAnswer =
                    Random.uniform h q

                arbitraryQuestion =
                    Random.map4
                        (\a b c d -> builder ( a, [ b, c, d ] ))
                        arbitraryAnswer
                        arbitraryAnswer
                        arbitraryAnswer
                        arbitraryAnswer

                ( question, newSeed ) =
                    Random.step arbitraryQuestion seed
            in
            GotNewQuestion { seed = newSeed, question = question }

        Ok _ ->
            GotHttpError (Http.BadBody "not enough resources returned by the server!")

        Err err ->
            GotHttpError err


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "BadUrl: the provided URL is invalid: " ++ url

        Http.Timeout ->
            "Timeout: it took too long to get a response."

        Http.NetworkError ->
            "NetworkError: Wi-Fi seems turned off, or you may be in a cave?"

        Http.BadStatus status ->
            "BadStatus: We got a response back from the server with an error status: " ++ String.fromInt status

        Http.BadBody body ->
            "BadBody: We got a successful response back but couldn't parse the response body correctly: " ++ body



{-
   Resources
-}


type Category
    = CPeople PeopleField


type PeopleField
    = Height
    | Mass
    | Gender


arbitraryCategory : Generator Category
arbitraryCategory =
    Random.uniform (CPeople Height)
        [ CPeople Mass
        , CPeople Gender
        ]


type alias People =
    { name : String
    , mass : String
    , height : String
    , gender : String
    }


peopleDecoder : Decoder People
peopleDecoder =
    Decode.map4 People
        (Decode.field "name" Decode.string)
        (Decode.field "mass" Decode.string)
        (Decode.field "height" Decode.string)
        (Decode.field "gender" Decode.string)



{-
   View
-}


view : Model -> Document Msg
view { questionnaire, notification } =
    { title = "Elm Toulouse #2"
    , body =
        [ viewNotification notification
        , viewQuestionnaire questionnaire
        ]
    }


viewQuestionnaire : Questionnaire -> Html Msg
viewQuestionnaire questionnaire =
    case questionnaire of
        PreparingQuestion ->
            div [ class "container" ] [ text "In a galaxy far far away..." ]

        AskingQuestion { question, goodAnswer, wrongAnswers } ->
            div
                [ class "container" ]
                [ p [] [ text question ]
                , div [ class "answers" ] <| List.map viewAnswer (goodAnswer :: wrongAnswers)
                ]

        Errored ->
            div
                [ class "container" ]
                [ p [] [ text "Oops! Something Went Wrong!" ]
                , button [ onClick Retry ] [ text "RETRY AND CROSS MY FINGERS" ]
                ]


viewAnswer : String -> Html Msg
viewAnswer answer =
    div
        [ class "answer"
        , onClick (GotAnswer answer)
        ]
        [ text answer ]


viewNotification : Maybe Notification -> Html Msg
viewNotification notification =
    case notification of
        Nothing ->
            div [] []

        Just (Error msg) ->
            div [ class "notification-error", onClick DismissNotification ] [ text msg ]

        Just (Info msg) ->
            div [ class "notification-info", onClick DismissNotification ] [ text msg ]
