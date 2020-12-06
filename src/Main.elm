module Main exposing (..)

import Browser
import Cards exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Element.Region as Region
import List
import Random
import Random.List



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MODEL


type Model
    = Playing PlayingModel
    | Won


type alias PlayingModel =
    { deck : Pile
    , discard : Pile
    , endpiles : Endpiles
    , columns : Columns
    , shuffle_count : Int
    , active_card : Maybe Cards
    }


type alias Pile =
    List Cards


type alias Endpiles =
    { a : Pile
    , b : Pile
    , c : Pile
    , d : Pile
    }


type alias Columns =
    { a : Pile
    , b : Pile
    , c : Pile
    , d : Pile
    , e : Pile
    , f : Pile
    , g : Pile
    }


type Msg
    = ShuffledCards Pile
    | CardDrawn
    | Shuffle
    | SelectedCard Cards
    | MovedCard Cards
    | TurnOverCard Cards
    | StartOver


init : () -> ( Model, Cmd Msg )
init _ =
    ( Playing initdata, shuffleCards new52Deck )


initdata : PlayingModel
initdata =
    { deck = []
    , discard = []
    , endpiles = { a = [], b = [], c = [], d = [] }
    , columns = { a = [], b = [], c = [], d = [], e = [], f = [], g = [] }
    , shuffle_count = 0
    , active_card = Nothing
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Won ->
            case msg of
                StartOver ->
                    startOver

                _ ->
                    ( model, Cmd.none )

        Playing playingmodel ->
            case msg of
                Shuffle ->
                    ( Playing { playingmodel | active_card = Nothing }, shuffleCards playingmodel.discard )

                ShuffledCards cards ->
                    if playingmodel.shuffle_count == 0 then
                        case dealCards ( cards, playingmodel.columns ) of
                            ( d, c ) ->
                                ( Playing { playingmodel | deck = d, columns = c, shuffle_count = playingmodel.shuffle_count + 1 }, Cmd.none )

                    else
                        ( Playing { playingmodel | deck = cards, discard = [], shuffle_count = playingmodel.shuffle_count + 1 }, Cmd.none )

                CardDrawn ->
                    ( Playing (drawCard playingmodel), Cmd.none )

                SelectedCard card ->
                    case playingmodel.active_card of
                        Nothing ->
                            ( Playing { playingmodel | active_card = Just card }, Cmd.none )

                        Just active ->
                            if active == card then
                                ( Playing { playingmodel | active_card = Nothing }, Cmd.none )

                            else
                                ( Playing { playingmodel | active_card = Just card }, Cmd.none )

                MovedCard card ->
                    ( Playing (moveCard playingmodel card), Cmd.none )

                TurnOverCard card ->
                    ( Playing (turnOverCard playingmodel card), Cmd.none )

                StartOver ->
                    startOver



--  HELPER FUNCTIONS


turnOverCard : PlayingModel -> Cards -> PlayingModel
turnOverCard playingmodel card =
    let
        columns =
            playingmodel.columns
    in
    case getColumnWithCard columns card of
        Just pile ->
            { playingmodel
                | columns =
                    updateColumn
                        (List.append
                            (List.filter (\c -> c /= card) pile)
                            [ { card | orientation = FaceUp } ]
                        )
                        pile
                        columns
            }

        Nothing ->
            playingmodel


moveCard : PlayingModel -> Cards -> PlayingModel
moveCard playingmodel card =
    let
        columns =
            playingmodel.columns
    in
    case getColumnWithCard columns card of
        Just pile ->
            case playingmodel.active_card of
                Just active_card ->
                    { playingmodel
                        | columns = addCardToColumn (removeCardFromColumn columns active_card) card active_card
                        , active_card = Nothing
                    }

                Nothing ->
                    playingmodel

        Nothing ->
            playingmodel


addCardToColumn : Columns -> Cards -> Cards -> Columns
addCardToColumn columns card active_card =
    case getColumnWithCard columns card of
        Just pile ->
            updateColumn (List.append pile [ active_card ]) pile columns

        Nothing ->
            columns


removeCardFromColumn : Columns -> Cards -> Columns
removeCardFromColumn columns card =
    case getColumnWithCard columns card of
        Just pile ->
            updateColumn (List.filter (\c -> c /= card) pile) pile columns

        Nothing ->
            columns


shuffleCards : Pile -> Cmd Msg
shuffleCards deck =
    Random.generate ShuffledCards (Random.List.shuffle deck)


dealCards : ( Pile, Columns ) -> ( Pile, Columns )
dealCards ( deck, columns ) =
    case deck of
        card :: rest ->
            if List.length rest >= 24 then
                if List.length columns.a < 1 then
                    ( rest, { columns | a = [ { card | orientation = FaceUp } ] } )
                        |> dealCards

                else if List.length columns.b < 2 then
                    if List.length columns.b < 1 then
                        ( rest, { columns | b = List.append columns.b [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | b = List.append columns.b [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else if List.length columns.c < 3 then
                    if List.length columns.c < 2 then
                        ( rest, { columns | c = List.append columns.c [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | c = List.append columns.c [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else if List.length columns.d < 4 then
                    if List.length columns.d < 3 then
                        ( rest, { columns | d = List.append columns.d [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | d = List.append columns.d [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else if List.length columns.e < 5 then
                    if List.length columns.e < 4 then
                        ( rest, { columns | e = List.append columns.e [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | e = List.append columns.e [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else if List.length columns.f < 6 then
                    if List.length columns.f < 5 then
                        ( rest, { columns | f = List.append columns.f [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | f = List.append columns.f [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else if List.length columns.g < 7 then
                    if List.length columns.g < 6 then
                        ( rest, { columns | g = List.append columns.g [ card ] } )
                            |> dealCards

                    else
                        ( rest, { columns | g = List.append columns.g [ { card | orientation = FaceUp } ] } )
                            |> dealCards

                else
                    ( rest, columns )

            else
                ( rest, columns )

        [] ->
            ( deck, columns )


drawCard : PlayingModel -> PlayingModel
drawCard model =
    case model.deck of
        card :: rest ->
            { model | deck = rest, discard = List.append [ { card | orientation = FaceUp } ] model.discard, active_card = Nothing }

        [] ->
            model


getColumnWithCard : { a | a : Pile, b : Pile, c : Pile, d : Pile, e : Pile, f : Pile, g : Pile } -> Cards -> Maybe Pile
getColumnWithCard { a, b, c, d, e, f, g } card =
    if List.member card a then
        Just a

    else if List.member card b then
        Just b

    else if List.member card c then
        Just c

    else if List.member card d then
        Just d

    else if List.member card e then
        Just e

    else if List.member card f then
        Just f

    else if List.member card g then
        Just g

    else
        Nothing


updateColumn : Pile -> Pile -> Columns -> Columns
updateColumn newpile oldpile columns =
    if columns.a == oldpile then
        { columns | a = newpile }

    else if columns.b == oldpile then
        { columns | b = newpile }

    else if columns.c == oldpile then
        { columns | c = newpile }

    else if columns.d == oldpile then
        { columns | d = newpile }

    else if columns.e == oldpile then
        { columns | e = newpile }

    else if columns.f == oldpile then
        { columns | f = newpile }

    else if columns.g == oldpile then
        { columns | g = newpile }

    else
        columns


startOver : ( Model, Cmd msg )
startOver =
    ( Playing initdata, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Won ->
            { title = "You Won!"
            , body =
                [ Element.layout [] viewCelebration
                ]
            }

        Playing playingmodel ->
            { title = "Solitaire"
            , body =
                [ Element.layout []
                    (viewUI playingmodel)
                ]
            }


viewCelebration : Element Msg
viewCelebration =
    el [] (text "Congratulations!")


viewUI : PlayingModel -> Element Msg
viewUI model =
    column []
        [ viewHeader model
        , viewTopBar model
        , viewColumns model
        , viewFooter model
        ]


viewHeader : PlayingModel -> Element Msg
viewHeader model =
    row [ width fill, centerY, spacing 30, padding 30 ]
        [ el [] (text "header")
        ]


viewTopBar : PlayingModel -> Element Msg
viewTopBar model =
    row [ width fill, spacing 30, padding 30 ]
        [ viewDrawPile model.deck
        , viewDiscardPile model.discard model.active_card
        , viewEndPiles model
        ]


viewDiscardPile : List Cards -> Maybe Cards -> Element Msg
viewDiscardPile cards active_card =
    let
        shownCards =
            1

        count =
            List.length cards

        buriedCardsString =
            String.fromInt (count - shownCards)

        topcard =
            List.head cards
    in
    case topcard of
        Nothing ->
            column []
                [ el [] (text "Discard Pile")
                ]

        Just card ->
            if count > shownCards then
                column []
                    [ el [] (text (buriedCardsString ++ " Hidden"))
                    , el [] (viewCard card active_card)
                    ]

            else
                column []
                    [ el [] (viewCard card active_card)
                    ]


viewEndPiles : PlayingModel -> Element Msg
viewEndPiles model =
    column [ alignRight ]
        [ row [ spacing 30 ]
            [ column [] [ el [] (text "end 1") ]
            , column [] [ el [] (text "end 2") ]
            , column [] [ el [] (text "end 3") ]
            , column [] [ el [] (text "end 4") ]
            ]
        ]


viewColumns : PlayingModel -> Element Msg
viewColumns model =
    row [ width fill, spacing 30, padding 30 ]
        [ column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column A") ]
            , column [ spacing 10 ] (viewColumn model.columns.a model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column B") ]
            , column [ spacing 10 ] (viewColumn model.columns.b model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column C") ]
            , column [ spacing 10 ] (viewColumn model.columns.c model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column D") ]
            , column [ spacing 10 ] (viewColumn model.columns.d model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column E") ]
            , column [ spacing 10 ] (viewColumn model.columns.e model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column F") ]
            , column [ spacing 10 ] (viewColumn model.columns.f model.active_card)
            ]
        , column [ alignTop, spacing 30 ]
            [ paragraph [] [ el [] (text "Column G") ]
            , column [ spacing 10 ] (viewColumn model.columns.g model.active_card)
            ]
        ]


viewFooter : PlayingModel -> Element msg
viewFooter model =
    row [] []


viewDrawPile : List Cards -> Element Msg
viewDrawPile deck =
    let
        remainingCardsString =
            List.length deck
                |> String.fromInt
    in
    case deck of
        [] ->
            column []
                [ el [ Events.onClick Shuffle, pointer ] (text "Shuffle")
                ]

        card :: rest ->
            column []
                [ el [] (text (remainingCardsString ++ " cards"))
                , el [ Events.onClick CardDrawn, pointer ] (text "Draw Card")
                ]


viewListAllCardsInDeck : PlayingModel -> List (Element Msg)
viewListAllCardsInDeck model =
    case model.deck of
        cards ->
            List.map (\c -> column [] [ viewCard c model.active_card ]) cards


viewColumn : List Cards -> Maybe Cards -> List (Element Msg)
viewColumn cardcolumn active_card =
    let
        length =
            List.length cardcolumn
    in
    List.take (length - 1) cardcolumn
        |> List.map
            (\c ->
                el [] (viewCard c active_card)
            )
        |> (\els ->
                List.map
                    (\c ->
                        el [] (viewTopCard c active_card)
                    )
                    (List.drop (length - 1) cardcolumn)
                    |> List.append els
           )


viewCard : Cards -> Maybe Cards -> Element Msg
viewCard card active_card =
    case card.orientation of
        FaceDown ->
            el [ Background.color (rgb 0.5 0.5 0.5) ] (text "Facedown")

        FaceUp ->
            el [ Background.color (rgb 0.9 0.9 0.9) ] (text (cardValueToString card.value ++ " of " ++ suitToString card.suit))


viewTopCard : Cards -> Maybe Cards -> Element Msg
viewTopCard card active_card =
    case card.orientation of
        FaceDown ->
            el [ Background.color (rgb 0.5 0.5 0.5), onClick (TurnOverCard card), pointer ] (text "Facedown")

        FaceUp ->
            case active_card of
                Nothing ->
                    el [ Background.color (rgb 0.9 0.9 0.9), onClick (SelectedCard card), pointer ] (text (cardValueToString card.value ++ " of " ++ suitToString card.suit))

                Just c ->
                    if c == card then
                        el [ Background.color (rgb 0 1 0), onClick (SelectedCard card), pointer ] (text (cardValueToString card.value ++ " of " ++ suitToString card.suit ++ " ACTIVE"))

                    else
                        el [ Background.color (rgb 0.5 0.5 0.5), onClick (MovedCard card), pointer ] (text (cardValueToString card.value ++ " of " ++ suitToString card.suit))
