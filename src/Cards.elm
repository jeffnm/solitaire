module Cards exposing (..)

import Html.Attributes exposing (value)


type alias Cards =
    { suit : Suit
    , value : CardValue
    , orientation : CardOrientation
    , color : CardColor
    }


type CardValue
    = CardValue Int


type CardOrientation
    = FaceDown
    | FaceUp


type Suit
    = Clubs
    | Spades
    | Hearts
    | Diamonds


type CardColor
    = Red
    | Black


cardValueToString : CardValue -> String
cardValueToString value =
    case value of
        CardValue card ->
            if card == 13 then
                "King"

            else if card == 12 then
                "Queen"

            else if card == 11 then
                "Jack"

            else if card == 1 then
                "Ace"

            else
                String.fromInt card


cardValuetoInt : CardValue -> Int
cardValuetoInt value =
    case value of
        CardValue card ->
            card


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "Clubs"

        Spades ->
            "Spades"

        Diamonds ->
            "Diamonds"

        Hearts ->
            "Hearts"


new52Deck : List Cards
new52Deck =
    let
        deck =
            []

        cardspersuit =
            List.range 1 13

        listclubs =
            List.map (\c -> Cards Clubs (CardValue c) FaceDown Black) cardspersuit

        listspades =
            List.map (\c -> Cards Spades (CardValue c) FaceDown Black) cardspersuit

        listdiamonds =
            List.map (\c -> Cards Diamonds (CardValue c) FaceDown Red) cardspersuit

        listhearts =
            List.map (\c -> Cards Hearts (CardValue c) FaceDown Red) cardspersuit
    in
    List.append deck listclubs
        |> List.append listspades
        |> List.append listdiamonds
        |> List.append listhearts
