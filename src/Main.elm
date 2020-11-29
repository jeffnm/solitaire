module Main exposing (..)

import Browser
import Cards exposing (..)
import List



-- MODEL


type Model
    = Playing PlayingModel
    | Won


type alias PlayingModel =
    { page_title : String
    , deck : List Cards
    , discard : List Cards
    , endpiles : Endpiles
    , columns : Columns
    , shuffle_count : Int
    , active_card : Maybe Cards
    }


type alias Endpiles =
    { a : List Cards
    , b : List Cards
    , c : List Cards
    , d : List Cards
    }


type alias Columns =
    { a : List Cards
    , b : List Cards
    , c : List Cards
    , d : List Cards
    , e : List Cards
    , f : List Cards
    , g : List Cards
    }
