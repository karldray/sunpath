module Airports (Airport, toString, search, decoder) where

import Geometry exposing (Location)
import LocalTime exposing (LocalTime, Timezone)

import Basics
import Json.Decode as J
import String as S
import Task exposing (Task, andThen)


type alias Airport =
    { codes : List String
    , name : String
    , city : String
    , location : Location
    , timezone : Timezone
    }

toString : Airport -> String
toString a = (S.join "/" a.codes) ++ ": " ++ a.name ++ " (" ++ a.city ++ ")"

search : String -> List Airport -> List Airport
search query airports =
    if S.isEmpty query then [] else
        let upperQ = S.toUpper query
            containsQ a = List.any (S.toUpper >> S.contains upperQ) <| [a.name, a.city] ++ a.codes
            matchesCode a = List.any ((==) upperQ) a.codes
            (front, back) = List.partition matchesCode (List.filter containsQ airports)
        in  front ++ back


decoder : J.Decoder (List Airport)
decoder = J.list (J.customDecoder (J.list J.string) fromRow)

fromRow : List String -> Result String Airport
fromRow row =
    case row of
        [id, name, city, country, iatafaa, icao, lat, long, alt, tzoffset, dst, tzname] ->
            case (S.toFloat lat, S.toFloat long, S.toFloat tzoffset) of
                (Ok lat', Ok long', Ok tzoffset') -> Ok
                    { codes = List.filter (not << S.isEmpty) [iatafaa, icao]
                    , name = name
                    , city = city ++ ", " ++ country
                    , location = Geometry.fromLatLong (degrees lat') (degrees long')
                    , timezone = LocalTime.offset tzoffset'
                    }
                _ -> Err ("Couldn't parse at least one of lat, long, tzoffset as float: " ++ Basics.toString row)
        _ -> Err ("Wrong number of fields in airport row: " ++ Basics.toString row)
