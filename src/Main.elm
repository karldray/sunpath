module Main where

import Airports exposing (Airport)
import Debug
import Geometry
import Globe
import Graph
import Html as H exposing (Html)
import Html.Events as E
import Html.Attributes as A
import Http
import LocalTime
import Number.Format exposing (prettyInt, pretty)
import Ref exposing (Ref)
import Result
import Signal exposing (Mailbox, mailbox, (<~), (~))
import String
import Structs exposing (..)
import Style as S
import Task exposing (Task, andThen)
import Time
import Util
import WebGL


endpoint : EndpointSpec -> List Airport -> Result String Endpoint
endpoint spec airports =
    let airportR = Result.fromMaybe "No matching airports found." (List.head (Airports.search spec.airport airports))
        timeR = Result.formatError (always "Specify a time.") (LocalTime.fromString spec.time)
    in  Result.map2 (\a t -> {airport = a, time = LocalTime.toTime a.timezone t}) airportR timeR


type alias Example = {name: String, path: FlightPathSpec}

examples : List Example
examples =
    let endpoint a t = {airport = a, time = t}
        ex startA startT endA endT name =
            { name = name
            , path = {start = endpoint startA startT,
                      end   = endpoint endA endT}
            }
    in  [ ex "SVO" "2014-10-29T16:50" "SFO" "2014-10-29T18:00" "rewind the sunset"
        , ex "MEX" "2015-05-15T12:00" "JAD" "2015-05-16T12:00" "follow the sun"
        , ex "BTU" "2015-05-15T08:00" "BOG" "2015-05-15T22:00" "repeat a day"
        ]
-- format "%Y-%m-%dT%H:%M"


type alias Model =
    { path: FlightPathSpec
    }

pathField = Ref.focus .path (\x m -> {m | path <- x})


initialModel : Model
initialModel =
    { path = (let head (x::_) = x in head examples).path
    }

model : Signal (Ref Model)
model = Ref.new initialModel


animPos : Signal (Ref Float)
animPos = Ref.new 0

port run0 : Signal (Task () ())
port run0 = Util.applyUpdates animPos <| (\dt pos -> Util.mod1 (pos + 0.1 * Time.inSeconds dt)) <~ Time.fps 10


airports : Mailbox (List Airport)
airports = mailbox []

port run1 : Task Http.Error ()
port run1 = Util.logError "load airports" <|
    Http.get Airports.decoder "airports.json" `andThen` Signal.send airports.address


texture : Mailbox (Maybe WebGL.Texture)
texture = mailbox Nothing

port run2 : Task WebGL.Error ()
port run2 = Util.logError "load texture" <|
    WebGL.loadTexture "world.jpg" `andThen` (Signal.send texture.address << Just)


-- view

port title : String
port title = "Sunpath"

main : Signal Html
main = view <~ texture.signal ~ airports.signal ~ model ~ animPos

view : Maybe WebGL.Texture -> List Airport -> Ref Model -> Ref Float -> Html
view texture airports model =
    let pathSpec = Ref.map pathField model

        exampleButton e = H.button [E.onClick (Ref.set pathSpec) e.path] [H.text e.name]

        startR = endpoint pathSpec.value.start airports
        endR   = endpoint pathSpec.value.end   airports
        pathR = Result.map2 FlightPath startR endR

    in  -- writing it this way prevents recomputing
        -- pathR (airport lookup) on every animation tick.
        \animPos ->
            H.div [] [
                H.div [] (List.map exampleButton examples),

                endpointLine "Start:" (Ref.map startField pathSpec) startR,
                endpointLine "End:"   (Ref.map endField   pathSpec) endR,

                case texture of
                    Just t -> H.fromElement <| Globe.globe 300 300 t animPos.value
                    Nothing -> H.text "loading texture for globe..."
                ,

                case pathR of
                    Ok path ->
                        if | path.start.time > path.end.time -> H.text "End time is before start time!"
                           | otherwise -> graphs path
                    _ -> H.text "No graphs to show."
            ]

graphs : FlightPath -> Html
graphs path = H.div []
    [ H.div [] [H.text (stats path)]
    , H.text "Sun altitude:"
    , H.fromElement <| Graph.sunAltitude 500 200 path
    , H.text "Local solar time:"
    , H.fromElement <| Graph.apparentTime 600 400 path
    ]


stats : FlightPath -> String
stats path = -- todo: pretty time deltas, miles
    let dist = Geometry.distance path.start.airport.location path.end.airport.location
        time = (path.end.time - path.start.time) / Time.hour
        speed = dist / time
    in  (pretty 1 ',' time) ++ " hours, " ++
        (prettyInt ',' (round dist)) ++ "km (" ++
        (pretty 1 ',' speed) ++ " km/h)"


endpointLine: String -> Ref EndpointSpec -> Result String Endpoint -> Html
endpointLine label spec result =
    let time = Ref.map timeField spec
        airport = Ref.map airportField spec
    in  H.div [] [
            H.span [A.style S.label] [H.text label],
            stringInput airport [A.type' "text"],
            stringInput time    [A.type' "datetime-local"],
            case result of
                Ok endpoint -> H.text (Airports.toString endpoint.airport)
                Err msg -> H.text msg
        ]


-- UI helpers

stringInput : Ref String -> List H.Attribute -> Html
stringInput str extraAttrs =
    H.input (
        [ A.style S.input
        , A.value str.value
        , E.on "input" E.targetValue (Signal.message (Ref.set str))
        ]
        ++ extraAttrs
    ) []
