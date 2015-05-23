module Structs where

import Airports exposing (Airport)
import Ref
import Time exposing (Time)


type alias Endpoint     = {airport: Airport, time: Time}
type alias EndpointSpec = {airport: String,  time: String}

airportField = Ref.focus .airport (\x m -> {m | airport <- x})
timeField    = Ref.focus .time    (\x m -> {m | time    <- x})

type alias FlightPath     = {start: Endpoint,     end: Endpoint}
type alias FlightPathSpec = {start: EndpointSpec, end: EndpointSpec}

startField = Ref.focus .start (\x m -> {m | start <- x})
endField   = Ref.focus .end   (\x m -> {m | end   <- x})


