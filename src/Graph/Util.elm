module Graph.Util where

import Geometry exposing (PathInfo)
import Structs exposing (..)
import Time exposing (Time)


flightPathInfo : FlightPath -> PathInfo
flightPathInfo fp = Geometry.pathInfo fp.start.airport.location fp.end.airport.location

interpolateTime : FlightPath -> Float -> Time
interpolateTime fp x = (1 - x) * fp.start.time + x * fp.end.time
