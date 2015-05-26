module Graph.Util where

import Geometry exposing (Location)
import Structs exposing (..)
import Time exposing (Time)


interpolate : FlightPath -> Float -> (Time, Location)
interpolate fp x = (
        (1 - x) * fp.start.time + x * fp.end.time,
        Geometry.greatCircle fp.start.airport.location fp.end.airport.location x
    )

