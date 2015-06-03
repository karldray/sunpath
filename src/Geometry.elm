module Geometry where

import LocalTime exposing (LocalTime)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Time exposing (Time)
import Util exposing (mod1)


-- represent locations on the sphere as unit vectors
type alias Location = Vec3


-- in km
distance : Location -> Location -> Float
distance a b = 6371 * acos (V3.dot a b)

-- (0, 0) -> (1, 0, 0)
fromLatLong : Float -> Float -> Location
fromLatLong lat long =
    let r = cos lat
    in  vec3 (r * (cos long)) (r * (sin long)) (sin lat)

toLatLong : Location -> (Float, Float)
toLatLong v = (asin (V3.getZ v), atan2 (V3.getY v) (V3.getX v))


type alias PathInfo =
    { path : Float -> Location -- great circle arc as function on [0,1]
    , dateLine : Float -- smallest t > 0 for which path(t) = (x,0,z) with x < 0
    }

-- compute the shortest great circle path between two locations
pathInfo : Location -> Location -> PathInfo
pathInfo a b = if a == b then {path x = a, dateLine = 0/0 } else
    let c = V3.normalize (V3.sub b (V3.scale (V3.dot a b) a))
        pos angle = V3.add (V3.scale (cos angle) a) (V3.scale (sin angle) c)
        abAngle = acos (V3.dot a b)
        y0Angle = -- TODO find a cleaner way to do this using atan2
            let m = atan (-(V3.getY a / V3.getY c))
                m' = if m < 0 then m + pi else m
            in  if V3.getX (pos m') < 0 then m' else m' + pi
    in  { path = (*) abAngle >> pos
        , dateLine = y0Angle / abAngle
        }

-- time of year as a float in [0, 1) with summer solstices at 0 and 1
timeOfYear : Time -> Float
timeOfYear t =
    let summerSolstice2015 = 1434904680 * Time.second
    in  mod1 <| (t - summerSolstice2015) / (Time.hour * 24 * 365)

-- latitude at which the sun is directly overhead (annual cycle)
solarDeclination : Time -> Float
solarDeclination t =
    degrees 23.44 * cos (turns (timeOfYear t))

-- longitude at which the sun is directly overhead (daily cycle)
solarNoonLongitude : Time -> Float
solarNoonLongitude t =
    let days = (Time.inSeconds t) / 86400
    in  turns (0.5 - mod1 days)

-- location at which the sun is directly overhead
sunDirection : Time -> Location
sunDirection t = fromLatLong (solarDeclination t) (solarNoonLongitude t)

-- angle of the sun above the horizon
sunAltitude : Location -> Time -> Float
sunAltitude pos t =
    (degrees 90) - acos (V3.dot pos (sunDirection t))


-- longitude -> actual time -> apparent time of day as a float in [0, 1)
solarTimeOfDay : Float -> Time -> Float
solarTimeOfDay long t =
    let longDiff = long - solarNoonLongitude t
    in  mod1 (0.5 + longDiff / (turns 1))

-- longitude -> actual time -> local solar time (discontinuous across date line)
apparentTime : Float -> Time -> LocalTime
apparentTime long t =
    -- TODO make this clearer
    let oneDay = Time.hour * 24
        days = t / oneDay
        dayAtLong0 = floor days
        todAtLong0 = mod1 days
        longTurns = mod1 (long / (turns 1))
        dateOffset = floor (longTurns + todAtLong0) - floor (2 * longTurns)
    in  LocalTime.fromTime LocalTime.utc (oneDay * (
                toFloat (dayAtLong0 + dateOffset) + (solarTimeOfDay long t)
            ))
