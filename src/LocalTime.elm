module LocalTime (LocalTime, Timezone, fromString, display, toTime, fromTime, offset, user, utc) where

import Date
import Time exposing (Time)

type LocalTime = LocalTime Time -- we represent a "local time" as the actual point in time at which a UTC clock would show it
type Timezone = Timezone Time -- UTC offset


-- input string should be ISO without the timezone
-- (as produced by <input type="datetime-local">)
fromString : String -> Result String LocalTime
fromString s = Result.map (LocalTime << Date.toTime) (Date.fromString (s ++ "Z"))

-- construct a timezone from a UTC offset in hours
offset : Float -> Timezone
offset x = Timezone (Time.hour * x)


toTime : Timezone -> LocalTime -> Time
toTime (Timezone offset) (LocalTime time) = time - offset

fromTime : Timezone -> Time -> LocalTime
fromTime (Timezone offset) time = LocalTime (time + offset)


-- produce a Date that, when formatted, shows the given time
display : LocalTime -> Date.Date
display = toTime user >> Date.fromTime


utc : Timezone
utc = Timezone 0

user : Timezone
user =
    let -- interpreted as local since not an ISO format string
        s = "April 17, 1987 12:00 AM"
    in  case (Date.fromString s, Date.fromString (s ++ " UTC")) of
            (Ok localNoon, Ok utcNoon) ->
                -- if UTC noon comes after our noon, then we're ahead of UTC
                Timezone (Date.toTime utcNoon - Date.toTime localNoon)
