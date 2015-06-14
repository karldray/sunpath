module Graph where

import Color
import Date
import Date.Format
import Geometry exposing (Location)
import Graph.Util
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import LocalTime as LT
import MapBackground exposing (mapBackground)
import Structs exposing (..)
import Style
import SunColorGL exposing (sunColorGL)
import Text as T
import Time exposing (hour)
import Util
import WebGL exposing (Texture)


sunAltitude : Int -> Int -> FlightPath -> Element
sunAltitude w h fp =
    let xvals = Util.divideUnit (w // 2)
        yscale = (toFloat h / 2) / degrees 90
        pathInfo = Graph.Util.flightPathInfo fp
        f x =
            let pos = pathInfo.path x
                t = Graph.Util.interpolateTime fp x
            in  Geometry.sunAltitude pos t

        points = List.map (\x -> ((toFloat w) * (x - 0.5), yscale * (f x))) xvals
        (w', h') = (toFloat w, toFloat h)

    in  collage w h (
            [ rect w' h'
                |> gradient (Color.linear (0, -0.5 * h') (0, 0.5 * h')
                    [ (0.0, Style.midnightColor)
                    , (0.5 - Style.colorStopAngle / pi, Style.darkColor)
                    , (0.5 + Style.colorStopAngle / pi, Style.lightColor)
                    , (1.0, Style.noonColor)
                    ])
            , traced (solid Color.black) (path points)
            ]
        )

apparentTime : Int -> Int -> FlightPath -> Element
apparentTime w h fp =
    let pathInfo = Graph.Util.flightPathInfo fp
        f x =
            let t = Graph.Util.interpolateTime fp x
                (lat, long) = Geometry.toLatLong (pathInfo.path x)
            in  LT.toTime LT.utc (Geometry.apparentTime long t)

        xvals = Util.divideUnit (h // 2)
        tvals = List.map f xvals

        (Just tmax') = List.maximum tvals
        (Just tmin') = List.minimum tvals
        range = tmax' - tmin'
        tmax = tmax' + 0.1 * range
        tmin = tmin' - 0.1 * range
        tmid = tmin + (tmax - tmin) / 2

        xscale = toFloat h
        tscale = toFloat w / (tmax - tmin)

        points = List.map2 (\x t -> (tscale * (t - tmid), xscale * (0.5 - x))) xvals tvals

        splitPoints = Util.splitAt pathInfo.dateLine xvals points
        paths = List.map (path >> traced (solid Color.black)) splitPoints

    in  flow outward [
        sunColorGL w h fp tmin tmax,
        collage w h paths
    ]

flatMap : Int -> Texture -> FlightPath -> Float -> Element
flatMap h tex fp =
    let
        scale = toFloat h / pi
        pathInfo = Graph.Util.flightPathInfo fp
        f = pathInfo.path >> Geometry.toLatLong >>
                \(lat, long) -> (scale * long, scale * lat)

        npoints = round <| 0.01 *
            Geometry.distance fp.start.airport.location fp.end.airport.location

        xvals = Util.divideUnit npoints
        points = List.map f xvals

        splitPoints = Util.splitAt pathInfo.dateLine xvals points
        paths = List.map (path >> traced (dotted Color.black)) splitPoints

        h' = toFloat h
    in
        \animPos ->
            let
                t = Graph.Util.interpolateTime fp animPos
                sunDirection = Geometry.sunDirection t
                noonLong = Geometry.solarNoonLongitude t
                midnightLong = if noonLong >= 0 then noonLong - turns 0.5
                                                else noonLong + turns 0.5
                dateLineX = h' * midnightLong / turns 0.5

                displayDate = LT.fromTime LT.utc >> LT.display >> Date.Format.format "%a %b %d"
                    >> T.fromString >> T.color Color.grey >> text >> rotate (turns 0.25)
            in
                flow outward
                    [ mapBackground h tex sunDirection
                    , collage (2 * h) h
                        (
                            [ segment (dateLineX, h'/2) (dateLineX,-h'/2)
                                |> traced (solid Color.grey)
                            , displayDate (t - 12 * hour)
                                |> move (dateLineX - 12, 0)
                            , displayDate (t + 12 * hour)
                                |> move (dateLineX + 9, 0)
                        ] ++ paths ++ [
                        --, circle 5 |> move ...
                        ])
                    ]
