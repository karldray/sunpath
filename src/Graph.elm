module Graph where

import Color
import Geometry exposing (Location)
import Graph.Util
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import LocalTime
import MapBackground exposing (mapBackground)
import Structs exposing (..)
import Style
import SunColorGL exposing (sunColorGL)
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
            in  LocalTime.toTime LocalTime.utc (Geometry.apparentTime long t)

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
        paths = List.map (path >> traced (solid Color.black)) splitPoints

    in
        \animPos ->
            let
                t = Graph.Util.interpolateTime fp animPos
                sunDirection = Geometry.sunDirection t
            in
                flow outward
                    [ mapBackground h tex sunDirection
                    , collage (2 * h) h
                        (paths ++ [
                        --, circle 5 |> move ...
                        ])
                    ]
