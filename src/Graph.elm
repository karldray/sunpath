module Graph where

import Color
import Geometry exposing (Location)
import Graph.Util exposing (interpolate)
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
    let npoints = w // 2
        xvals = List.map (\x -> (toFloat x) / (toFloat npoints)) [0..npoints]

        yscale = (toFloat h / 2) / degrees 90
        f = interpolate fp >> \(t, pos) -> Geometry.sunAltitude pos t

        points = List.map (\x -> ((toFloat w) * (x - 0.5), yscale * (f x))) xvals

        (w', h') = (toFloat w, toFloat h)

    in  collage w h [
        rect w' h'
            |> gradient (Color.linear (0, -0.5 * h') (0, 0.5 * h')
                [ (0.0, Style.midnightColor)
                , (0.5 - Style.colorStopAngle / pi, Style.darkColor)
                , (0.5 + Style.colorStopAngle / pi, Style.lightColor)
                , (1.0, Style.noonColor)
                ]),
        traced (solid Color.black) (path points)
    ]

apparentTime : Int -> Int -> FlightPath -> Element
apparentTime w h fp =
    let f x =
            let (t, pos) = interpolate fp x
                (lat, long) = Geometry.toLatLong pos
            in  LocalTime.toTime LocalTime.utc (Geometry.apparentTime long t)

        npoints = h // 2
        xvals = List.map (\i -> (toFloat i) / (toFloat npoints)) [0..npoints]
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

    in  flow outward [
        sunColorGL w h fp tmin tmax,
        collage w h [
            traced (solid Color.black) (path points)
        ]
    ]

flatMap : Int -> Texture -> FlightPath -> Float -> Element
flatMap h tex fp =
    let
        scale = toFloat h / pi
        f = interpolate fp >> snd >> Geometry.toLatLong >>
                \(lat, long) -> (scale * long, scale * lat)

        npoints = round <| 0.01 *
            Geometry.distance fp.start.airport.location fp.end.airport.location

        step = 1.0 / toFloat npoints
        points = List.map (toFloat >> (*) step >> f) [0..npoints]

        pathArc = traced (solid Color.black) (path points)
    in
        \animPos ->
            let
                (t, pos) = interpolate fp animPos
                sunDirection = Geometry.sunDirection t
            in
                flow outward
                    [ mapBackground h tex sunDirection
                    , collage (2 * h) h
                        [ pathArc
                        --, circle 5 |> move ...
                        ]
                    ]
