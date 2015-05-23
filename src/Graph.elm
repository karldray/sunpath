module Graph where

import Color
import Geometry exposing (Location)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import LocalTime
import Math.Vector2 exposing (Vec2, vec2)
import Structs exposing (..)
import Time exposing (Time)
import WebGL


interpolate : FlightPath -> Float -> (Time, Location)
interpolate fp x = (
        (1 - x) * fp.start.time + x * fp.end.time,
        Geometry.greatCircle fp.start.airport.location fp.end.airport.location x
    )

sunAltitude : Int -> Int -> FlightPath -> Element
sunAltitude w h fp =
    let npoints = w // 2
        xvals = List.map (\x -> (toFloat x) / (toFloat npoints)) [0..npoints]

        yscale = (toFloat h / 2) / degrees 90
        f = interpolate fp >> \(t, pos) -> Geometry.sunAltitude pos t

        points = List.map (\x -> ((toFloat w) * (x - 0.5), yscale * (f x))) xvals
    
    in  collage w h [
        rect (toFloat w) (toFloat h)
            |> gradient (Color.linear (0, -20) (0, 20) [(0, Color.lightBlue), (1, Color.yellow)]),
        traced (solid Color.black) (path points)
    ]

apparentTime : Int -> Int -> FlightPath -> Element
apparentTime w h fp =
    let npoints = h // 2
        xvals = List.map (\x -> (toFloat x) / (toFloat npoints)) [0..npoints]
        
        -- todo: better window size scaling
        tmin = f 0 - 2 * Time.hour
        tmax = f 1 + 2 * Time.hour
        tmid = tmin + (tmax - tmin) / 2
        
        xscale = toFloat h
        tscale = toFloat w / (tmax - tmin)
        
        f x =
            let (t, pos) = interpolate fp x
                (lat, long) = Geometry.toLatLong pos
            in  LocalTime.toTime LocalTime.utc (Geometry.apparentTime long t)
        
        points = List.map (\x -> (tscale * ((f x) - tmid), xscale * (0.5 - x))) xvals
    
    in  flow outward [
        sunColorGL w h tmin tmax,
        collage w h [
            traced (solid Color.black) (path points)
        ]
    ]


type alias Vertex = {pos: Vec2}

sunColorGL : Int -> Int -> Time -> Time -> Element
sunColorGL w h tmin tmax =
    let vec x y = Vertex (vec2 x y)
        a = vec -1  1
        b = vec  1  1
        c = vec  1 -1
        d = vec -1 -1
        square : List (WebGL.Triangle Vertex)
        square = [(a, b, c), (c, d, a)]

        vertexShader : WebGL.Shader {pos:Vec2} u {vpos:Vec2}
        vertexShader = [glsl|
            precision mediump float;
            attribute vec2 pos;
            varying vec2 vpos;
            void main() {
                gl_Position = vec4(pos, 0.0, 1.0);
                vpos = pos;
            }
        |]

        fragmentShader : WebGL.Shader {} {tmin:Float, tmax:Float} {vpos:Vec2}
        fragmentShader = [glsl|
            precision mediump float;
            uniform float tmin;
            uniform float tmax;
            varying vec2 vpos;
            void main () {
                float t = mix(tmin, tmax, 0.5 * (1.0 + vpos.x));
                float a = 0.5 * (1.0 - cos(t));
                vec3 night = vec3(0.6, 0.7, 0.8);
                vec3 day = vec3(1.0, 1.0, 0.7);
                gl_FragColor = vec4(mix(night, day, a), 1.0);
            }
        |]

        days t = t / (86400 * Time.second)
        startTod = Geometry.mod1 (days tmin)
        delta = days (tmax - tmin)
        uniforms = {
            tmin = turns startTod,
            tmax = turns (startTod + delta)
            }

    in  WebGL.webgl (w, h) [WebGL.entity vertexShader fragmentShader square uniforms]

