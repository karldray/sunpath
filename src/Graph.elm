module Graph where

import Color
import Geometry exposing (Location)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import LocalTime
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Structs exposing (..)
import Style
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


type alias Uniforms =
    { localTimeMin: Float
    , localTimeMax: Float
    , startPos: Vec3
    , endPos: Vec3
    , startTime: Float
    , endTime: Float
    , startTimeOfYear: Float

    , midnightColor: Vec3
    , darkColor: Vec3
    , lightColor: Vec3
    , noonColor: Vec3
    , colorStopAngle: Float
    }

sunColorGL : Int -> Int -> FlightPath -> Time -> Time -> Element
sunColorGL w h fp tmin tmax =
    let vtx x y = {pos = vec2 x y}
        a = vtx -1  1
        b = vtx  1  1
        c = vtx  1 -1
        d = vtx -1 -1
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

        fragmentShader : WebGL.Shader {} Uniforms {vpos:Vec2}
        fragmentShader = [glsl|
            precision mediump float;

            uniform float localTimeMin;
            uniform float localTimeMax;
            uniform vec3 startPos;
            uniform vec3 endPos;
            uniform float startTime;
            uniform float endTime;
            uniform float startTimeOfYear;

            uniform vec3 midnightColor;
            uniform vec3 darkColor;
            uniform vec3 lightColor;
            uniform vec3 noonColor;
            uniform float colorStopAngle;

            varying vec2 vpos;

            const float PI = 3.1415926535897932384626433832795;

            vec3 altitude2color(float alt) {
                vec3 color = midnightColor;
                float stop = colorStopAngle;
                color = mix(color, darkColor, smoothstep(-0.5 * PI, -stop, alt));
                color = mix(color, lightColor, smoothstep(-stop, stop, alt));
                color = mix(color, noonColor, smoothstep(stop, PI * 0.5, alt));
                return color;
            }

            vec3 fromLatLong(float lat, float lon) {
                float r = cos(lat);
                return vec3(r * cos(lon), r * sin(lon), sin(lat));
            }

            // great circle path between two locations, as a function on [0, 1]
            vec3 greatCircle(vec3 a, vec3 b, float x) {
                if (a == b) return a;
                float ab = dot(a, b);
                vec3 c = normalize(b - ab * a);
                float m = x * acos(ab);
                return cos(m) * a + sin(m) * c;
            }

            // latitude at which the sun is directly overhead (annual cycle)
            float solarDeclination(float t) {
                return radians(23.44) * cos(2.0 * PI * (startTimeOfYear + t / 365.0));
            }

            // longitude at which the sun is directly overhead (daily cycle)
            float solarNoonLongitude(float t) {
                return 2.0 * PI * (0.5 - fract(t));
            }

            void main() {
                float left2right = 0.5 * (1.0 + vpos.x);
                float top2bottom = 0.5 * (1.0 - vpos.y);

                float actualTime = mix(startTime, endTime, top2bottom);
                float latitude = asin(greatCircle(startPos, endPos, top2bottom).z);
                float localTime = mix(localTimeMin, localTimeMax, left2right);

                vec3 relSunDirection = fromLatLong(solarDeclination(actualTime), solarNoonLongitude(localTime));
                float sunAltitude = radians(90.0) - acos(dot(fromLatLong(latitude, 0.0), relSunDirection));

                gl_FragColor = vec4(altitude2color(sunAltitude), 1.0);
            }
        |]

        oneDay = 24 * Time.hour
        offsetDays = toFloat (floor (tmin / oneDay))
        days t = t / oneDay - offsetDays

        toF x = (toFloat x) / 255.0
        c2v = Color.toRgb >> \{red, green, blue} -> vec3 (toF red) (toF green) (toF blue)

        uniforms =
            { localTimeMin = days tmin
            , localTimeMax = days tmax
            , startTime = days fp.start.time
            , endTime   = days fp.end.time
            , startPos = fp.start.airport.location
            , endPos   = fp.end.airport.location
            , startTimeOfYear = Geometry.timeOfYear fp.start.time

            , midnightColor = c2v Style.midnightColor
            , darkColor = c2v Style.darkColor
            , lightColor = c2v Style.lightColor
            , noonColor = c2v Style.noonColor
            , colorStopAngle = Style.colorStopAngle
            }

    in  WebGL.webgl (w, h) [WebGL.entity vertexShader fragmentShader square uniforms]

