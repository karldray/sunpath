module SunColorGL where
-- generates the background image for the "local solar time" graph

import Geometry
import Graphics.Element exposing (Element)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Structs exposing (..)
import Time exposing (Time)
import Util
import WebGL exposing (Shader, Triangle, webgl)


type alias Uniforms = Util.WithColorUniforms
    { localTimeMin: Float
    , localTimeMax: Float
    , startPos: Vec3
    , endPos: Vec3
    , startTime: Float
    , endTime: Float
    , startTimeOfYear: Float
    }

vertexShader : Shader {pos:Vec2} u {vpos:Vec2}
vertexShader = [glsl|
    precision mediump float;
    attribute vec2 pos;
    varying vec2 vpos;
    void main() {
        gl_Position = vec4(pos, 0.0, 1.0);
        vpos = pos;
    }
|]

fragmentShader : Shader {} Uniforms {vpos:Vec2}
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


sunColorGL : Int -> Int -> FlightPath -> Time -> Time -> Element
sunColorGL w h fp tmin tmax =
    let oneDay = 24 * Time.hour
        offsetDays = toFloat (floor (tmin / oneDay))
        days t = t / oneDay - offsetDays

        uniforms = Util.addColorUniforms
            { localTimeMin = days tmin
            , localTimeMax = days tmax
            , startTime = days fp.start.time
            , endTime   = days fp.end.time
            , startPos = fp.start.airport.location
            , endPos   = fp.end.airport.location
            , startTimeOfYear = Geometry.timeOfYear fp.start.time
            }

    in  webgl (w, h) [WebGL.entity vertexShader fragmentShader Util.squareData uniforms]

