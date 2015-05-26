module MapBackground where

import Graphics.Element exposing (Element)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Util
import WebGL exposing (Shader, Texture, entity, webgl)


type alias Uniforms = Util.WithColorUniforms
    { sunDirection: Vec3
    , tex: Texture
    }

type alias LatLong = {longitude: Float, latitude:Float}

vertexShader : Shader {pos: Vec2} u LatLong
vertexShader = [glsl|
    precision mediump float;

    attribute vec2 pos;

    varying float longitude;
    varying float latitude;

    const float PI = 3.1415926535897932384626433832795;

    void main() {
        longitude = PI * pos.x;
        latitude = 0.5 * PI * pos.y;
        gl_Position = vec4(pos, 0.0, 1.0);
    }
|]

fragmentShader : Shader {} Uniforms LatLong
fragmentShader = [glsl|
    precision mediump float;

    uniform vec3 midnightColor;
    uniform vec3 darkColor;
    uniform vec3 lightColor;
    uniform vec3 noonColor;
    uniform float colorStopAngle;

    uniform vec3 sunDirection;
    uniform sampler2D tex;

    varying float longitude;
    varying float latitude;

    const float PI = 3.1415926535897932384626433832795;

    // todo: figure out a way to avoid copying logic among shaders
    vec3 altitude2color(float alt) {
        vec3 color = midnightColor;
        float stop = colorStopAngle;
        color = mix(color, darkColor, smoothstep(-0.5 * PI, -stop, alt));
        color = mix(color, lightColor, smoothstep(-stop, stop, alt));
        color = mix(color, noonColor, smoothstep(stop, PI * 0.5, alt));
        return color;
    }

    void main() {
        float r = cos(latitude);
        vec3 vpos = vec3(r * cos(longitude), r * sin(longitude), sin(latitude));

        float mapColor = texture2D(tex, vec2(
            0.5 + longitude / (2.0 * PI),
            0.5 + latitude / PI
        )).r;

        float alt = radians(90.0) - acos(dot(sunDirection, vpos));
        vec3 sunColor = altitude2color(alt);

        vec3 ret = mix(vec3(0.0), sunColor, 0.8 + 0.2 * mapColor);

        gl_FragColor = vec4(ret, 1.0);
    }
|]

mapBackground: Int -> Texture -> Vec3 -> Element
mapBackground h tex sunDirection =
    let uniforms = Util.addColorUniforms {tex = tex, sunDirection = sunDirection}
    in  webgl (2 * h, h) [entity vertexShader fragmentShader Util.squareData uniforms]
