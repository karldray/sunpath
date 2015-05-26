module Globe where

import Geometry
import Graph.Util exposing (interpolate)
import Graphics.Element exposing (Element)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Matrix4 as M4 exposing (Mat4)
import Structs exposing (..)
import Util
import WebGL exposing (Shader, Texture, Triangle, entity, webgl)


dodecahedron : List (Triangle Vec3)
dodecahedron =
    let theta = 0.4 * pi
        z = 1 / sqrt 5
        r = 2 * z
        makev i = vec3 (r * cos (i * theta)) (r * sin (i * theta)) z
        [a, b, c, d, e] = List.map makev [0..4]
        t = vec3 0 0 1
        n = V3.negate
        top = [
            (a,t,b), (b,t,c), (c,t,d), (d,t,e), (e,t,a),
            (a,b,n d), (b,c,n e), (c,d,n a), (d,e,n b), (e,a,n c)
        ]
        bottom = List.map (WebGL.map n) top
    in top ++ bottom

tessellateOnce : List (Triangle Vec3) -> List (Triangle Vec3)
tessellateOnce =
    let split (a, b, c) =
            let m x y = V3.scale 0.5 (V3.add x y)
                ab = m a b
                bc = m b c
                ca = m c a
            in [(a,ab,ca), (b,bc,ab), (c,ca,bc), (ab,bc,ca)]
    in  List.concatMap split

tessellate : Int -> List (Triangle Vec3) -> List (Triangle Vec3)
tessellate n xs =
    if | n <= 0 -> xs
       | otherwise -> tessellate (n - 1) (tessellateOnce xs)

{-
doubleSided : List (Triangle t) -> List (Triangle t)
doubleSided xs =
    xs ++ List.map (\(a, b, c) -> (c, b, a)) xs
-}

wrap : List (Triangle Vec3) -> List (Triangle {pos: Vec3})
wrap = List.map (WebGL.map (\v -> {pos = v}))

sphereData : List (Triangle {pos: Vec3})
sphereData = wrap (tessellate 3 dodecahedron)


type alias Uniforms = Util.WithColorUniforms
    { sunDirection: Vec3
    , view: Mat4
    , tex: Texture
    }

sphereVertexShader : Shader {pos:Vec3} {u|view:Mat4} {vpos:Vec3}
sphereVertexShader = [glsl|
    precision mediump float;

    attribute vec3 pos;
    uniform mat4 view;
    varying vec3 vpos;

    void main() {
        vpos = normalize(pos);
        gl_Position = view * vec4(vpos, 1.0);
    }
|]

sphereFragmentShader : Shader {} Uniforms {vpos:Vec3}
sphereFragmentShader = [glsl|
    precision mediump float;

    uniform vec3 midnightColor;
    uniform vec3 darkColor;
    uniform vec3 lightColor;
    uniform vec3 noonColor;
    uniform float colorStopAngle;

    uniform vec3 sunDirection;
    uniform sampler2D tex;
    varying vec3 vpos;

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
        float latitude = asin(vpos.z);
        float longitude = atan(vpos.y, vpos.x);

        float mapColor = texture2D(tex, vec2(
            0.5 + longitude / (2.0 * PI),
            0.5 + latitude / PI
        )).r;

        float alt = radians(90.0) - acos(dot(sunDirection, vpos));
        vec3 sunColor = altitude2color(alt);

        vec3 ret = mix(vec3(0.0), sunColor, 1.0 - mapColor);

        gl_FragColor = vec4(ret, 1.0);
    }
|]

simpleVertexShader : Shader {pos: Vec3} {view: Mat4} {}
simpleVertexShader = [glsl|
    precision mediump float;
    attribute vec3 pos;
    uniform mat4 view;
    void main() {
        gl_Position = view * vec4(pos, 1.0);
    }
|]

blackFragmentShader : Shader {} u {}
blackFragmentShader = [glsl|
    precision mediump float;
    void main() {
        gl_FragColor = vec4(vec3(0.0), 1.0);
    }
|]

planeData : List (Triangle {pos: Vec3})
planeData = wrap [((vec3 0 1 1), (vec3 1 -1 1), (vec3 -1 -1 1))]


globe : Int -> Int -> Texture -> FlightPath -> Float -> Element
globe w h tex path =
    let 
        aspectRatio = toFloat w / toFloat h
        perspective = M4.makeOrtho -aspectRatio aspectRatio -1 1 0.01 100
        -- perspective = M4.makePerspective 45 aspectRatio 0.01 100

    in  \x ->
        let (t, pos) = interpolate path x

            (lat, long) = Geometry.toLatLong pos
            camera = V3.scale 10 (Geometry.fromLatLong (0.5 * lat) long)
            cameraMatrix = M4.makeLookAt camera (vec3 0 0 0) V3.k
            view = M4.mul perspective cameraMatrix

            sphereUniforms = Util.addColorUniforms
                { view = view
                , tex = tex
                , sunDirection = Geometry.sunDirection t
                }

            end = path.end.airport.location
            dir = V3.normalize <| V3.sub end <| V3.scale (V3.dot pos end) pos
            right = V3.negate <| V3.cross pos dir

            planeTransform = List.foldr M4.mul M4.identity
                [ view
                , M4.makeBasis right dir pos
                , M4.makeTranslate3 0 0 1
                , M4.makeScale3 0.03 0.03 0.03
                ]

        in  webgl (w, h)
            [ entity sphereVertexShader sphereFragmentShader sphereData sphereUniforms
            , entity simpleVertexShader blackFragmentShader planeData {view = planeTransform}
            ]
