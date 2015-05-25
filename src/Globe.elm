module Globe where

import Graphics.Element exposing (Element)
import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Matrix4 as M4 exposing (Mat4)
import Util
import WebGL exposing (Texture, Triangle, entity, webgl)


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

doubleSided : List (Triangle t) -> List (Triangle t)
doubleSided xs =
    xs ++ List.map (\(a, b, c) -> (c, b, a)) xs


type alias Uniforms =
    { sunDirection: Vec3
    , view: Mat4
    , tex: Texture
    --todo can we get rid of these?
    , midnightColor: Vec3
    , darkColor: Vec3
    , lightColor: Vec3
    , noonColor: Vec3
    , colorStopAngle: Float
    }

globe : Int -> Int -> Texture -> Element
globe w h tex =
    let 
        aspectRatio = (toFloat w / toFloat h)
        -- perspective = M4.makePerspective 45 aspectRatio 0.01 100
        perspective = M4.makeOrtho -aspectRatio aspectRatio -1 1 0.01 100
        camera = M4.makeLookAt (vec3 2 2 2) (vec3 0 0 0) V3.k
        view = M4.mul perspective camera

        vertexShader : WebGL.Shader {pos:Vec3} {u|view:Mat4} {vpos:Vec3}
        vertexShader = [glsl|
            precision mediump float;

            attribute vec3 pos;
            uniform mat4 view;
            varying vec3 vpos;
            
            void main() {
                vpos = normalize(pos);
                gl_Position = view * vec4(vpos, 1.0);
            }
        |]

        fragmentShader : WebGL.Shader {} Uniforms {vpos:Vec3}
        fragmentShader = [glsl|
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

        uniforms = Util.addColorUniforms
            { view = view
            , tex = tex
            , sunDirection = vec3 1 0 0
            }
            
        data = List.map (WebGL.map (\v -> {pos = v})) (tessellate 4 dodecahedron)
    in  webgl (w, h) [entity vertexShader fragmentShader data uniforms]

