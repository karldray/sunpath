module Util where

import Color exposing (Color)
import Debug
import Math.Vector3 exposing (Vec3, vec3)
import Style
import Task exposing (Task)


logError : String -> Task x a -> Task x a
logError s t = Task.onError t (Debug.log s >> Task.fail)


colorToVec3 : Color -> Vec3
colorToVec3 =
    let f x = (toFloat x) / 255.0
    in  Color.toRgb >> \c -> vec3 (f c.red) (f c.green) (f c.blue)

addColorUniforms : a -> { a
    | midnightColor: Vec3
    , darkColor: Vec3
    , lightColor: Vec3
    , noonColor: Vec3
    , colorStopAngle: Float
    }
addColorUniforms r =
    let c2v = colorToVec3
        -- is there really no multi-field syntax for this?
        r1 = {r  | midnightColor = c2v Style.midnightColor}
        r2 = {r1 | darkColor = c2v Style.darkColor}
        r3 = {r2 | lightColor = c2v Style.lightColor}
        r4 = {r3 | noonColor = c2v Style.noonColor}
        r5 = {r4 | colorStopAngle = Style.colorStopAngle}
    in  r5
