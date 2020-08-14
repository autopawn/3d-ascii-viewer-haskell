import AsciiRender
import ReadOBJ

import System.Environment

import Data.Time.Clock.System

import Data.Array (Array,(!))
import Data.Array.IArray (listArray)

import Control.Concurrent (threadDelay)

data Transform = Transform (Float,Float) (Float,Float) Float
-- (cosθ,sinθ) (cosϕ,sinϕ) Zoom

-- Rotate vector on X axis, expects cosθ and sinθ
rotateX :: (Float,Float) -> Vec3 -> Vec3
rotateX (cosθ,sinθ) (x,y,z) = let
    y2 = y*cosθ - z*sinθ
    z2 = y*sinθ + z*cosθ
    in (x,y2,z2)

-- Rotate vector on Y axis, expects cosθ and sinθ
rotateY :: (Float,Float) -> Vec3 -> Vec3
rotateY (cosθ,sinθ) (x,y,z) = let
    x2 = x*cosθ - z*sinθ
    z2 = x*sinθ + z*cosθ
    in (x2,y,z2)

-- Rotate vector on Z axis, expects cosθ and sinθ
rotateZ :: (Float,Float) -> Vec3 -> Vec3
rotateZ (cosθ,sinθ) (x,y,z) = let
    x2 = x*cosθ - y*sinθ
    y2 = x*sinθ + y*cosθ
    in (x2,y2,z)

center :: [Vec3] -> [Vec3]
center vs = let
    xs = map (\(x,_,_) -> x) vs
    ys = map (\(_,y,_) -> y) vs
    zs = map (\(_,_,z) -> z) vs
    midx = (maximum xs + minimum xs)/2
    midy = (maximum ys + minimum ys)/2
    midz = (maximum zs + minimum zs)/2
    in map (\(x,y,z) -> (x-midx,y-midy,z-midz)) vs

triangles :: [Vec3] -> [(Int,Int,Int)] -> [Triangle]
triangles pts tris = let
    npts = length pts
    ptsa = listArray (1,npts) pts :: Array Int Vec3
    triang (a,b,c) = Triangle (ptsa ! a) (ptsa ! b) (ptsa ! c) Norm
    in map triang tris

applyTransform :: Transform -> Vec3 -> Vec3
applyTransform (Transform (cosθ,sinθ) (cosϕ,sinϕ) a) vec = let
    (x2,y2,z2) = rotateX (cosϕ,sinϕ) (rotateY (cosθ,sinθ) vec)
    xf = (x2 * a)/2
    yf = (y2 * a)/2
    zf = (z2 * a)/2
    in (0.5+xf,0.5-yf,zf)

getSystemNanosecs :: IO Integer
getSystemNanosecs = do
    st <- getSystemTime
    let nano1 = 1000000000 * toInteger (systemSeconds st)
    let nano2 = toInteger (systemNanoseconds st)
    return $ nano1+nano2

drawModel :: Float -> [Vec3] -> [(Int,Int,Int)] -> Float -> IO ()
drawModel zoom vs fs t = do
    tstart <- getSystemNanosecs
    let θ = t/10
    let (cosθ,sinθ) = (cos θ, sin θ)
    let ϕ = -0.8+0.6*cos(t/30)
    let (cosϕ,sinϕ) = (cos ϕ, sin ϕ)
    let trf  = Transform (cosθ,sinθ) (cosϕ,sinϕ) zoom
    let vs2  = map (applyTransform trf) vs
    let tris = triangles vs2 fs
    putStrLn "\x1b[H"
    putStrLn (render 80 40 tris)
    tcurrent <- getSystemNanosecs
    let diffnanosecs = max 0 (tcurrent - tstart)
    threadDelay (41666 - fromIntegral (diffnanosecs `div` 1000))

main = do
    (fpath:_) <- getArgs
    (vs,fs) <- readOBJ fpath
    let vs2 = center vs
    let zoom = 1/(0.001 + maximum (map mag vs2))
    mapM_ (drawModel zoom vs2 fs) [0..]
