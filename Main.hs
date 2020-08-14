import AsciiRender
import ReadOBJ

import System.Environment

import Data.Array (Array,(!))
import Data.Array.IArray (listArray)

import Control.Concurrent (threadDelay)

data Transform = Transform Float Float Float
-- rotZ rotX Zoom

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
applyTransform (Transform θ ϕ a) vec = let
    (x2,y2,z2) = rotateX (-ϕ) (rotateY θ vec)
    xf = (x2 * a)/2
    yf = (y2 * a)/2
    zf = (z2 * a)/2
    in (0.5+xf,0.5-yf,zf)

drawModel :: Float -> [Vec3] -> [(Int,Int,Int)] -> Float -> IO ()
drawModel zoom vs fs t = do
    let trf  = Transform (t/5) (0.8+0.6*cos(t/15)) zoom
    let vs2  = map (applyTransform trf) vs
    let tris = triangles vs2 fs
    putStrLn "\x1b[H"
    putStrLn (render 80 40 tris)
    threadDelay 80000

main = do
    (fpath:_) <- getArgs
    (vs,fs) <- readOBJ fpath
    let vs2 = center vs
    let zoom = 1/(0.001 + maximum (map mag vs2))
    mapM_ (drawModel zoom vs2 fs) [0..]
