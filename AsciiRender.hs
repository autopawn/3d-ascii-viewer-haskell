{-
MIT License

Copyright (c) 2020 Francisco Javier Andrés Casas Barrientos

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

module AsciiRender (Vec3,Color(..),Triangle(..),render,mag,norm) where

import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (when)
import Control.Monad.ST
import Data.List (sort)

fI = fromIntegral

-- A 3D vector.
type Vec3 = (Float,Float,Float)

-- A char to paint a triangle with, or alternativelly paint using the normal vector
data Color = Col Char | Norm deriving (Show)

-- A triagle is composed of 3 points (x,y,z) and a Color
data Triangle = Triangle Vec3 Vec3 Vec3 Color deriving (Show)

-- Vector magnitude
mag :: Vec3 -> Float
mag (x,y,z) = sqrt (x*x + y*y + z*z)

-- Normalize vector
norm :: Vec3 -> Vec3
norm (x,y,z) = let
    m = mag (x,y,z)
    in (x/m,y/m,z/m)

-- Sort triangle Vec3s in x
sortbyx :: Triangle -> Triangle
sortbyx (Triangle p1 p2 p3 c) = let
    [p1',p2',p3'] = sort [p1,p2,p3]
    in Triangle p1' p2' p3' c

-- Cosine similarity
cosineSim :: Vec3 -> Vec3 -> Float
cosineSim v@(x,y,z) u@(a,b,c) = (x*a + y*b + z*c) / (mag u * mag v)

-- Ascii character from normal vector
normalToChar :: Vec3 -> Char
normalToChar v = let
    ops = ".,-~:;!*=#$@"
    p = round (fI (length ops - 1) * (cosineSim (-1,1,0) v *0.5 + 0.5))
    in ops !! p

-- Normal vector from a triangle
normal :: Triangle -> Vec3
normal (Triangle (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) col) = let
    (v1x,v1y,v1z) = (x2-x1 , y2-y1 , z2-z1)
    (v2x,v2y,v2z) = (x3-x1 , y3-y1 , z3-z1)
    in norm (v1y*v2z-v1z*v2y , v1z*v2x-v1x*v2z , v1x*v2y-v1y*v2x)

-- Triangle orientation
orientation :: Triangle -> Bool
orientation (Triangle (x1,y1,_) (x2,y2,_) (x3,y3,_) _) = (x2-x1)*(y3-y2) > (x3-x2)*(y2-y1)

-- List of (x_index,y_index,z) characters that should be drawn in the buffer,
-- for a triangle within the [0,1]X[0,1] on a sizex X sizey matrix
pixelsInside :: Int -> Int -> Triangle -> [((Int,Int,Float),Char)]
pixelsInside sizex sizey tri = let
    -- sortbyx triangle
    (Triangle (x1,y1,z1) (x2,y2,z2) (x3,y3,z3) col) = sortbyx tri
    -- float to index:
    dx = 1.0 / fI sizex
    dy = 1.0 / fI sizey
    toIx x = max 0 $ min (sizex-1) $ floor (x / dx) :: Int
    toIy y = max 0 $ min (sizey-1) $ floor (y / dy) :: Int
    -- triangle indexes limit for x
    xxi = toIx (x1 + dx/2)
    xxf = toIx (x3 - dx/2)
    -- triangle limits for a given float x:
    limy1 x
        | x <= x1   = y1
        | x >= x3   = y3
        | x <= x2   = y1 + (y2 - y1) * (x - x1) / (x2 - x1)
        | otherwise = y2 + (y3 - y2) * (x - x2) / (x3 - x2)
    limy2 x
        | x <= x1   = y1
        | x >= x3   = y3
        | otherwise = y1 + (y3 - y1) * (x - x1) / (x3 - x1)
    -- range for y indexes for a given x index
    limsyy xx = let
        x = (fI xx + 0.5) * dx
        ya = limy1 x
        yb = limy2 x
        yi = min ya yb
        yf = max ya yb
        yyi = toIy (yi + dy/2)
        yyf = toIy (yf - dy/2)
        in (yyi,yyf)
    -- normal vector
    nor@(nx,ny,nz) = normal tri
    -- depth of a given Vec3
    dp xx yy = let
        x = (fI xx + 0.5) * dx
        y = (fI yy + 0.5) * dy
        in z1 - (nx*(x-x1) + ny*(y-y1)) / nz
    -- Triangle color
    ch = case col of
        Norm  -> normalToChar nor
        Col c -> c
    --
    in [((xx,yy,dp xx yy), ch) | xx <- [xxi..xxf], let (yyi,yyf) = limsyy xx, yy <- [yyi..yyf]]

-- Renders the given triangles using an unboxed mutable array, retrieves it.
renderTriangles :: Int -> Int -> [Triangle] -> UArray (Int,Int) Char
renderTriangles sizex sizey tris = runSTUArray $ do
    -- The distance buffer
    zbuffer <- newArray ((0,0),(sizey-1,sizex-1)) (1.0/0.0) :: ST s (STUArray s (Int,Int) Float)
    -- The "pixels"
    cbuffer <- newArray ((0,0),(sizey-1,sizex-1)) ' '       :: ST s (STUArray s (Int,Int) Char)
    -- Get all pixel changes
    let pixs = concatMap (pixelsInside sizex sizey) tris
    -- Perform updates
    mapM_ (\((x,y,z),c) -> do
        -- Update a pixel on both buffers
        z0 <- readArray zbuffer (y,x)
        when (z<z0) $ do
            writeArray zbuffer (y,x) z
            writeArray cbuffer (y,x) c
        ) pixs
    -- Return result
    return cbuffer

-- Renders the given triangles using a Z-buffer, retrieves the resulting "image" as a multi-line string
render :: Int -> Int -> [Triangle] -> String
render sizex sizey tris = let
    arr = renderTriangles sizex sizey (filter orientation tris)
    lines = take sizey $ map (take sizex) $ iterate (drop sizex) (elems arr)
    in unlines lines
