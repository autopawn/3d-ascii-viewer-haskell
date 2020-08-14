module ReadOBJ (readOBJ) where

import System.IO

split :: (Eq a) => a -> [a] -> [[a]]
split d xs = split' d (reverse xs)

split' :: (Eq a) => a -> [a] -> [[a]]
split' d xs = let
    addchar (l:ls) x
        | x == d  = []:l:ls
        | x /= d  = (x:l):ls
    in foldl addchar [[]] xs

updatevf :: ([(Float,Float,Float)],[(Int,Int,Int)]) -> [String] -> ([(Float,Float,Float)],[(Int,Int,Int)])
updatevf (vs,fs) ("v":sx:sy:sz:_) = ((read sx,read sy,read sz):vs,fs)
updatevf (vs,fs) ("f":fc) = let
    getv s = read $ head $ split '/' s
    idx = map getv fc
    tris = zip3 (repeat (head idx)) (drop 1 idx) (drop 2 idx) -- NOTE: Triangularization may not be correct for complex faces.
    in (vs,tris ++ fs)
updatevf (vs,fs) _ = (vs,fs)

readOBJ :: FilePath -> IO ([(Float,Float,Float)],[(Int,Int,Int)])
readOBJ fname = do
    contents <- readFile fname
    let lins    = map words (lines contents)
    let (vs,fs) = foldl updatevf ([],[]) lins
    return (reverse vs,fs)
