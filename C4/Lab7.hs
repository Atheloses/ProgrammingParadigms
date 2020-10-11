type Pic = [String]

pp :: Pic -> IO ()
pp x = putStr (concat (map (++"\n") x))

pic :: Pic
pic = [ "....#....",
        "...###...",
        "..#.#.#..",
        ".#..#..#.",
        "....#....",
        "....#....",
        "....#####"]

flipV :: Pic -> Pic
flipV = map reverse 


flipH :: Pic -> Pic
flipH = reverse 


zoom :: Int -> Pic -> Pic
zoom n xs = [concat(map (replicate n) x)|x<-concat (map (replicate n) xs)]

sideBySide :: Pic -> Pic -> Pic
sideBySide xs ys = map (\(x,y) -> x ++ y)(zip xs ys) 
