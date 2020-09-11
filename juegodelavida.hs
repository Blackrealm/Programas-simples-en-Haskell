{-# LANGUAGE BlockArguments #-}

width :: Int
width = 5
height :: Int
height = 5
type Pos = (Int , Int )

type Board = [Pos]
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

seqn :: [IO a ] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
            seqn as
goto :: Pos -> IO ()
goto (x , y) =
   putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
             putStr xs
showcells :: Pos -> Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b 
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)
neighbs :: Pos -> [Pos]   --28
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                   (x+1,y-1),(x-1,y),
                   (x+1,y),(x-1,y+1),
                   (x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x , y) = (((x - 1) `mod` width) + 1,
           ((y - 1) `mod` height ) + 1)
liveneighbs :: Board -> Pos ->Int
liveneighbs b = length . filter (isAlive b) . neighbs --38
survivors :: Board -> [Pos ]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]
births :: Board -> [Pos]

--births b = [(x,y) | x <- [1..width ],
--                    y <- [1--height ],
--                    isEmpty b(x , y),
--                    liveneighbs b(x , y) == 3]

births b = [p | p <- rmdups (concat (map neighbs b)),
                  isEmpty b p,
                  liveneighbs b p == 3]
rmdups :: Eq a => [a ] -> [a ]
rmdups [ ] = [ ]
rmdups (x : xs) = x : rmdups (filter ( /= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]] --41

cls :: IO ()
cls = putStr "\n\n\n"
life :: Board -> IO ()
life b = do cls
         showcells b
         wait 5000
         life (nextgen b)