a    :: IO(Char,Char)
a    = do x <- getChar
          getChar
          y <- getChar
          return (x,y)