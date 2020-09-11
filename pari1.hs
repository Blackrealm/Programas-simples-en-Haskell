paridad :: [Int] -> Int
agregaParidad :: [Int]->[Int]

paridad xs | odd (sum xs)  = 1
           | otherwise     = 0

agregaParidad bs = (paridad bs) : bs