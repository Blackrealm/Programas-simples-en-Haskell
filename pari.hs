paridad xs | odd (sum xs)  = 1
           | otherwise     = 0

agregaParidad xs |odd (lenght xs) = (paridad xs) : xs
                 |otherwise = (paridad xs) : xs