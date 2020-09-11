filter :: (a -> Bool) -> [a] -> [a]
filter men xs
men ys = [xs | xs <- ys,ys == length=<3]