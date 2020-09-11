vec      :: Int->([Int]->[Int])->
vec y [] = []
vec y xs = [(y *  head xs)] concat vec