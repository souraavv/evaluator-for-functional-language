module HashMap where

data HashMap k v = Mp [(k, v)] deriving (Show, Eq)

isPresent :: Eq t => HashMap t v -> t -> Bool
isPresent (Mp []) k = False
isPresent (Mp ((key, val) : remMp)) k = (k == key) || isPresent (Mp remMp) k

giveVal (Mp ((key, val) : remMp)) k
  | k == key = val
  | otherwise = giveVal (Mp remMp) k

updateVal (Mp ((key, val) : remMp)) k v
  | key == k = Mp ((key, v) : remMp)
  | otherwise =
    let Mp newMp = updateVal (Mp remMp) k v
     in Mp ((key, val) : newMp)


addVal (Mp []) k v = Mp [(k, v)]
addVal (Mp l) k v
  | isPresent (Mp l) k = updateVal (Mp l) k v
  | otherwise = Mp ((k, v) : l)

deleteKey (Mp []) k = Mp []
deleteKey (Mp ((key, val) : remMp)) k
  | key == k = Mp remMp
  | otherwise =
    let Mp newMp = deleteKey (Mp remMp) k
     in Mp ((key, val) : newMp)
