-- | Assignment 1: implementing various small functions
module A01
  ( Day(..)
  , addTuple
  , productDot
  , maybeMap
  , maybeThen
  , Tree(..)
  , sumTree
  , rightRotateTree
  , listSum
  , productSeq
  , setMem
  , setEquiv
  , setUnion
  , setIntersection
  , setDiff
  , setSymDiff
  , relMem
  , relEquiv
  , relComp
  , relTrans
  , relFull
  , fibs
  , primes
  , fuzzySeq
  , funComp
  , curry2
  , uncurry2
  , myFilter
  , myFilterMap
  , myFoldL
  , myRev
  ) where

-- | TODO marker.
todo :: t
todo = error "todo"

 

-- | Days of week.
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show)

-- | Returns the next weekday (excluding weekend, namely Saturday and Sunday).
nextWeekday :: Day -> Day
nextWeekday day
  | day == Sunday = Monday
  | day == Monday = Tuesday
  | day == Tuesday = Wednesday
  | day == Wednesday = Thursday
  | day == Thursday = Friday
  | day == Friday = Monday
  | day == Saturday = Monday

-- | Add tuples of the 2-dimensional plane.
addTuple :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
addTuple t1 t2 = let { (x1, y1) = t1; (x2, y2) = t2}
                  in (x1 + x2, y1 + y2)

-- | Dot-products two integer (list) vectors: https://en.wikipedia.org/wiki/Dot_product
-- |
-- | If the two vectors have different number of elements, you can return anything. 
productDot :: [Integer] -> [Integer] -> Integer
productDot t1 t2
  | length t1 /= length t2 = 0
  | null t1 && null t2 = 0
  | otherwise = (head t1) * (head t2) + productDot (tail t1) (tail t2)

-- | Maps the given value if it's Just.
maybeMap :: (Integer -> Integer) -> Maybe Integer -> Maybe Integer
maybeMap f value = case value of
  Just a -> Just (f a)
  _ -> Nothing

-- | If the given value is Just, map it with the given function; otherwise, the result is Nothing.
maybeThen :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
maybeThen value cont = case value of
  Just a -> cont a 
  _ -> Nothing

-- | Trees of integers.
data Tree = Leaf Integer | Branch Integer Tree Tree deriving (Eq, Show) -- Integer is value, Trees are left/right subtrees.

-- | Sums all the integers in the given tree.
sumTree :: Tree -> Integer
sumTree tree = case tree of
  Leaf x -> x
  Branch x t1 t2 -> x + (sumTree t1) + (sumTree t2)

-- | Right-rotate the given tree. See https://en.wikipedia.org/wiki/Tree_rotation for more detail.
-- |
-- | Returns Nothing if there are not enough nodes.
rightRotateTree :: Tree -> Maybe Tree
rightRotateTree tree = case tree of 
    Leaf a -> Nothing
    Branch a t1 t2 -> case t1 of 
        Leaf a1 -> Nothing
        Branch a1 t11 t12 -> Just (Branch a1 t11 (Branch a t12 t2))

-- | Maps the given list.
listMap = map

-- | Sums all the integers in the given list.
listSum :: [Integer] -> Integer
listSum l = foldl step 0 l 
  where step acc x = acc + x

-- | More compositional construction of sigma.
sumSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
sumSeq f from to = listSum (listMap f [from .. to])

-- | product of a sequence. See https://en.wikipedia.org/wiki/Multiplication#Product_of_a_sequence for more detail.
productSeq :: (Integer -> Integer) -> Integer -> Integer -> Integer
productSeq f from to = foldl step 1 (map f [from..to])
  where step acc x = acc * x

-- | Returns if the given value is in the (list) set.
setMem :: Integer -> [Integer] -> Bool
setMem value set
  | null set = False
  | otherwise = (head set == value) || setMem value (tail set)

filterSet (x:xs) acc 
                    | setMem x acc = filterSet xs acc
                    | otherwise = filterSet xs (x:acc)
filterSet [] acc = acc

-- | Returns the two sets contain the same elements.
setEquiv :: [Integer] -> [Integer] -> Bool
setEquiv s1 s2 = let {ns1 = filterSet s1 []; ns2 = filterSet s2 [];}
                in (length ns1 == length ns2) && processSetEquiv ns1 ns2 
                  where processSetEquiv (x:xs) l2
                                              | not(setMem x s2) = False
                                              | otherwise = processSetEquiv xs l2
                        processSetEquiv [] l2 = True

-- | Returns the set union.
setUnion :: [Integer] -> [Integer] -> [Integer]
setUnion s1 s2 = filterSet (nload s1 s2) []
    where   nload (x:xs) ls2 = x : nload xs ls2
            nload _ (x:xs) 
                | setMem x s1 = nload [] xs
                | otherwise = x : nload [] xs
            nload _ _ = []


-- | Returns the set intersection
setIntersection :: [Integer] -> [Integer] -> [Integer]
setIntersection s1 s2
    | null s1 = []
    | setMem (head s1) s2 = (head s1) : setIntersection (tail s1) s2
    | otherwise = setIntersection (tail s1) s2

-- | Returns the set diff, i.e., setDiff a b = $a - b$.
setDiff :: [Integer] -> [Integer] -> [Integer]
setDiff s1 s2
    | null s1 = []
    | setMem (head s1) s2 = setDiff (tail s1) s2
    | otherwise = (head s1) : setDiff (tail s1) s2

-- | Returns the set symmetric diff.
setSymDiff :: [Integer] -> [Integer] -> [Integer]
setSymDiff s1 s2 = (setDiff s1 s2) ++ (setDiff s2 s1) 

-- | Returns if the given pair is in the (list) relation.
relMem :: [(Integer, Integer)] -> Integer -> Integer -> Bool
relMem (x:xs) v1 v2
    | x == (v1, v2) = True
    | otherwise = relMem xs v1 v2
relMem _ v1 v2 = False

-- | Returns the two relations contain the same elements.
setMemP :: (Integer, Integer) -> [(Integer, Integer)] -> Bool
setMemP value set
  | null set = False
  | otherwise = (head set == value) || setMemP value (tail set)
relEquiv :: [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
relEquiv r1 r2 
  | null r1 = False
  | setMemP (head r1) r2 = True
  | otherwise = relEquiv (tail r1) r2

-- | Composes two relations, i.e., {(a,c) | exists b, (a,b) in r1 and (b,c) in r2}.

filterSetP (x:xs) acc 
                    | setMemP x acc = filterSetP xs acc
                    | otherwise = filterSetP xs (x:acc)
filterSetP [] acc = acc


relMatch ((x1, x2):xs) v1 v2
    | v2 == x1 = (v1, x2):(relMatch xs v1 v2)
    | otherwise = relMatch xs v1 v2 
relMatch [] v1 v2 = []

relComp :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
relComp r1 r2 = (processRelComp r1 r2)
  where   processRelComp ((x1, x2):xs) nr2 = (relMatch nr2 x1 x2) ++ (processRelComp xs nr2)
          processRelComp [] nr2 = []

-- | Returns the transitive closure of the given relation: https://en.wikipedia.org/wiki/Transitive_closure
relTrans :: [(Integer, Integer)] -> [(Integer, Integer)]
relTrans rel = let  processElem ((x1, x2):ls) acc1 
                        | setMem x1 acc1 && setMem x2 acc1 = processElem ls acc1
                        | setMem x1 acc1 && not (setMem x2 acc1) = processElem ls (x2:acc1)
                        | setMem x2 acc1 && not (setMem x1 acc1) = processElem ls (x1:acc1)
                        | otherwise = processElem ls (x1:x2:acc1)
                    processElem [] acc1 = acc1

                    findAdj ((x1, x2):ls) u inU outU
                        | x1 == x2 && x1 == u = findAdj ls u inU outU
                        | x1 == u = findAdj ls u inU (x2:outU)
                        | x2 == u = findAdj ls u (x1:inU) outU
                        | otherwise = findAdj ls u inU outU
                    findAdj [] u inU outU = (inU, outU)

                    crossMatch (x:l1) l2 = foldl (\acc y -> (x, y):acc) [] l2 ++ crossMatch l1 l2
                    crossMatch [] l2 = []

                    mErge (x:xs) acc 
                        | setMemP x acc = mErge xs acc
                        | otherwise = mErge xs (x:acc)
                    mErge [] acc = acc
                    
                    elemList = processElem rel []

                    solve (u:ls) acc = let  (inU, outU) = findAdj acc u [] []
                                            nAcc = mErge (crossMatch inU outU) acc
                                        in  solve ls nAcc
                    solve [] acc = acc
                in solve elemList rel

-- | Returns the relation [0..n] * [0..n] = {(0,0), (0,1), ..., (0,n), (1,0), (1,1), ..., (1,n), ..., (n,n)}.
relFull :: Integer -> [(Integer, Integer)]
relFull n = crossRel 0 0 
  where crossRel x y 
                    | y == (n + 1) = crossRel (x + 1) 0
                    | x == (n + 1) = []
                    | otherwise = (x, y):(crossRel x (y+1))


-- | The Fibonacci sequence, starting with 0, 1, 1, 2, 3, ...
fibs :: [Integer]
fibs = loadFibs 0 1 1
  where loadFibs a b n
                      | n == 1 = a:(loadFibs a b (n+1))
                      | n == 2 = b:(loadFibs a b (n+1))
                      | otherwise = (a + b):(loadFibs b (a+b) (n+1))

-- | The primes, starting with 2, 3, 5, 7, ...
primes :: [Integer]
primes = loadPrimes 2 []
  where loadPrimes n acc
                        | checkPrime acc n = n:(loadPrimes (n+1) (n:acc))
                        | otherwise = loadPrimes (n+1) (n:acc)
                          where   checkPrime (x:xs) value 
                                                          | value `rem` x == 0 = False
                                                          | otherwise = checkPrime xs value
                                  checkPrime [] value = True

-- | The sequence of 1, 2, 1, 3, 2, 1, 4, 3, 2, 1, 5, 4, 3, 2, 1, ...
fuzzySeq :: [Integer]
fuzzySeq = loadFuzzySeq 1 1
  where loadFuzzySeq lst n 
                          | lst == 0 = (n+1):(loadFuzzySeq n (n+1))
                          | otherwise = lst:(loadFuzzySeq (lst-1) n)

-- | Composes two functions, i.e., applies f1 and then f2 to the given argument
funComp :: (Integer -> Integer) -> (Integer -> Integer) -> (Integer -> Integer)
funComp f1 f2 = f2 . f1

-- | Transforms a function that gets single pair into a function that gets two arguments, i.e., curry2 f a1 a2 = f (a1, a2)
curry2 :: ((Integer, Integer) -> Integer) -> Integer -> Integer -> Integer
curry2 f a1 a2 = f (a1, a2)

-- | Transforms a function that gets two arguments into a function that gets single pair, i.e., uncurry2 f (a1, a2) = f a1 a2
uncurry2 :: (Integer -> Integer -> Integer) -> (Integer, Integer) -> Integer
uncurry2 f (a1, a2) = f a1 a2

-- | Filters the given list so that the the filter function returns True for the remaining elements.
myFilter :: (Integer -> Bool) -> [Integer] -> [Integer]
myFilter f l = filter f l

-- | Maps the given list. If the map function returns Nothing, just drop it.
myFilterMap :: (Integer -> Maybe Integer) -> [Integer] -> [Integer]
myFilterMap f l = processFilterMap l 
  where   processFilterMap (x:xs) 
                                  | (f x) /= Nothing = let (Just a) = (f x) in a:processFilterMap(xs)
                                  | otherwise = processFilterMap(xs)
          processFilterMap [] = []
                              

-- | Folds the list from the left, i.e., myFoldL init f [l1, l2, ..., ln] = (f (f (f (f init l1) l2) ...) ln).
myFoldL :: Integer -> (Integer -> Integer -> Integer) -> [Integer] -> Integer
myFoldL init f l = foldl f 0 l

-- | Reverses the given list.
myRev :: [Integer] -> [Integer]
myRev l = processRev l []
  where processRev (x:xs) acc = processRev xs (x:acc)
        processRev [] acc = acc  

