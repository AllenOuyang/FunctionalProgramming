import Data.List
import Data.Char
{-**No1**有一个有N个元素的整数数组A，检查其中是否有元素为0，
如果有，求其中第一个元素的数目，即元素ai=0的最小i。-}
findFirst0 :: [Int] -> Int
findFirst0 [] = -1  --没有0返回-1
findFirst0 a = head [i | i <- [0,1..length a - 1], a!!i == 0]

{-**No2**给出一个N个正整数的数组，求奇数的算术平均数和几何平均数。-}
calArithmeticAvg :: [Int] -> Double
calArithmeticAvg l = avg (filter (\x -> if x `mod` 2 == 1 then True else False) l)
    where 
        avg :: [Int] -> Double
        avg l = (fromIntegral (foldl (+) 0 l)) / (fromIntegral (length l))

calAlgebraicAvg :: [Int] -> Double
calAlgebraicAvg l = (exp 1) ** (avg (toLog (filter isOdd l)))
    where
        avg :: [Double] -> Double
        avg l = (foldl (+) 0 l) / (fromIntegral (length l)) --求平均数

        isOdd :: Int -> Bool
        isOdd x = if x `mod` 2 == 1 then True else False    --foldl的辅助函数

        toLog :: [Int] -> [Double]
        toLog l = [log (fromIntegral x) | x <- l]           --对list全部求ln

{-**No3**输入数组A = ( a1, a2, ..., an).从左到右遍历其元素，用下一个和上一个元素的均值替换其中的每个零元素。
如果第一个元素为0那么用第二替换，如果最后一个元素为0那么用倒数第二个元素替换。-}
replaceZero :: (Fractional a, Ord a) => [a] -> [a]
replaceZero [] = []
replaceZero [x,y]
    | x == 0 = [y,y]
    | y == 0 = [x,x]
replaceZero l = [if nl!!i == 0 then (nl!!(i-1) + nl!!(i+1)) / 2 else nl!!i | i <- [1,2..length nl - 2]]
    where 
        nl = [l!!1] ++ l ++ [l!!(length l - 2)]

{-**No4**确定一个由M个元素组成的给定数组中元素的符号变化数。-}
findTimesofChange :: (Num a, Ord a) => [a] -> Int
findTimesofChange [x] = 0
findTimesofChange [] = 0
findTimesofChange (x:xs)
    | x * nxt < 0 = 1 + findTimesofChange xs 
    | otherwise = findTimesofChange xs 
    where
        nxt = head xs


{-**No5**用1替数组中绝对值最小的元素，如果有多个，则替换多个-}
replaceOne :: (Num a, Ord a) => [a] -> [a]
replaceOne [] = []
replaceOne l = [if abs e == minn then 1 else e | e <- l]
    where
        minimum' [x] = abs x
        minimum' (x:xs)
            | abs x < minTail = abs x
            | otherwise = minTail
            where
                minTail = minimum' xs
        minn = minimum' l 

{--**No8**给出一个150个元素的整数数组。将所有能被5整除的元素分配到一个单独的数组中。-}
arrayDivFive :: (Integral a, Eq a) => [a] -> [a]
arrayDivFive [] = []
arrayDivFive l = [e | e <- l, e `mod` 5 == 0]
-- filter (\x -> x `mod` 5) l

{--**No9**一条直线有n个坐标，按升序排列：x1, x2, ..., xn.求相邻点之间的最大距离，打印这些点的编号。-}
findLongestDis :: (Num a, Floating a, Ord a) => [(a,a)] -> IO ()
findLongestDis l = putStrLn ("The longest distance is the one between the " ++ show ind ++ "th point and " ++ "the " ++ show (ind + 1) ++ "th point")
    where
        dis = [sqrt ((fst (l!!i) - fst (l!!(i-1)))**2 + (snd (l!!i) - snd (l!!(i-1)))**2) | i <- [1,2..length l - 1]]
        maxx = maximum dis
        index = findIndex (== maxx) dis
        ind = (\(Just i) -> i) index


{--**No10**找到列表中所有奇数的乘积。-}
productOfOdd :: [Int] -> Int
productOfOdd [] = 1
productOfOdd (x:xs)
    | odd x = x * productOfOdd xs
    | otherwise = productOfOdd xs

{--**No11**给定一个有n个元素的数组A和一个实数c. 将A中大于c的所有元素写进新数组B。-}
listGtThanC :: (Ord a) => [a] -> a -> [a]
listGtThanC l c = [e | e <- l, e > c]

{--**No12**找出自然数数组中最小的偶数。-}
minEven :: [Int] -> Int
minEven l = minimum (filter even l)

{--**No13**给出一个n个整数的序列和一个整数p，求能被p整除的元素之和。-}
sumDivisableByP :: [Int] -> Int -> Int
sumDivisableByP [] _ = 0
sumDivisableByP (x:xs) p
    | x `mod` p == 0 = x + sumDivisableByP xs p
    | otherwise = sumDivisableByP xs p

{--**No14**给出整数x1，x2，...，xn（这个序列中可能有重复的项）。 得到所有进入这个序列的数字，每次都是一个。-}
mynub :: (Eq a) => [a] -> [a]
mynub [] = []
mynub (x:xs)
    | x `elem` xs = mynub xs
    | otherwise = x : mynub xs

nub2 :: Eq a => [a] -> [a]
nub2 [] = []
nub2 (x : xs) = x : [y | y <- nub2 xs, y /= x] -- /= means 'is not equal'

{--**No15**给出自然数x1，x2，...，xn（所有数字都是成对不同的）。 把这个序列中最大和最小的项交换。-}
swapMaxMin :: [Int] -> [Int]
swapMaxMin [] = []
swapMaxMin l = swapTwo maxInd minInd l
    where
        swapTwo f s xs = zipWith (\x y -> if x == f then xs !! s else if x == s then xs !! f else y) [0..] xs
        maxInd = (\(Just i) -> i) (findIndex (== (maximum l)) l) 
        minInd = (\(Just i) -> i) (findIndex (== (minimum l)) l)

{--**No16**给出整数x1, x2, ..., xn.得到一个新的序列，它与原序列的不同之处在于所有奇数项都被加倍。-}
doubleOdd :: [Int] -> [Int]
doubleOdd l = [if odd e then e*2 else e | e <- l]

{--**No17**给出整数a, x1, x2, ..., xn.确定序列x1, x2, ..., xn中的哪项等于a.如果没有这样的项，那么答案应该是0。-}
myFindIndex :: Int -> [Int] -> Int
myFindIndex a l
    | res == Nothing = 0
    | otherwise = (\(Just i) -> i) res + 1
    where
        res = findIndex (==a) l

{--**No18**求一个由N个元素组成的给定的整数数组A中非零元素的数量。-}
amountOfNoneZero :: [Int] -> Int
amountOfNoneZero [] = 0
amountOfNoneZero (x:xs)
    | x /= 0 = 1 + amountOfNoneZero xs
    | otherwise = amountOfNoneZero xs

{--**No19**做一个程序，根据数字7是否出现在给定的由N个元素组成的整数阵A中，回答是或不是。-}
isContainSeven :: [Int] -> IO ()
isContainSeven l
    | '7' `elem` str = putStrLn "Да"
    | otherwise = putStrLn "Нет"
    where
        str = foldr (++) "" [show e | e <- l]

{--**No20**给出一个有N个元素的实数数组A。求大于数组中所有元素的算术平均值的元素数。-}
amountOfoverAvg :: (Fractional a, Ord a) => [a] -> Int
amountOfoverAvg l = helper l (avg l)
    where 
        avg l = (foldl (+) 0 l) / (fromIntegral (length l))
        helper [] _ = 0
        helper (x:xs) ag
            | x > ag = 1 + helper xs ag
            | otherwise = helper xs ag

{--**No21**给出一个有N个元素的整数数组A。计算最大的数字在数组中出现了多少次。-}
findTimesMax :: [Int] -> Int 
findTimesMax l = helper l (maximum l)
    where
        helper [] _ = 0
        helper (x:xs) maxx
            | x == maxx = 1 + helper xs maxx
            | otherwise = helper xs maxx

{--**No22**有一个有N个元素的整数数组A。检查其中是否有等于0的元素。如果有，求其中第一个元素的索引，即元素ai=0的最小i。-}
firstZeroInd :: [Int] -> Int 
firstZeroInd [] = -1
firstZeroInd l
    | zero == Nothing = -1
    | otherwise = (\(Just i) -> i) zero 
    where
        zero = findIndex (==0) l

{--**No23**给出一个有N个元素的整数数组A。计算最长连续子数组的长度。-}
longestSubseq :: [Int] -> Int
longestSubseq [] = 0
longestSubseq l = maximum $ map length $ groupBy (\x y -> x == y) l


-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy rel []          =  []
-- groupBy rel (x:xs)      =  (x:ys) : groupBy rel zs
--    where (ys,zs) = groupByAux x xs
--          groupByAux x0 (x:xs) | rel x0 x = (x:ys, zs)
--            where (ys,zs) = groupByAux x xs
--          groupByAux y xs = ([], xs)

{--**No24**计算在有N个元素的整数阵列A中出现的不同数字的数量。重复的数字只数一次。-}
amountOfDigit :: [Int] -> Int
amountOfDigit l = length (nub l)

{--**No25** 给出一个有N个元素的整数数组A.构造一个程序来构造一个数组，其中数组A的所有负元素在所有非负元素前。-}
negBeforePos :: [Int] -> [Int]
negBeforePos l = [e | e <- l, e < 0] ++ [e | e <- l, e >= 0]

{--**No26**有三个整数数组A、B和C，每个数组由N个元素组成，已知三个数组中都有某些整数出现。找到这些整数中的一个。-}
commonInt :: [Int] -> [Int] -> [Int] -> Int
commonInt x y z = head (intersect z $ intersect x y)

{--**No27**给出一个有N个元素的整数数组A，编写一个程序，将元素按升序排列。-}
-- 快排
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x] 
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

{--**No28**在N个整数的数组A中搜索一个等于给定数字x的元素的索引。-}
firstXInd :: [Int] -> Int -> Int 
firstXInd [] _ = -1
firstXInd l x
    | res == Nothing = -1
    | otherwise = (\(Just i) -> i) res
    where
        res = findIndex (==x) l

{--**No29** 有两个数组A和B。每个数组由N个元素组成，请计算这些数组元素的数量。
1) ai > bi; 2) ai = bi; 3) ai < bi。-}
amountUnderCon :: (Num a, Ord a) => [a] -> [a] -> [a]
amountUnderCon [] [] = []
amountUnderCon p q = (condition1 p q):(condition2 p q):(condition3 p q):[]
    where
        condition1 [] [] = 0
        condition1 (x:xs) (y:ys)
            | x > y = 1 + condition1 xs ys 
            | otherwise = condition1 xs ys
        condition2 [] [] = 0
        condition2 (x:xs) (y:ys)
            | x == y = 1 + condition1 xs ys 
            | otherwise = condition1 xs ys
        condition3 [] [] = 0
        condition3 (x:xs) (y:ys)
            | x < y = 1 + condition1 xs ys 
            | otherwise = condition1 xs ys

{--**No30**给出一个由100个整数组成的序列，请确定连续为零的最长子序列中的数字数量。-}
longestZeroSubseq :: [Int] -> Int
longestZeroSubseq [] = 0
longestZeroSubseq l
    | 0 `elem` l = maximum $ map length $ groupBy (\x y -> x == y && x == 0) l
    | otherwise = 0

{--**No31**有200个实数，请确定其中有多少个实数大于其 "邻居"，即上一个和下一个数字，找有多少个波峰。-}
waveCrest :: (Ord a) => [a] -> Int 
waveCrest l = sum [if l!!i > l!!(i-1) && l!!i > l!!(i+1) then 1 else 0 | i <- [1,2..length l - 2]]

{--**No32**给出一个有N个元素的整数数组A和一个自然数k<N.求数组A的最后k个最小值的索引和值。-}
indexAndValOfMin :: [Int] -> Int -> (Int, Int)
indexAndValOfMin l k = ((\(Just i) -> i) ind + start, minn)
    where
        start = length l - k
        nl = [l!!i | i <- [length l - k..length l - 1]]
        minn = minimum nl
        ind = findIndex (==minn) nl

{--**No33**给出一个有N个元素的整数数组A和一个自然数k<N.求数组A的开头k个最小值的索引和值。-}
indexAndValOfMin1 :: [Int] -> Int -> (Int, Int)
indexAndValOfMin1 l k = ((\(Just i) -> i) ind, minn)
    where
        nl = [l!!i | i <- [0..k - 1]]
        minn = minimum nl
        ind = findIndex (==minn) nl

{--**No34** 有一个有N个元素的整数数组A，有一个自然数k≤N，从数组a中删除索引为k的元素。-}
removeKth :: [Int] -> Int -> [Int]
removeKth [] _ = []
removeKth (x:xs) 0 = removeKth xs (-1)
removeKth (x:xs) k = x:removeKth xs (k - 1)

{--**No35** 有一个有N个元素的整数数组A，找到其中偶数的最大值的索引和值。-}
evenMax :: [Int] -> (Int, Int)
evenMax l = ((\(Just i) -> i) ind, maxx)
    where
        maxx = maximum (filter even l)
        ind = findIndex (==maxx) l

{--**No36** 有一个有N个元素的整数数组A，找到其中奇数的最大值的索引和值。-}
oddMax :: [Int] -> (Int, Int)
oddMax l = ((\(Just i) -> i) ind, maxx)
    where
        maxx = maximum (filter odd l)
        ind = findIndex (==maxx) l

{--**No37** 有一个有N个元素的整数数组A，找到其中可以整除5的数最大值的索引和值。-}
multiple5Max :: [Int] -> (Int, Int)
multiple5Max l = ((\(Just i) -> i) ind, maxx)
    where
        maxx = maximum (filter (\x -> x `mod` 5 == 0) l)
        ind = findIndex (==maxx) l

{--**No38** 给出一个有N个元素的整数数组A。按降序排列。-}
quicksortDes :: (Ord a) => [a] -> [a]
quicksortDes [] = []
quicksortDes (x:xs) =
    let smallerSorted = quicksortDes [a | a <- xs, a <= x] 
        biggerSorted = quicksortDes [a | a <- xs, a > x]
    in biggerSorted ++ [x] ++ smallerSorted

{--**No39** 给出一个有N个元素的整数数组A。按照每个元素记录中的数字数量的降序排列。-}
howManyDigit :: Int -> Int
howManyDigit 0 = 0
howManyDigit x = 1 + howManyDigit (x `div` 10)

quicksortSpec :: [Int] -> [Int]
quicksortSpec [] = []
quicksortSpec (x:xs) =
    let smallerSorted = quicksortSpec [a | a <- xs, howManyDigit a <= howManyDigit x] 
        biggerSorted = quicksortSpec [a | a <- xs, howManyDigit a > howManyDigit x]
    in biggerSorted ++ [x] ++ smallerSorted

{--**No40** 给出一个有N个元素的整数数组A，按每个元素记录的第一个数字进行升序排列。-}
quicksortFirst :: [Int] -> [Int]
quicksortFirst [] = []
quicksortFirst (x:xs) =
    let smallerSorted = quicksortFirst [a | a <- xs, firstDigit a <= firstDigit x] 
        biggerSorted = quicksortFirst [a | a <- xs, firstDigit a > firstDigit x]
    in smallerSorted ++ [x] ++ biggerSorted
    where
        firstDigit x = digitToInt ((show x)!!0)
