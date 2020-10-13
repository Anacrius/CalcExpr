{-# LANGUAGE MultiWayIf #-}

module EvalExpr where

import MyErrors

import Data.Either      (lefts, rights)
import Data.List
import Data.Maybe

data Calcul = Calcul { lrest ::String, used ::Res, rrest ::String } 

data Res = Res  { lvalue    :: String
                , operator  :: Char
                , rvalue    :: String
                }

instance Show Res where
    show (Res a '+' b) = show $ (read a ::Float)  +  (read b ::Float)
    show (Res a '-' b) = show $ (read a ::Float)  -  (read b ::Float)
    show (Res a '*' b) = show $ (read a ::Float)  *  (read b ::Float)
    show (Res a '/' b) = show $ (read a ::Float)  /  (read b ::Float)
    show (Res a '^' b) = show $ (read a ::Float)  ** (read b ::Float)

instance Show Calcul where
    show calc = (lrest calc) ++ (show $ used calc) ++ (rrest calc)


toCalcul' :: (String, String) -> Calcul
toCalcul' strs =
                case (length h, length g) of
                    (0, 0) -> Calcul "" (Res (fst strs) ( (snd strs) !!0) (tail $ snd strs)) ""
                    (0, _) -> if
                            | (snd strs) !!1 == '-' -> Calcul "" (Res (take (g !!1) (tail $ snd strs))  ((snd strs) !!((g!!0)+1)) (drop ((g !!0)+2) $ snd strs) ) ((drop ((g !!1)+1) $ snd strs))
                            | otherwise -> Calcul "" (Res (fst strs) ( (snd strs) !!0) (take (head g) (tail $ snd strs))) (drop (head g) (tail $ snd $ strs))
                    (1, 0) -> if
                            | (h !!0) == ((length $ fst strs) - 1) -> Calcul "" (Res (init $ fst strs) ( (snd strs) !!0) (tail $ snd strs)) ""
                            | otherwise -> Calcul (take ((last h) + 1) $ fst strs) (Res (drop ((last h) + 1) $ fst strs) ( (snd strs) !!0) (tail $ snd strs)) ""
                    (1, _) -> if
                            | (snd strs) !!1 == '-' -> Calcul "" (Res (init $ fst strs) ((snd strs) !!((g!!0)+1)) (take (head g) (tail $ snd strs))) (drop (head g) (tail $ snd $ strs))
                            | otherwise -> Calcul (take ((last h) + 1) $ fst strs) (Res (drop ((last h) + 1) $ fst strs) ( (snd strs) !!0) (take (head g) (tail $ snd strs))) (drop (head g) (tail $ snd $ strs))
                    (_, 0) -> if
                            | (last h) == (length $ fst strs) - 1 -> Calcul (take ((last $ tail h) + 1) $ fst strs) (Res (drop ((last h) + 1) $ fst strs) ( (snd strs) !!0) (tail $ snd strs)) ""
                            | otherwise -> Calcul (take ((last h) + 1) $ fst strs) (Res (drop ((last h) + 1) $ fst strs) ( (snd strs) !!0) (tail $ snd strs)) ""
                    _      -> Calcul (take ((last h) + 1) $ fst strs) (Res (drop ((last h) + 1) $ fst strs) ( (snd strs) !!0) (take (head g) (tail $ snd strs))) (drop (head g) (tail $ snd $ strs))
                where
                    h = (findIndices (`elem` "+-*/^") $ fst strs)
                    g
                        | (tail $ snd strs) !!0 /= '-' = (findIndices (`elem` "+-*/^") $ tail $ snd strs)
                        | otherwise = (findIndices (`elem` "+-*/^") $ tail $ tail $ snd strs)

makeCalc :: String -> String
makeCalc calc
            | length (findIndices (`elem` "+-*/^") (tail calc)) /= 0 = makeCalc (show $ toCalcul calc)
            | otherwise = calc

toCalcul :: String -> Calcul
toCalcul str
        | getIndex '^' str /= (-1) = toCalcul' $ splitAt (getIndex '^' str) str
        | mu /= (-1) || di /= (-1) = if
            | mu > di && di > (-1) -> toCalcul' $ splitAt di str
            | di > mu && mu > (-1) -> toCalcul' $ splitAt mu str
            | di /= (-1) -> toCalcul' $ splitAt di str
            | otherwise  -> toCalcul' $ splitAt mu str
        | ad /= (-1) || mi /= (-1) = if
            | ad > mi && mi > (-1) -> toCalcul' $ splitAt mi str
            | mi > ad && ad > (-1) -> toCalcul' $ splitAt ad str
            | mi /= (-1) -> toCalcul' $ splitAt mi str
            | otherwise  -> toCalcul' $ splitAt ad str
        | otherwise = Calcul "" (Res str '+' "0") ""
            where   ad = getIndex '+' str
                    mi = getIndex '-' str
                    mu = getIndex '*' str
                    di = getIndex '/' str


-- Error Messages
errorList       = ["Missing '('",   "Missing ')",   "Misplaced: ')' is before '('"]

-- Operators
opeList         = [ '^', '*', '/', '+', '-']
--opeFunc         = [(**), (*), (/), (+), (-)]

-- erase all the ' ' from the string
clearSpace :: String -> String
clearSpace xs = [x | x <- xs, not (x `elem` " ")]

-- return index of the searched char, -1 otherwise
getIndex :: Char -> String -> Int
getIndex c str = fromMaybe (-1) (findIndex (== c) $ str)
-- same for lists
findSubList :: (Eq a) => [a] -> [a] -> Int
findSubList search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

-- delete element at nth index from a list
deleteNth :: Int -> [a] -> [a]
deleteNth n list =  let (pref, suff) = splitAt n list
                    in  pref ++ (tail suff)

-- insert and replace l1 inside l2 at n index
insertNth :: Int -> [a] -> [a] -> [a]
insertNth n l1 l2 = let (pref, suff) = splitAt n l2
                    in  pref ++ l1 ++ (tail suff)

-- check a couple of parenthesis logic
isLogic :: String -> (Int, Int)
isLogic str = do
    let a = getIndex '(' str
        b = getIndex ')' str
    case (a, b) of
        (-1, -1)    ->  (0, 0)
        (-1, _)     ->  (-1, 0)
        (_, -1)     ->  (-2, 0)
        _           ->  if
                        | b < a -> (-3, 0)
                        | otherwise -> (a, b)

-- interpreting error message from isLogic
-- if no error, return (Int, Int) with fst = left parenthesis & snd = right parenthesis
checkParenthesis :: String -> Either String (Int, Int)
checkParenthesis str = do
    let a = isLogic str
    if (fst a) < 0
        then Left $ errorList !! ((abs $ fst a) - 1)
    else
        Right a

isLegal :: Char -> Bool
isLegal c
    | c `elem` ['0'..'9'] = True
    | c `elem` ['+', '-', '/', '*', '^', '(', ')'] = True
    | otherwise           = False

areULegal :: String -> [Int]
areULegal str = findIndices (== False) $ map isLegal str

evalChecker :: String -> IO ()
evalChecker str
            | length str == 0               = errorPrinter (MiscError $ "༼ つ ಥ_ಥ ༽つ  { C'mon gimme numbers } ")
            | length illegalList /= 0       = errorPrinter (IllegalChar illegalList str)
            | findSubList "/0" str /= (-1)  = errorPrinter (DivZero str $ findSubList "/0" str)
            | nbLeft  > nbRight             = errorPrinter TooManyLeft
            | nbRight > nbLeft              = errorPrinter TooManyRight
            | length pLogic /= 0            = errorPrinter (MiscError $ head pLogic)
            | otherwise = evalExpr str
                where   pLogic  = lefts [checkParenthesis str]
                        nbLeft  = length $ findIndices (== '(') str
                        nbRight = length $ findIndices (== ')') str
                        illegalList = areULegal str

evalExpr :: String -> IO ()
evalExpr str =
        case (parPos) of
            (-1, -1) -> putStrLn $ makeCalc str
            (-1, _)  -> errorPrinter InvalidParams
            (_, -1)  -> errorPrinter InvalidParams
            _        -> evalExpr (insertNth (getIndex ')' (deleteFromTo str (fstp) (diff))) (makeCalc $ tail (getSublist str (fstp) (diff))) (deleteFromTo str (fstp) (diff)))
        where   parPos = getPrio str
                fstp   = fst parPos
                sndp   = snd parPos
                diff   = sndp - fstp

-- get first match of _ : '(' : _ : ')' : _
-- where snd = first occurence of ')'
-- and   fst = corresponding '(' of snd
getPrio :: String -> (Int, Int)
getPrio str =   (matchingParth $ fst $ splitAt (a) str, a)
                where a = getIndex ')' str

-- get the last '(' from a str w/ only one ')' at the end
matchingParth :: String -> Int
matchingParth str
        | a == (-1) = a
        | otherwise = length str - a - 1
        where a = getIndex '(' $ reverse str

-- from = starting index, to = nb of elements to delete
deleteFromTo :: [a] -> Int -> Int -> [a]
deleteFromTo list from to = a ++ b
    where   a = fst c
            b = drop to $ snd c
            c = splitAt from list

-- from = starting index, to = nb of elements to get
getSublist :: [a] -> Int -> Int -> [a]
getSublist list from to = take to $ drop from list