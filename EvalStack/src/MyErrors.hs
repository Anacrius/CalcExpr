--  
-- EPITECH PROJECT, 2020
-- FUN Project #1 : Wolfram - Cellular automaton
-- File description:
-- Errors file
--

module MyErrors where

import System.Exit
import System.IO


data CustomError    = InvalidParams
                    | IllegalChar [Int] String
                    | TooManyRight
                    | TooManyLeft
                    | DivZero String Int
                    | MiscError String

instance Show CustomError where
    show InvalidParams      =  "USAGE: ./funEvalExpr c\n"
                                ++ "\tc: calcul wrapped into a string"
    show (IllegalChar l s)  = "Illegal characters used in:\n  \"" ++ s ++ "\"\n   " ++ (pinChar l) ++ "\n \x1b[90m( •̀ω•́ )σ  { keep ur trash characters away }\x1b[0m"
    show TooManyRight       = "Unmatching parenthesis - more ')' than '('"
    show TooManyLeft        = "Unmatching parenthesis - more '(' than ')'"
    show (DivZero str pos)  = "Division by \x1b[33mzero\x1b[0m in:\n  \"" ++ hightlight str pos 2 ++ "\"\n" ++ (take (pos + 3) $ repeat ' ') ++ "\x1b[90mヽ( `д´*)\x1b[0m"
    show (MiscError str)    = str

errorPrinter msg = do
    hPutStrLn stderr $ "\x1b[31mError:\x1b[0m " ++ (show msg)
    exitWith         $ ExitFailure 84

type    ErrorM  =   Either CustomError


   {----------------------
    - funcs 4 pretty
    - print
---------------------}

-- insert w/o replacing l1 inside l2 at n index
trueInsertNth :: Int -> [a] -> [a] -> [a]
trueInsertNth n l1 l2 = let (pref, suff) = splitAt n l2
                        in  pref ++ l1 ++ suff

-- replace
replaceNth :: [a] -> Int -> [a] -> [a]
replaceNth r n list = let (pref, suff) = splitAt n list
                      in  pref ++ r ++ (tail suff)

pinChar :: [Int] -> String
pinChar lint = foldr (replaceNth "\x1b[33m^\x1b[0m") (take ((last lint)+1) $ repeat ' ') (lint)

hightlight :: String -> Int -> Int -> String
hightlight str from to = let resize = from + to + (length "\x1b[33m")
                         in  trueInsertNth resize "\x1b[0m" (trueInsertNth from "\x1b[33m" str)