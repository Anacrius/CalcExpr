{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import EvalExpr
import MyErrors

import System.Environment

main :: IO()
main = do
    args    <- getArgs
    check args
    where
        check args
            | length args /= 1  = errorPrinter InvalidParams
            | otherwise         = evalChecker $ args !! 0