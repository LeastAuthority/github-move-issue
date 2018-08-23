module Main where

import System.Environment
  ( getArgs
  )

import Data.Text
  ( pack
  )

import Lib

main :: IO ()
main = do
  args <- getArgs
  let args' = map pack args
  let token = args' !! 0
  let sourceOwner = args' !! 1
  let sourceRepo = args' !! 2
  let sourceNum = read $ (args !! 3)
  let destOwner = args' !! 4
  let destRepo = args' !! 5
  someFunc token sourceOwner sourceRepo sourceNum destOwner destRepo
