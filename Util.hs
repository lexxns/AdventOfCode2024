module Util where

dropLast :: [a] -> [a]
dropLast    []  =  []
dropLast    xs  =  init xs