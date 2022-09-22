{-# LANGUAGE OverloadedRecordDot #-}

module MyLib where

data Point = Point {x :: Int, y :: Int}

getX :: Point -> Int
getX p = p.x
