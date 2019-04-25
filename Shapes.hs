module Shapes (
  Point(..),
  Shape(..),
  surface,
  nudge,
  baseCircle,
  baseRect
  ) where

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
--
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 3
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point xo yo) r) x y = Circle (Point (xo+x) (yo+y)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) x y = Rectangle (Point (x1+x) (y1+y)) (Point (x2+x) (y2+y))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
