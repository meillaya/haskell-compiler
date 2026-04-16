module WYAH.Span
  ( Position(..)
  , Span(..)
  , startOfInput
  , dummySpan
  , advancePosition
  , spanBetween
  , renderSpan
  ) where

newtype Position = Position { unPosition :: (Int, Int) }
  deriving (Eq, Ord)

instance Show Position where
  show (Position (line, column)) = show line <> ":" <> show column

data Span
  = Span { spanStart :: Position, spanEnd :: Position }
  | NoSpan
  deriving (Eq, Ord, Show)

startOfInput :: Position
startOfInput = Position (1, 1)

dummySpan :: Span
dummySpan = Span startOfInput startOfInput

advancePosition :: Position -> Char -> Position
advancePosition (Position (line, column)) char =
  case char of
    '\n' -> Position (line + 1, 1)
    _ -> Position (line, column + 1)

spanBetween :: Span -> Span -> Span
spanBetween (Span start _) (Span _ end) = Span start end
spanBetween NoSpan right = right
spanBetween left NoSpan = left

renderSpan :: Span -> String
renderSpan NoSpan = "<unknown>"
renderSpan (Span start end)
  | start == end = show start
  | otherwise = show start <> "-" <> show end
