-- | Source positions and spans.
--
-- The compiler keeps spans deliberately small: a span is a pair of
-- one-based line\/column positions.  A source range is half-open, so the
-- span of the token @42@ in @42@ is @1:1-1:3@.
module WYAH.Span
  ( Position(..)
  , Span(..)
  , startOfInput
  , dummySpan
  , advancePosition
  , spanBetween
  , spans
  , mergeSpan
  , spanStartPosition
  , spanEndPosition
  , renderSpan
  , renderSpanSuffix
  ) where

-- | A one-based source position.
newtype Position = Position {unPosition :: (Int, Int)}
  deriving (Eq, Ord)

instance Show Position where
  show (Position (line, column)) = show line <> ":" <> show column

-- | A source span.  'NoSpan' is used for synthetic or compiler-generated
-- syntax that cannot be attributed to user source.
data Span
  = Span
      { spanStart :: Position
      , spanEnd :: Position
      }
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
    '\t' -> Position (line, column + 1)
    _ -> Position (line, column + 1)

-- | Build the smallest span that starts at the start of the first span and
-- ends at the end of the second span.  If either span is missing the other
-- span is used.  The result is normalized so that a malformed pair of
-- positions can never produce a backwards span.
spanBetween :: Span -> Span -> Span
spanBetween NoSpan right = right
spanBetween left NoSpan = left
spanBetween (Span start _) (Span _ end)
  | end < start = Span start start
  | otherwise = Span start end

-- | A span covering all of the supplied spans.
spans :: [Span] -> Span
spans = foldr spanBetween NoSpan

mergeSpan :: Span -> Span -> Span
mergeSpan = spanBetween

spanStartPosition :: Span -> Maybe Position
spanStartPosition NoSpan = Nothing
spanStartPosition (Span start _) = Just start

spanEndPosition :: Span -> Maybe Position
spanEndPosition NoSpan = Nothing
spanEndPosition (Span _ end) = Just end

renderSpan :: Span -> String
renderSpan NoSpan = "<unknown>"
renderSpan (Span start end)
  | start == end = show start
  | otherwise = show start <> "-" <> show end

-- | Render a span as a parenthesized source suffix, or the empty string when
-- no source location is known.
renderSpanSuffix :: Span -> String
renderSpanSuffix NoSpan = ""
renderSpanSuffix span_ = " (at " <> renderSpan span_ <> ")"
