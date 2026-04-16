module WYAH.Diagnostic
  ( Diagnostic(..)
  , renderDiagnostic
  ) where

import WYAH.Span (Span, renderSpan)

data Diagnostic = Diagnostic
  { diagnosticPhase :: String
  , diagnosticSource :: Maybe FilePath
  , diagnosticSpan :: Maybe Span
  , diagnosticMessage :: String
  }
  deriving (Eq, Show)

renderDiagnostic :: Diagnostic -> String
renderDiagnostic diag =
  unwords
    [ "phase=" <> diagnosticPhase diag <> ";"
    , "source=" <> maybe "<unknown>" id (diagnosticSource diag) <> ";"
    , "span=" <> maybe "<unknown>" renderSpan (diagnosticSpan diag) <> ";"
    , "message=" <> diagnosticMessage diag
    ]
