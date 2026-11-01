-- | The normalized diagnostic contract used by every compiler phase.
--
-- A diagnostic renders as:
--
-- > phase=<phase>; source=<source>; span=<span>; message=<message>
--
-- The format is intentionally structural and stable so that fixtures can
-- assert on phase, source, and location without depending on prose.
module WYAH.Diagnostic
  ( Phase(..)
  , phaseName
  , Diagnostic(..)
  , renderDiagnostic
  , diagnosticLine
  , CompilerError(..)
  , renderCompilerError
  ) where

import WYAH.Span (Span, renderSpan)

-- | The compiler phase that owns a diagnostic.
data Phase
  = ParsePhase
  | RenamePhase
  | KindPhase
  | InferPhase
  | DesugarPhase
  | EvalPhase
  | PipelinePhase
  deriving (Eq, Ord, Show)

phaseName :: Phase -> String
phaseName phase = case phase of
  ParsePhase -> "parse"
  RenamePhase -> "rename"
  KindPhase -> "kind"
  InferPhase -> "infer"
  DesugarPhase -> "desugar"
  EvalPhase -> "eval"
  PipelinePhase -> "pipeline"

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

-- | A single-line rendering suitable for a REPL or a log.
diagnosticLine :: Diagnostic -> String
diagnosticLine diag =
  phaseNameOf (diagnosticPhase diag)
    <> maybe "" (\source -> " " <> source) (diagnosticSource diag)
    <> maybe "" (\span_ -> " " <> renderSpan span_) (diagnosticSpan diag)
    <> ": "
    <> diagnosticMessage diag
  where
    phaseNameOf = id

-- | A phase-parameterized error that can cross module boundaries.  Concrete
-- passes keep their own rich error types for direct tests, but the driver
-- normalizes everything to 'CompilerError'.
data CompilerError = CompilerError
  { compilerPhase :: Phase
  , compilerSource :: Maybe FilePath
  , compilerSpan :: Span
  , compilerMessage :: String
  }
  deriving (Eq)

renderCompilerError :: CompilerError -> String
renderCompilerError err =
  renderDiagnostic
    Diagnostic
      { diagnosticPhase = phaseName (compilerPhase err)
      , diagnosticSource = compilerSource err
      , diagnosticSpan = Just (compilerSpan err)
      , diagnosticMessage = compilerMessage err
      }
