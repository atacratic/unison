module Unison.Util.SyntaxText where

import Unison.Prelude

-- ! TODO topic/syntaxtext-markup
-- ! I've sketched out changes in here, which will break compilation...

import Unison.Util.AnnotatedText      ( AnnotatedText(..), annotate )
import Referent as R
import Data.Text (Text)

type SyntaxText = AnnotatedText Element

-- The elements of the Unison grammar, for syntax highlighting purposes
data Element = NumericLiteral
             | TextLiteral
             | CharLiteral
             | BooleanLiteral
             | Blank
             | Var
             | Reference ReferentInfo
             | Constructor ReferentInfo
             | Request ReferentInfo
             | AbilityBraces
             -- let|handle|in|where|match|with|cases|->|if|then|else|and|or
             | ControlKeyword
             -- forall|->
             | TypeOperator
             | BindingEquals
             | TypeAscriptionColon
             -- type|ability
             | DataTypeKeyword
             | DataTypeParams
             | DataType ReferentInfo
             -- unique
             | DataTypeModifier
             -- `use Foo bar` is keyword, prefix, suffix
             | UseKeyword
             | UsePrefix
             | UseSuffix
             | HashQualifier
             | DelayForceChar
             -- ? , ` [ ] @ |
             -- Currently not all commas in the pretty-print output are marked up as DelimiterChar - we miss
             -- out characters emitted by Pretty.hs helpers like Pretty.commas.
             | DelimiterChar
             -- ! '
             | Parenthesis
             | LinkKeyword -- `typeLink` and `termLink`
             -- [: :] @[]
             | DocDelimiter
             -- the 'include' in @[include], etc
             | DocKeyword
             deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- A Referent (typically including a term hash), and a corresponding FQN.  Useful
-- for providing more detail when rendering terms to the reader.
-- The pretty-printed source may not otherwise include the whole FQN - it may have been
-- abbreviated by dropping some prefix of the name.
type ReferentInfo = ReferentInfo R.Referent Text
-- ! TODO topic/syntaxtext-markup
-- ! In the Reference and DataType case you can hopefully use Referent.Ref to wrap and
-- ! get a Referent, same as line 170 of TermPrinter.hs.  If that ends turning into a hack
-- ! you can define `type ReferenceInfo = ReferenceInfo R.Reference Text` as well.

syntax :: Element -> SyntaxText -> SyntaxText
syntax = annotate

-- Convert a `SyntaxText` to a `String`, ignoring syntax markup
toPlain :: SyntaxText -> String
toPlain (AnnotatedText at) = join (toList $ fst <$> at)
