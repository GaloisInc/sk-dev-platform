-- The following is a scrap of code to placed directly in the Haskell module
-- ouptut by alex (http://www.haskell.org/alex/doc/html/introduction.html).
{
module SCD.Lobster.Lexer(
   Token(..),
   TokenConstructor(..),
   alexScanTokens,
   posnToString
 ) where

import qualified SCD.Lobster.Syntax as Syntax
}

--------------------------------------------------------------------------------
-- Select the position wrapper, which keeps track of line and column numbers
-- along with the tokens produced by the Lexer. The type of token actions is:
-- AlexPosn -> String -> Token
--
-- where AlexPosn is defined as
-- data AlexPosn = AlexPn !Int  -- absolute character offset
--                        !Int  -- line number
--                        !Int  -- column number
--------------------------------------------------------------------------------

%wrapper "posn"

@lower = [a-z]                                       -- Lower case letters
@upper = [A-Z]                                       -- Upper case letters
@digit = [0-9]                                       -- Digits
@identifier = @lower(@lower|@upper|@digit|"_")*      -- Value identifier
@typeIdentifier = @upper(@lower|@upper|@digit|"_")*  -- Type identifier
@number = @digit@digit*                              -- Number, sequence of digits
@quotedString = \"([^\"]|'\\'\")*\"                  -- String in quotes

:-

$white+ ;                                              -- Ignore white space
"#".* ;                                                -- Ignore comments
bidirectional { ctoken BidirectionalKeyword }
class { ctoken ClassKeyword }
direction { ctoken DirectionKeyword }
input { ctoken InputKeyword }
new { ctoken NewKeyword }
object { ctoken ObjectKeyword }
output { ctoken OutputKeyword }
port { ctoken PortKeyword }
position { ctoken PositionKeyword }
subject { ctoken SubjectKeyword }
type { ctoken TypeKeyword }
"<-->" { ctoken (Connection Syntax.BidirectionalConnection) }
"-->" { ctoken (Connection Syntax.LeftToRightConnection) }
"<--" { ctoken (Connection Syntax.RightToLeftConnection) }
"--" { ctoken (Connection Syntax.NeutralConnection) }
@identifier { atoken identifier }
@typeIdentifier { atoken typeIdentifier }
@number { atoken number }
@quotedString { atoken quotedString }
":" { ctoken Colon }
"," { ctoken Comma }
"." { ctoken Dot }
"=" { ctoken Equals }
"{" { ctoken LeftBrace }
"}" { ctoken RightBrace }
"(" { ctoken LeftBracket }
")" { ctoken RightBracket }
";" { ctoken SemiColon }
"*" { ctoken Star }
. { lexError }

{
-- Each action has type :: String -> Token

-- This is the Token type produced by the generated Lexer
data Token = Token AlexPosn TokenConstructor
  deriving (Show, Eq)

-- And some supporting types for the Lexer
-- Suggestion:
--   data CookedIdentifier = ShortIdentifier String | LongIdentifer [String]
--   data RawIdentifier    = Raw String
--   data Identifier       = ( CookedIdentifier, RawIdentifer )
data TokenConstructor =
-- Tokens with special handling
    Identifier Syntax.Identifier
  | TypeIdentifier Syntax.Identifier
  | Number Int
  | Connection Syntax.Connection
  | QuotedString String
-- Reserved words, each reserved word corresponds to a string of letters
  | BidirectionalKeyword
  | ClassKeyword
  | DirectionKeyword
  | InputKeyword
  | NewKeyword
  | ObjectKeyword
  | OutputKeyword
  | PortKeyword
  | PositionKeyword
  | SubjectKeyword
  | TypeKeyword
-- Symbols
  | Colon
  | Comma
  | Dot
  | Equals
  | LeftBrace
  | LeftBracket
  | RightBrace
  | RightBracket
  | SemiColon
  | Star
 deriving (Read, Show, Eq, Ord)

-- Action is the type of token actions, for the "position" kind of wrapper
type Action = AlexPosn -> String -> Token

-- A lower level function for converting a string into a lexer action. It
-- first converts the string into a TokenConstructor. For example, an identifier
-- string (containing dots) is converted to an Identifer TokenConstructor,
-- where the string with dots has been converted into a list of strings.
atoken :: (String -> TokenConstructor) -> Action
atoken c p s = Token p (c s)

-- Convert a token constructor to an action. The TokenConstructor determines the
-- corresponding string, and vice versa, so we can ignore the string in favor of
-- the token constructor.
ctoken :: TokenConstructor -> Action
ctoken c = atoken (const c)

-- Break an identifier into the component strings
identifier :: String -> TokenConstructor
identifier s = Identifier (Syntax.mkId s)

typeIdentifier :: String -> TokenConstructor
typeIdentifier s = TypeIdentifier (Syntax.mkId s)

-- Convert a number into a TokenConstructor. The lexer guarantees that the
-- input string to this function is all digits, so the read function will
-- produce an Int
number :: String -> TokenConstructor
number = Number . read

-- Build a quoted string, using the built in read function
quotedString :: String -> TokenConstructor
quotedString = QuotedString . read

-- Format a position for printing
posnToString :: AlexPosn -> String
posnToString (AlexPn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- Format a lexical error for printing
lexError :: Action
lexError p s = error ("Lexer error at " ++ posnToString p ++ ": " ++ show s)
}
