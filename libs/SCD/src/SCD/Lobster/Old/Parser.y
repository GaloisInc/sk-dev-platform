{
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

-- This code is ralphed out verbatim into the generated Haskell module header.
module SCD.Lobster.Parser(
    parser
  ) where

import qualified Data.List as List
import qualified SCD.Lobster.Syntax as Syntax
import qualified SCD.Lobster.Lexer as Lexer
}

-- Happy will generate a parsing function called "parser"
%name parser

-- The parser will work with tokens of type Token, which is defined
-- by the alex lexer
%tokentype { Lexer.Token }

-- The function parseError in case of a parsing error. The function
-- parseError is defined later in this file.
%error { parseError }

-- The tokens used in defining the langauge. This list is similar to the
-- list of tokens in the alex source for the lexer. The stuff in curly
-- braces is the Haskell pattern for the token, i.e. what is actually
-- returned by the lexer.
%token
identifier { Lexer.Token _ (Lexer.Identifier $$) }
typeIdentifier { Lexer.Token _ (Lexer.TypeIdentifier $$) }
number { Lexer.Token _ (Lexer.Number $$) }
connection { Lexer.Token _ (Lexer.Connection $$) }
quotedString { Lexer.Token _ (Lexer.QuotedString $$) }
bidirectional { Lexer.Token _ Lexer.BidirectionalKeyword }
class { Lexer.Token _ Lexer.ClassKeyword }
direction { Lexer.Token _ Lexer.DirectionKeyword }
input { Lexer.Token _ Lexer.InputKeyword }
new { Lexer.Token _ Lexer.NewKeyword }
object { Lexer.Token _ Lexer.ObjectKeyword }
output { Lexer.Token _ Lexer.OutputKeyword }
port { Lexer.Token _ Lexer.PortKeyword }
position { Lexer.Token _ Lexer.PositionKeyword }
subject { Lexer.Token _ Lexer.SubjectKeyword }
type { Lexer.Token _ Lexer.TypeKeyword }
':' { Lexer.Token _ Lexer.Colon }
',' { Lexer.Token _ Lexer.Comma }
'.' { Lexer.Token _ Lexer.Dot }
'=' { Lexer.Token _ Lexer.Equals }
'{' { Lexer.Token _ Lexer.LeftBrace }
'(' { Lexer.Token _ Lexer.LeftBracket }
'}' { Lexer.Token _ Lexer.RightBrace }
')' { Lexer.Token _ Lexer.RightBracket }
';' { Lexer.Token _ Lexer.SemiColon }
'*' { Lexer.Token _ Lexer.Star }

%%

-- Overall, the result of the parse is an abstract syntax tree (AST)
-- as defined by Syntax.hs. Each group of productions in the grammar has
-- an overall return type specified (.e.g :: Policy), and each member
-- of the group of productions has a value of that type specified
-- (e.g. Policy $1).

-- A policy is a list of statements. The type of the value resulting
-- from parsing a policy is "Policy", as defined in Syntax.hs.
policy :: { Syntax.Policy }
  : statements { Syntax.Policy $1 }

-- Upon parse of a list of declarations, the type returned is a list of
-- statements.
statements :: { [Syntax.Statement] }
  : {- empty -} { [] }
  | statement statements { $1 : $2 }

-- And so on and so forth for the rest of the grammar.
statement :: { Syntax.Statement }
  : class typeIdentifier parameterList '{' statements '}'
    { Syntax.ClassDeclaration (Syntax.fromId $2) $3 $5 }
  | type typeIdentifier ';'
    { Syntax.TypeDeclaration (Syntax.fromId $2) }
  | port identifier portDeclarationType portDeclarationConnection ';'
    { Syntax.PortDeclaration (Syntax.fromId $2) $3 $4 }
  | identifier '=' expression ';'
    { Syntax.Assignment (Syntax.fromId $1) $3 }
  | expressions connection expressions ';'
    { Syntax.PortConnection $1 $2 $3 }

-- Allowing the possibility of no port constraints.
portDeclarationType :: { Syntax.Expression }
  : {- empty -} { Syntax.PortTypeExpression Syntax.portTypeEmpty }
  | ':' expression { $2 }

portDeclarationConnection :: { Maybe (Syntax.Connection,[Syntax.Expression]) }
  : {- empty -} { Nothing }
  | connection expressions { Just ($1,$2) }

-- Parameters to a class declaration.
parameterList :: { [Syntax.Identifier] }
  : {- empty -} { [] }
  | '(' parameter parameters ')' { $2 : $3 }

-- A continuation of a parameter list.
parameters :: { [Syntax.Identifier] }
  : {- empty -} { [] }
  | ',' parameter parameters { $2 : $3 }

parameter :: { Syntax.Identifier }
  : identifier { $1 }

-- A list of Lobster expressions
expressionList :: { [Syntax.Expression] }
  : {- empty -} { [] }
  | '(' expressions ')' { $2 }

expressions :: { [Syntax.Expression] }
  : expression expressionsTail { $1 : $2 }

-- The continuation of a list of Lobster expressions.
expressionsTail :: { [Syntax.Expression] }
  : {- empty -} { [] }
  | ',' expression expressionsTail { $2 : $3 }

-- A single Lobster expression.
expression :: { Syntax.Expression }
  : atomicExpression { $1 }
  | expression '.' atomicExpression { Syntax.DotExpression $1 $3 }

atomicExpression :: { Syntax.Expression }
  : identifier { Syntax.IdentifierExpression $1 }
  | typeIdentifier { Syntax.TypeIdentifierExpression $1 }
  | number { Syntax.IntExpression $1 }
  | quotedString { Syntax.StringExpression $1 }
  | new quotedString typeIdentifier expressionList
    { Syntax.NewExpression $2 (Syntax.fromId $3) $4 }
  | '(' expression ')' { $2 }
  | '{' portTypeConstraintList '}' { Syntax.PortTypeExpression $2 }

polymorphicExpression :: { Syntax.Polymorphic Syntax.Expression }
  : '*' { Syntax.None }
  | expression { Syntax.Some $1 }

-- The port type, consisting of the result of parsing the port constraints.
-- It is illegal to specify the same constraint more than once.
portTypeConstraintList :: { Syntax.PortType }
  : {- empty -} { Syntax.portTypeEmpty }
  | portTypeConstraint portTypeConstraints { Syntax.unifyPortType $1 $2 }

-- The continutation of list of port constraints.
portTypeConstraints :: { Syntax.PortType }
  : {- empty -} { Syntax.portTypeEmpty }
  | ',' portTypeConstraint portTypeConstraints { Syntax.unifyPortType $2 $3 }

-- A single port constraint, specifying the type, input, output and/or position
-- attribute of a port.
portTypeConstraint :: { Syntax.PortType }
  : type '=' polymorphicExpression { Syntax.portTypeType $3 }
  | input '=' polymorphicExpression { Syntax.portTypeInput $3 }
  | output '=' polymorphicExpression { Syntax.portTypeOutput $3 }
  | position '=' polymorphicPositionValue { Syntax.portTypePosition $3 }
  | direction '=' polymorphicDirectionValue { Syntax.portTypeDirection $3 }

-- A position (when specified) can be either a subject or an object.
-- When not specified, a port is polymorphic in position.
positionValue :: { Syntax.Position }
  : subject { Syntax.SubjectPosition }
  | object { Syntax.ObjectPosition }

polymorphicPositionValue :: { Syntax.Polymorphic Syntax.Position }
  : '*' { Syntax.None }
  | positionValue { Syntax.Some $1 }

-- A direction (when specified) can be either an input, output or bidirectional.
-- When not specified, a port is polymorphic in direction.
directionValue :: { Syntax.Direction }
  : input { Syntax.InputDirection }
  | output { Syntax.OutputDirection }
  | bidirectional { Syntax.BidirectionalDirection }

polymorphicDirectionValue :: { Syntax.Polymorphic Syntax.Direction }
  : '*' { Syntax.None }
  | directionValue { Syntax.Some $1 }

{
-- Format a parse error for printing. Apparently parseError takes a list of
-- tokens that gives the context of the error, and you get to figure out what
-- to do with that context.
parseError :: [Lexer.Token] -> a
parseError =
  \tokens ->
    case tokens of
      [] -> error "Parse error at EOF"
      (Lexer.Token p _ : _) ->
          error ("Parse error at " ++ Lexer.posnToString p ++ ":\n" ++
                 show (map strip (List.take 10 tokens)))
  where
    strip (Lexer.Token _ tok) = tok
}
