{

{- |
Module      : $Header$
Description : Lexer for Shrimp M4 subset
Copyright   : (c) Galois, Inc.

Lexer for Shrimp M4 subset
-}

module SCD.M4.Lexer(
   Token(..)
 , TokenConstructor(..)
 , scan
 ) where
import Data.Generics(Typeable,Data)
import Data.List(isInfixOf)
import Text.Happy.ParserMonad(Pos(..))
import Text.Regex(matchRegex, mkRegex)
}

%wrapper "posn"

$letter  = [A-Za-z]
$digit   = [0-9]
$hexval  = [0-9A-Fa-f]

:-

\n ;
-- TODO: clean up unused tokens (both here and in SELinux/Lexer.x
clone				{ ctoken CLONE }
common				{ ctoken COMMON }
class				{ ctoken CLASS }
constrain			{ ctoken CONSTRAIN }
validatetrans			{ ctoken VALIDATETRANS }
inherits			{ ctoken INHERITS }
sid				{ ctoken SID }
role				{ ctoken ROLE }
roles				{ ctoken ROLES }
types				{ ctoken TYPES }
typealias			{ ctoken TYPEALIAS }
typeattribute			{ ctoken TYPEATTRIBUTE }
type				{ ctoken TYPE }
bool                            { ctoken BOOL }
if				{ ctoken IF }
else				{ ctoken ELSE }
alias				{ ctoken ALIAS }
attribute			{ ctoken ATTRIBUTE }
type_transition			{ ctoken TYPE_TRANSITION }
type_member			{ ctoken TYPE_MEMBER }
type_change			{ ctoken TYPE_CHANGE }
role_transition			{ ctoken ROLE_TRANSITION }
range_transition		{ ctoken RANGE_TRANSITION }
sensitivity			{ ctoken SENSITIVITY }
dominance			{ ctoken DOMINANCE }
category			{ ctoken CATEGORY }
level				{ ctoken LEVEL }
range				{ ctoken RANGE }
mlsconstrain			{ ctoken MLSCONSTRAIN }
mlsvalidatetrans		{ ctoken MLSVALIDATETRANS }
neverallow		        { ctoken NEVERALLOW }
allow			        { ctoken ALLOW }
auditallow		        { ctoken AUDITALLOW }
auditdeny		        { ctoken AUDITDENY }
dontaudit                       { ctoken DONTAUDIT }
source			        { ctoken SOURCE }
target			        { ctoken TARGET }
sameuser			{ ctoken SAMEUSER }
module                          { ctoken MODULE }
require                         { ctoken REQUIRE }
optional                        { ctoken OPTIONAL }
or     			        { ctoken OR }
and				{ ctoken AND }
not				{ ctoken NOT }
xor                             { ctoken XOR }
eq				{ ctoken EQUALS }
true                            { ctoken CTRUE } 
false                           { ctoken CFALSE } 
self                            { ctoken SELF }
dom				{ ctoken DOM }
domby				{ ctoken DOMBY }
incomp				{ ctoken INCOMP }
fscon                           { ctoken FSCON }
portcon				{ ctoken PORTCON }
netifcon			{ ctoken NETIFCON }
nodecon				{ ctoken NODECON }
fs_use_xattr			{ ctoken FSUSEXATTR }
fs_use_task                     { ctoken FSUSETASK }
fs_use_trans                    { ctoken FSUSETRANS }
r1				{ ctoken R1 }
r2				{ ctoken R2 }
r3				{ ctoken R3 }
u1				{ ctoken U1 }
u2				{ ctoken U2 }
u3				{ ctoken U3 }
t1				{ ctoken T1 }
t2				{ ctoken T2 }
t3				{ ctoken T3 }
l1				{ ctoken L1 }
l2				{ ctoken L2 }
h1				{ ctoken H1 }
h2				{ ctoken H2 }
-- these are for the Reference Policy (M4) language:
dnl[^\n]*                       ;
define                          { ctoken DEFINE }
policy_module                   { ctoken POLICY_MODULE }
gen_require                     { ctoken GEN_REQUIRE }
gen_tunable                     { ctoken GEN_TUNABLE }
gen_bool                        { ctoken GEN_BOOL }
gen_ifdef                       { ctoken GEN_IFDEF }
ifdef                           { ctoken IFDEF }
ifndef                          { ctoken IFNDEF }
interface                       { ctoken INTERFACE }
optional_policy                 { ctoken OPTIONAL_POLICY }
template                        { ctoken TEMPLATE }
tunable_policy                  { ctoken TUNABLE_POLICY }
gen_context                     { ctoken GEN_CONTEXT }
"<<none>>"                      { ctoken NONE }
HOME_DIR                        { ctoken HOME_DIR }
HOME_ROOT                       { ctoken HOME_ROOT }
refpolicywarn[\ \t]*"("[\ \t]*`[^']*'[\ \t]*")" { atoken (REFPOLICYWARN . maybe "Lexer error" head . matchRegex (mkRegex "`(.*)'[ \\t]*\\)$")) }
"'"                             { ctoken TICK }
"`"                             { ctoken SQUOTE }
"##"([\ \t][^\n]*\n|\n)("##"([\ \t][^\n]*\n|\n))* { atoken XMLDOC }
--
"/"($letter|$digit|_|"."|"-"|"/"|":")* { atoken PATH }
"/"($letter|$digit|_|"."|"-"|"/"|":"|[\,\|\*\+\?\{\}\(\)\[\]\\\^\`\'])* { atoken REGEXP_PATH }
-- scan concatenated strings and M4 variables as one token to avoid mixed-in whitespace
($letter|"$"([0-9]|"*"))($letter|"$"([0-9]|"*")|$digit|_|".")* { atoken identifier }

-- hack to allow identifiers after genfscon to include minus characters
genfscon[\ \t]+($letter|"$"([0-9]|"*"))($letter|"$"([0-9]|"*")|$digit|_|"."|"-")* { atoken (GENFSCON . dropWhile (`elem` " \t") . drop (length "genfscon")) }

--"$"([0-9]|"*")                { atoken M4VARIABLE }
$digit$digit*                   { atoken (NUMBER . read) }
$hexval{0,4}":"$hexval{0,4}":"($hexval|":"|".")* { atoken IPV6_ADDR }
$digit+("."$digit+){3}          { atoken IPV4_ADDR }
($letter|$digit)+("."($letter|$digit)+){1,2} { atoken VERSION_IDENTIFIER }
"#"[^\n]*                       ;
[\ \t\f]+			;
"==" 				{ ctoken EQUALS }
"!="				{ ctoken NOTEQUAL }
"&&"				{ ctoken AND }
"||"				{ ctoken OR }
"!"				{ ctoken NOT }
"^"                             { ctoken XOR }
","                             { ctoken COMMA }
":"                             { ctoken COLON }
";"                             { ctoken SEMI }
"("                             { ctoken LPAREN } 
")"                             { ctoken RPAREN }
"{"                             { ctoken LBRACE } 
"}"                             { ctoken RBRACE }
"["                             { ctoken LBRACK }
"-"                             { ctoken MINUS }
"."                             { ctoken PERIOD }
"]"                             { ctoken RBRACK }
"~"                             { ctoken TILDE }
"*"				{ ctoken STAR }
"="				{ ctoken EQUAL }
.                               { ctoken (Error "Unrecognized character") }

{
data Token = T Pos TokenConstructor
 deriving (Show, Eq)
data TokenConstructor =
   CLONE
 | COMMON
 | CLASS
 | CONSTRAIN
 | VALIDATETRANS
 | INHERITS
 | SID
 | ROLE
 | ROLES
 | TYPES
 | TYPEALIAS
 | TYPEATTRIBUTE
 | TYPE
 | BOOL
 | IF
 | ELSE
 | ALIAS
 | ATTRIBUTE
 | TYPE_TRANSITION
 | TYPE_MEMBER
 | TYPE_CHANGE
 | ROLE_TRANSITION
 | RANGE_TRANSITION
 | SENSITIVITY
 | DOMINANCE
 | CATEGORY
 | LEVEL
 | RANGE
 | MLSCONSTRAIN
 | MLSVALIDATETRANS
 | NEVERALLOW
 | ALLOW
 | AUDITALLOW
 | AUDITDENY
 | DONTAUDIT
 | SOURCE
 | TARGET
 | SAMEUSER
 | MODULE
 | REQUIRE
 | OPTIONAL
 | OR
 | AND
 | NOT
 | XOR
 | EQUALS
 | CTRUE 
 | CFALSE 
 | SELF
 | DOM
 | DOMBY
 | INCOMP
 | FSCON
 | PORTCON
 | NETIFCON
 | NODECON
 | FSUSEXATTR
 | FSUSETASK
 | FSUSETRANS
 | GENFSCON String
 | R1
 | R2
 | R3
 | U1
 | U2
 | U3
 | T1
 | T2
 | T3
 | L1
 | L2
 | H1
 | H2
 | PATH String
 | REGEXP_PATH String
 | IDENTIFIER String
 | NUMBER Integer
 | IPV4_ADDR String
 | IPV6_ADDR String
 | VERSION_IDENTIFIER String
 | NOTEQUAL
 | COMMA
 | COLON
 | SEMI
 | LPAREN
 | RPAREN
 | LBRACE
 | RBRACE
 | LBRACK
 | MINUS
 | PERIOD
 | RBRACK
 | TILDE
 | STAR
-- these are for the Reference Policy (M4) language:
 | DEFINE
 | POLICY_MODULE
 | GEN_REQUIRE
 | GEN_TUNABLE 
 | GEN_BOOL
 | GEN_IFDEF
 | IFDEF
 | IFNDEF
 | INTERFACE
 | OPTIONAL_POLICY
 | TEMPLATE
 | TUNABLE_POLICY
 | GEN_CONTEXT
 | NONE
 | HOME_DIR
 | HOME_ROOT
 | REFPOLICYWARN String
 | TICK
 | SQUOTE
 | XMLDOC String
 | M4VARIABLE String
 | EQUAL
--
 | Error String
 deriving (Read, Show, Eq, Ord)

type Action = AlexPosn -> String -> Token

atoken :: (String -> TokenConstructor) -> Action
atoken c p s = T (Pos "" a li co) (c s)
  where AlexPn a li co = p

ctoken :: TokenConstructor -> Action
ctoken c = atoken (const c)

identifier :: String -> TokenConstructor
identifier s | last s == '.'      = Error "Identifier cannot end with '.'"
             | ".." `isInfixOf` s = Error "Identifier cannot contain '..'"
             | otherwise          = IDENTIFIER s

scan :: FilePath -> String -> [Token]
scan f s = [T (Pos f a li co) t | T (Pos _ a li co) t <- alexScanTokens s]
}
