{

{- |
Module      : $Header$
Description : Parser for Shrimp M4 subset
Copyright   : (c) Galois, Inc.

Parser for Shrimp M4 subset
-}

module SCD.M4.Parser(parseInterface, parseImplementation,
  parseFileContexts, parseClassPermissionDefs, parseSupportDefs,
  parseGlobalBooleans, parseModulesConf, parseIfdefDecls) where

import Text.Happy.ParserMonad(P, parseError, mkParser)
import Control.Monad(ap)
import Control.Monad.Error(throwError)
import Data.Tree(Forest, Tree(..))
import Data.Bits((.&.))
import Data.List(intersperse)
import Data.NonEmptyList(NonEmptyList, singleton, cons, append)
import qualified Data.NonEmptyList as NE
import Data.Foldable(toList)
import Prelude hiding (FilePath)
import Numeric(readHex)
import qualified System.FilePath as FilePath

import SCD.M4.Syntax( Interface(..), InterfaceElement(..),
   InterfaceType(..), Implementation(..), Stmts(..), Require(..),
   Stmt(..), M4Id(..), IfdefId(..), ModuleId(..), MlsRange(..),
   Level(..), XMLDoc(..), RefPolicyWarnLine(..), Version(..),
   GenContext(..), FileContexts(..), FileContext(..), HomePath(..),
   RegexpPath(..), ClassPermissionDefs, ClassPermissionDef(..),
   SupportDefs, SupportDef(..), GlobalBooleans, GlobalBoolean(..),
   BoolType(..), ModuleConf(..), ModuleConfSetting(..), IfdefDecl(..))

import SCD.SELinux.Syntax(Identifier, IsIdentifier(..), mkId, ClassId,
   RoleId, PermissionId, TypeId, AttributeId, TypeOrAttributeId,
   BoolId, CondExpr(..), SignedId(..), NeverAllow(..), Self(..),
   Permissions(..), SourceTarget(..), Transition(..), AllowDeny(..),
   AvRuleBlock(..), AvRule(..), CondExpr(..), Op(..), Constraint(..),
   ConstraintExpr(..), ConstraintExprPrim(..), ContextIndex(..),
   COp(..), CEqOp(..), RoleMlsOp(..), SidContext(..), PortContext(..),
   NetInterfaceContext(..), NodeContext(..), Protocol(..),
   FileSystemUse(..), GenFileSystemContext(..), FilePath(..),
   FileType(..), IPV4Address(..), IPV6Address(..), IPAddressMask(..),
   SecurityContext(..), Transition(..), SourceTarget(..),
   AllowDeny(..), Self(..), NeverAllow(..), SignedId(..), Sign(..),
   User(..), Permissions(..), StarTilde(..), IsIdentifier(..),
   ClassId, PermissionId, TypeId, AttributeId, TypeOrAttributeId, Sid,
   BoolId, UserId, RoleId, NetInterfaceId, FileSystemId)

import SCD.M4.Lexer(Token(..), TokenConstructor(..), scan)

import SCD.SELinux.Parser(fromIds, projectSignedIds, projectSelf,
  memoMkId, mkSourceTarget, checkProtocol, mkPortNumber,
  checkFileType, mkOctet, mkIPV4Address, mkIPV6Address, check)

import qualified SCD.M4.XML as XML
import Text.XML.Light(parseXML)
import qualified Text.ParserCombinators.PolyReadP as PR

#ifndef __HADDOCK__
}

%name pInterface interface_module
%name pImplementation implementation
%name pFileContexts file_contexts
%name pClassPermissionDefs class_permission_defs
%name pSupportDefs support_defs
%name pGlobalBooleans global_booleans
%name pModulesConf modules_conf
%name pIfdefDecls gen_ifdefs

%tokentype { Token }

%error { parseError }

%monad { P } { >>= } { return }

%token
'clone'                         { T _ CLONE }
'common'                        { T _ COMMON }
'class'                         { T _ CLASS }
'constrain'                     { T _ CONSTRAIN }
'validatetrans'                 { T _ VALIDATETRANS }
'inherits'                      { T _ INHERITS }
'sid'                           { T _ SID }
'role'                          { T _ ROLE }
'roles'                         { T _ ROLES }
'types'                         { T _ TYPES }
'typealias'                     { T _ TYPEALIAS }
'typeattribute'                 { T _ TYPEATTRIBUTE }
'type'                          { T _ TYPE }
'bool'                          { T _ BOOL }
'if'                            { T _ IF }
'else'                          { T _ ELSE }
'alias'                         { T _ ALIAS }
'attribute'                     { T _ ATTRIBUTE }
'type_transition'               { T _ TYPE_TRANSITION }
'type_member'                   { T _ TYPE_MEMBER }
'type_change'                   { T _ TYPE_CHANGE }
'role_transition'               { T _ ROLE_TRANSITION }
'range_transition'              { T _ RANGE_TRANSITION }
'sensitivity'                   { T _ SENSITIVITY }
'dominance'                     { T _ DOMINANCE }
'category'                      { T _ CATEGORY }
'level'                         { T _ LEVEL }
'range'                         { T _ RANGE }
'mlsconstrain'                  { T _ MLSCONSTRAIN }
'mlsvalidatetrans'              { T _ MLSVALIDATETRANS }
'neverallow'                    { T _ NEVERALLOW }
'allow'                         { T _ ALLOW }
'auditallow'                    { T _ AUDITALLOW }
'auditdeny'                     { T _ AUDITDENY }
'dontaudit'                     { T _ DONTAUDIT }
'source'                        { T _ SOURCE }
'target'                        { T _ TARGET }
'sameuser'                      { T _ SAMEUSER }
'module'                        { T _ MODULE }
'require'                       { T _ REQUIRE }
'optional'                      { T _ OPTIONAL }
'or'                            { T _ OR }
'and'                           { T _ AND }
'not'                           { T _ NOT }
'xor'                           { T _ XOR }
'eq'                            { T _ EQUALS }
'true'                          { T _ CTRUE } 
'false'                         { T _ CFALSE } 
'self'                          { T _ SELF } 
'dom'                           { T _ DOM }
'domby'                         { T _ DOMBY }
'incomp'                        { T _ INCOMP }
'fscon'                         { T _ FSCON }
'portcon'                       { T _ PORTCON }
'netifcon'                      { T _ NETIFCON }
'nodecon'                       { T _ NODECON }
'fs_use_xattr'                  { T _ FSUSEXATTR }
'fs_use_task'                   { T _ FSUSETASK }
'fs_use_trans'                  { T _ FSUSETRANS }
'genfscon'                      { T _ (GENFSCON $$) }
'r1'                            { T _ R1 }
'r2'                            { T _ R2 }
'r3'                            { T _ R3 }
'u1'                            { T _ U1 }
'u2'                            { T _ U2 }
'u3'                            { T _ U3 }
't1'                            { T _ T1 }
't2'                            { T _ T2 }
't3'                            { T _ T3 }
'l1'                            { T _ L1 }
'l2'                            { T _ L2 }
'h1'                            { T _ H1 }
'h2'                            { T _ H2 }
Path                            { T _ (PATH $$) }
RegexpPath                      { T _ (REGEXP_PATH $$) }
Identifier                      { T _ (IDENTIFIER _) }
Number                          { T _ (NUMBER $$) }
Ipv4addr                        { T _ (IPV4_ADDR $$) }
Ipv6addr                        { T _ (IPV6_ADDR $$) }
Version                         { T _ (VERSION_IDENTIFIER $$) }
'!='                            { T _ NOTEQUAL }
','                             { T _ COMMA }
':'                             { T _ COLON }
';'                             { T _ SEMI }
'('                             { T _ LPAREN } 
')'                             { T _ RPAREN }
'{'                             { T _ LBRACE } 
'}'                             { T _ RBRACE }
'['                             { T _ LBRACK }
'-'                             { T _ MINUS }
'.'                             { T _ PERIOD }
']'                             { T _ RBRACK }
'~'                             { T _ TILDE }
'*'                             { T _ STAR }
-- these are for the Reference Policy (M4) language:
'policy_module'                 { T _ POLICY_MODULE }
'define'                        { T _ DEFINE }
'gen_require'                   { T _ GEN_REQUIRE }
'gen_tunable'                   { T _ GEN_TUNABLE }
'gen_bool'                      { T _ GEN_BOOL }
'gen_ifdef'                     { T _ GEN_IFDEF }
'ifdef'                         { T _ IFDEF }
'ifndef'                        { T _ IFNDEF }
'interface'                     { T _ INTERFACE }
'opt_policy'                    { T _ OPTIONAL_POLICY }
'refpolicywarn'                 { T _ (REFPOLICYWARN $$) }
'template'                      { T _ TEMPLATE }
'tunable_policy'                { T _ TUNABLE_POLICY }
'gen_context'                   { T _ GEN_CONTEXT }
'<<none>>'                      { T _ NONE }
'HOME_DIR'                      { T _ HOME_DIR }
'HOME_ROOT'                     { T _ HOME_ROOT }
"'"                             { T _ TICK }
"`"                             { T _ SQUOTE }
'xmldoc'                        { T _ (XMLDOC $$) }
'='                             { T _ EQUAL }
--

%left 'or'
%left 'xor'
%left 'and'
%right 'not'
%left 'eq' '!='
%%

interface_module        :: { Interface }
interface_module        : xmldocs interface_elements {% return InterfaceModule `ap` 
                                                        xmlParse XML.parseModule $1 `ap` 
                                                        return (reverse $2) }

interface_elements      :: { [InterfaceElement] }
                        : interface_elements interface { $2 : $1 }
                        | interface_elements template  { $2 : $1 }
                        |                              { [] }

interface               :: { InterfaceElement }
                        : xmldocs 'interface' '(' "`" identifier "'" ',' "`" interface_stmts "'" ')' {% return (InterfaceElement InterfaceType) `ap`
                                                                                                        xmlParse XML.parseInterface $1 `ap`
                                                                                                        return (fromId $5) `ap`
                                                                                                        return $9 }

template                :: { InterfaceElement }
                        : xmldocs 'template' '(' "`" identifier "'" ',' "`" template_stmts "'" ')' {% return (InterfaceElement TemplateType) `ap`
                                                                                                      xmlParse XML.parseInterface $1 `ap`
                                                                                                      return (fromId $5) `ap`
                                                                                                      return $9 }

xmldocs                 :: { XMLDoc }
                        : 'xmldoc' { XMLDoc $1 }

template_stmts          :: { Stmts }
                        : interface_stmts { $1 } -- FIXME: capture the difference between template and interface syntax, if there is one.

interface_stmts         :: { Stmts }
interface_stmts         : policy { (reverse $1) }

gen_require             :: { NonEmptyList Require }
                        : 'gen_require' '(' "`" requires "'" ')' { NE.reverse $4 }

requires                :: { NonEmptyList Require }
                        : requires require_decl      { $2 `cons` $1 }
                        | require_decl               { singleton $1 }

require_decl            :: { Require }
                        : 'class' identifier nested_ids ';'                                            { RequireClass (fromId $2) (fromIds $3) }
                        | 'role'        id_comma_list ';'                                              { RequireRole (fromIds (NE.reverse $2)) }
                        | 'type'        id_comma_list ';'                                              { RequireType (fromIds (NE.reverse $2)) }
                        | 'attribute'   id_comma_list ';'                                              { RequireAttribute (fromIds (NE.reverse $2)) }
                        | 'bool'        id_comma_list ';'                                              { RequireBool (fromIds (NE.reverse $2)) }         
                        | 'ifdef' '(' "`" identifier "'" ',' "`" requires "'" ')'                      { RequireIfdef (fromId $4) (NE.reverse $8) [] }
                        | 'ifdef' '(' "`" identifier "'" ',' "`" requires "'" ',' "`" requires "'" ')' { RequireIfdef (fromId $4) (NE.reverse $8) (toList (NE.reverse $12)) }
                        | 'ifndef' '(' "`" identifier "'" ',' "`" requires "'" ')'                     { RequireIfndef (fromId $4) (NE.reverse $8) }

optional_policy         :: { Stmt }
                        : 'opt_policy' '(' "`" interface_stmts "'" ')'                             { Optional $4 [] }
                        | 'opt_policy' '(' "`" interface_stmts "'" ',' "`" interface_stmts "'" ')' { Optional $4 $8 }

tunable_policy          :: { Stmt }
                        : 'tunable_policy' '(' "`" cond_expr "'" ',' "`" interface_stmts "'"  ')'                            { Tunable $4 $8 [] }
                        | 'tunable_policy' '(' "`" cond_expr "'" ',' "`" interface_stmts "'" ',' "`" interface_stmts "'" ')' { Tunable $4 $8 $12 }

ifdef_stms              :: { Stmt }
                        : 'ifdef' '(' "`" identifier "'" ',' "`" interface_stmts "'" ')'                             { Ifdef (fromId $4) $8 [] }
                        | 'ifdef' '(' "`" identifier "'" ',' "`" interface_stmts "'" ',' "`" interface_stmts "'" ')' { Ifdef (fromId $4) $8 $12 }
                        | 'ifndef' '(' "`" identifier "'" ',' "`" interface_stmts "'" ')'                            { Ifndef (fromId $4) $8 }
{-
refpolicywarn_stmts     :: { [[String]] }
                        : refpolicywarn_stmts refpolicywarn_stmt { $2 : $1 }
                        |                                        { [] }

refpolicywarn_stmt      :: { [String] }
                        : nested_ids { $1 }
                        | '('        { ["("] }
                        | ')'        { [")"] }
                        | '.'        { ["."] }
-}
refpolicywarn           :: { Stmt }
                        : 'refpolicywarn' { RefPolicyWarn (RefPolicyWarnLine $1) }


interface_call          :: { Stmt }
                        : identifier '(' set_comma_list ')' { Call (fromId $1) (reverse $3) }

set_comma_list          :: { [NonEmptyList (SignedId Identifier)] }
set_comma_list          : nested_signed_list                    { [$1] }
                        | set_comma_list ',' nested_signed_list { $3 : $1 }
                        |                                       { [] }

policy                  :: { [Stmt] }
policy                  : policy policy_stmt { $2 : $1 }
                        |                    { [] }
                        | policy ';'         { $1 }

policy_stmt             :: { Stmt }
                        : te_rbac_decl    { $1 }
                        | interface_call  { $1 }
                        | refpolicywarn   { $1 }
                        | optional_policy { $1 }
                        | tunable_policy  { $1 }
                        | ifdef_stms      { $1 }
                        | conditional     { $1 }
                        | xmldocs          { XMLDocStmt $1 }


te_rbac_decl            :: { Stmt }
                        : te_decl         { $1 }
                        | rbac_decl       { $1 }

rbac_decl               :: { Stmt }
                        : 'role' identifier 'types' nested_signed_ts ';'               { Role (fromId $2) (toList $4) }
                        | 'role' identifier ';'                                        { Role (fromId $2) [] }
                        | 'role_transition' nested_ids nested_signed_ts identifier ';' { RoleTransition (fromIds $2) $3 (fromId $4) }
-- this would introduce reduce/reduce conflicts with the TeAvTab Allow case:
--                      | 'allow' nested_ids nested_ids ';'                         { RoleAllow (fromIds $2) (fromIds $3) }
                        | 'allow' source_types target_types ';'                     {% return RoleAllow `ap` projectSignedIds (return . fromId . toId) $2
                                                                                                        `ap` projectSignedIds projectSelf $3 }

te_decl                 :: { Stmt }
                        : 'attribute' identifier ';'                                                       { Attribute (fromId $2) }
                        | 'type' identifier alias_def opt_attr_list ';'                                    { Type (fromId $2) (toList $3) $4 }
                        | 'type' identifier opt_attr_list ';'                                              { Type (fromId $2) [] $3 }
                        | 'typealias' identifier alias_def ';'                                             { TypeAlias (fromId $2) $3 }
                        | 'typeattribute' identifier id_comma_list ';'                                     { TypeAttribute (fromId $2) (fromIds (NE.reverse $3)) }
                        | 'range_transition' nested_signed nested_signed mls_range_def ';'                 { RangeTransition (fmap fromIds $2) (fmap fromIds $3) (singleton (mkId "process")) $4 }
                        | 'range_transition' nested_signed nested_signed ':' nested_ids mls_range_def ';'  { RangeTransition (fmap fromIds $2) (fmap fromIds $3) (fromIds $5) $6 }
                        | 'neverallow' na_source_types na_target_types ':' nested_ids permissions  ';'     { TeNeverAllow (mkSourceTarget $2 $3 $5) $6 }
                        | stmt                                                                             { $1 }

mls_range_def           :: { MlsRange }
                        : mls_level_def '-' mls_level_def { MlsRange $1 $3 }
                        | mls_level_def                   { MlsRange $1 $1 }

mls_level_def           :: { Level }
mls_level_def           : identifier { fromId $1 }

alias_def               :: { NonEmptyList TypeId }
                        : 'alias' nested_ids { fromIds $2 }

stmt                    :: { Stmt }
                        : 'type_transition' source_types source_types ':' nested_ids identifier ';'    { Transition TypeTransition (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'type_member'     source_types source_types ':' nested_ids identifier ';'    { Transition TypeMember     (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'type_change'     source_types source_types ':' nested_ids identifier ';'    { Transition TypeChange     (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'allow'      source_types target_types ':' nested_ids permissions  ';'       { TeAvTab Allow (mkSourceTarget $2 $3 $5) $6 }
                        | 'auditallow' source_types target_types ':' nested_ids permissions ';'        { TeAvTab AuditAllow (mkSourceTarget $2 $3 $5) $6 }
                        | 'auditdeny'  source_types target_types ':' nested_ids permissions ';'        { TeAvTab AuditDeny (mkSourceTarget $2 $3 $5) $6 }
                        | 'dontaudit'  source_types target_types ':' nested_ids permissions ';'        { TeAvTab DontAudit (mkSourceTarget $2 $3 $5) $6 }
                        | initial_sid_context_def                                                      { SidStmt $1 }
                        | fs_use_def                                                                   { FileSystemUseStmt $1 }
                        | genfs_context_def                                                            { GenFileSystemStmt $1 }
                        | port_context_def                                                             { PortStmt $1 }
                        | netif_context_def                                                            { NetInterfaceStmt $1 }
                        | node_context_def                                                             { NodeStmt $1 }
                        | 'define' '(' "`" identifier "'" ')'                                          { Define (fromId $4) }
                        | gen_require                                                                  { Require $1 }
                        | gen_boolean                                                                  { let (t,i,b) = $1 in GenBoolean t i b }


opt_attr_list           :: { [AttributeId] }
                        : ',' id_comma_list { fromIds (reverse (toList $2)) }
                        |                   { [] }

bool_val                :: { Bool }
                        : 'true'  { True }
                        | 'false' { False }

conditional             :: { Stmt }
                        : 'if' cond_expr '{' interface_stmts '}' cond_else { CondStmt $2 $4 $6 }

cond_else               :: { Stmts }
                        : 'else' '{' interface_stmts '}' { $3 }
                        |                                { [] }
cond_expr               :: { CondExpr }
                        : '(' cond_expr ')'         { $2 }
                        | 'not' cond_expr           { Not $2 }
                        | cond_expr 'and' cond_expr { Op $1 And $3 }
                        | cond_expr 'or' cond_expr  { Op $1 Or $3 }
                        | cond_expr 'xor' cond_expr { Op $1 Xor $3 }
                        | cond_expr 'eq' cond_expr  { Op $1 Equals $3 }
                        | cond_expr '!=' cond_expr  { Op $1 Notequal $3 }
                        | identifier                { Var (fromId $1) }


initial_sid_context_def :: { SidContext GenContext }
                        :  'sid' identifier security_context_def       { SidContext (fromId $2) $3 }

port_context_def        :: { PortContext GenContext }
                        : 'portcon' identifier number security_context_def            {% return PortContext `ap` checkProtocol $2 
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return $4 }
                        | 'portcon' identifier number '-' number security_context_def {% return PortContext `ap` checkProtocol $2 
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return (mkPortNumber $5)
                                                                                                            `ap` return $6 }

netif_context_def       :: { NetInterfaceContext GenContext }
                        : 'netifcon' identifier security_context_def security_context_def { NetInterfaceContext (fromId $2) $3 $4 }

node_context_def        :: { NodeContext GenContext }
                        : 'nodecon' ipv4_addr ipv4_addr security_context_def { NodeContext (IPV4AddrMask $2 $3) $4 }
                        | 'nodecon' ipv6_addr ipv6_addr security_context_def { NodeContext (IPV6AddrMask $2 $3) $4 }

fs_use_def              :: { FileSystemUse GenContext }
                        : 'fs_use_xattr' identifier security_context_def ';' { FSUseXattr (fromId $2) $3 }
                        | 'fs_use_task' identifier security_context_def ';'  { FSUseTask (fromId $2) $3 }
                        | 'fs_use_trans' identifier security_context_def ';' { FSUseTrans (fromId $2) $3 }

genfs_context_def       :: { GenFileSystemContext GenContext }
                        : 'genfscon' path '-' identifier security_context_def {% return (GenFSCon (mkId $1) $2) `ap` fmap Just (checkFileType $4) `ap` return $5 }
                        | 'genfscon' path '-' '-' security_context_def        { GenFSCon (mkId $1) $2 (Just PlainFile) $5 }
                        | 'genfscon' path security_context_def                { GenFSCon (mkId $1) $2 Nothing $3 }

path                    :: { FilePath } 
                        : Path       { FilePath $1 }

number                  :: { Integer } 
                        : Number     { $1 } 

ipv4_addr               :: { IPV4Address }
                        : Ipv4addr  { mkIPV4Address $1 }

ipv6_addr               :: { IPV6Address }
                        : Ipv6addr  { mkIPV6Address $1 }

security_context_def    :: { GenContext }
                        : 'gen_context' '(' identifier ':' identifier ':' identifier ',' mls_range_def ')' { GenContext (fromId $3) (fromId $5) (fromId $7) $9 }

id_comma_list           :: { NonEmptyList Identifier }
                        : identifier                   { singleton $1 }
                        | id_comma_list ',' identifier { $3 `cons` $1 }

tilde                   : '~' { () }

asterisk                : '*' { () }

source_types            :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : identifier '-' identifier { SignedId Positive (fromId $1) `cons` singleton (SignedId Negative (fromId $3)) }
                        | nested_signed_ts          { $1 }

target_types            :: { NonEmptyList (SignedId Self) }
                        : self '-' self      { SignedId Positive $1 `cons` singleton (SignedId Negative $3) }
                        | nested_self_signed { $1 }

self                    :: { Self }
                        : identifier { NotSelf (fromId $1) }
                        | 'self'     { Self }

na_source_types         :: { NeverAllow TypeOrAttributeId }
                        : asterisk               { NAStarTilde Star }
                        | tilde nested_signed_ts { NAStarTilde (Tilde $2) }
                        | source_types           { NeverAllow $1 }

na_target_types         :: { NeverAllow Self }
                        : asterisk            { NAStarTilde Star }
                        | tilde nested_self_signed { NAStarTilde (Tilde $2) }
                        | target_types { NeverAllow $1 }

permissions             :: { Permissions }
                        : nested_ids       { Permissions (fromIds $1) }
                        | asterisk         { PStarTilde Star }
                        | tilde nested_ids { PStarTilde (Tilde (fromIds $2)) }

identifier_list         :: { NonEmptyList Identifier }
                        : identifier                 { singleton (fromId $1) }
                        | identifier_list identifier { fromId $2 `cons` $1 }

nested_signed_ts        :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : nested_signed { fmap fromIds $1 }

nested_signed_set       :: { NonEmptyList (SignedId Identifier) }
                        : '{' nested_signed_list '}' { $2 }

nested_signed_list      :: { NonEmptyList (SignedId Identifier) }
                        : nested_signed_element                    { $1 }
                        | nested_signed_list nested_signed_element { $1 `append` $2 }

nested_signed           :: { NonEmptyList (SignedId Identifier) }
                        : identifier        { singleton (SignedId Positive (fromId $1)) }
                        | nested_signed_set { $1 }

nested_signed_element   :: { NonEmptyList (SignedId Identifier) }
                        : '-' identifier { singleton (SignedId Negative (fromId $2)) }
                        | nested_signed  { $1 }

-- same for identifiers+self

nested_self_signed_set  :: { NonEmptyList (SignedId Self) }
                        : '{' nested_self_signed_list '}' { $2 }

nested_self_signed_list :: { NonEmptyList (SignedId Self) }
                        : nested_self_signed_element                    { $1 }
                        | nested_self_signed_list nested_self_signed_element { $1 `append` $2 }

nested_self_signed      :: { NonEmptyList (SignedId Self) }
                        : self              { singleton (SignedId Positive $1) }
                        | nested_self_signed_set { $1 }

nested_self_signed_element   :: { NonEmptyList (SignedId Self) }
                        : '-' self { singleton (SignedId Negative $2) }
                        | nested_self_signed  { $1 }

nested_id_set           :: { NonEmptyList Identifier }
                        : '{' nested_id_list '}' { $2 }

nested_id_list          :: { NonEmptyList Identifier }
                        : nested_ids                { $1 }
                        | nested_id_list nested_ids { $1 `append` $2 }

nested_ids              :: { NonEmptyList Identifier }
                        : identifier    { singleton $1 }
                        | nested_id_set { $1 } 



{- Unfortunately, this doesn't work.  Maybe because we would need
   type signatures on the generated (recursive) definitions, and happy
   doesn't insert that.
identifier              :: { IsIdentifier i => i }
                        : Identifier { fromId $1 }
-}
identifier              :: { Identifier }
                        : Identifier {% strictMkId $1 }

{- This doesn't work since happy generates code that unsafeCoerces
    things into the overloaded type in a way that is ambiguous.

identifier2             :: { (forall a. IsIdentifier a => a) }
                        : Identifier { fromId $1 }
-}

-------------- implementation parser

implementation   :: { Implementation }
                 : 'policy_module' '(' identifier ',' version ')' implementation_stmts { Implementation (fromId $3) $5 $7 }

implementation_stmts    :: { Stmts }
                        : interface_stmts { $1 } -- FIXME: capture the difference between implementation and interface syntax, if there is one.

version          :: { Version }
                 : Version { Version $1 }

-------------- file-context configuration parser

file_contexts   :: { FileContexts }
file_contexts   : file_context_defs { FileContexts (reverse $1) }

file_context_defs :: { [FileContext] }
                  : file_context_defs file_context_def { $2 : $1 }
                  |                                    { [] }

file_context_def  :: { FileContext }
                        : homepath '-' identifier msecurity_context_def {% return (FileContext $1) `ap` fmap Just (checkFileType $3) `ap` return $4 }
                        | homepath '-' '-' msecurity_context_def        { FileContext $1 (Just PlainFile) $4 }
                        | homepath msecurity_context_def                { FileContext $1 Nothing $2 }
                        | 'ifdef' '(' "`" identifier "'" ',' "`" file_context_defs "'" ')'                               { FileContextIfdef (fromId $4) (reverse $8) [] }
                        | 'ifdef' '(' "`" identifier "'" ',' "`" file_context_defs "'" ',' "`" file_context_defs "'" ')' { FileContextIfdef (fromId $4) (reverse $8) (reverse $12) }
                        | 'ifndef' '(' "`" identifier "'" ',' "`" file_context_defs "'" ')'                              { FileContextIfndef (fromId $4) (reverse $8) }


msecurity_context_def :: { Maybe GenContext }
                      : security_context_def { Just $1 }
                      | '<<none>>'           { Nothing }


homepath          :: { HomePath }
homepath          : 'HOME_DIR' rpath  { HomeDir $2 }
                  | 'HOME_DIR'        { HomeDir (PlainPath (FilePath "")) }
                  | 'HOME_ROOT' rpath { HomeRoot $2 }
                  | 'HOME_ROOT'       { HomeRoot (PlainPath (FilePath "")) }
                  | rpath             { Path $1 }

rpath             :: { RegexpPath }
rpath             : path { PlainPath $1 }
                  | RegexpPath { RegexpPath $1 }

-------------- class/permission set definitions

class_permission_defs :: { ClassPermissionDefs }
                      : class_permission_macros { reverse $1 }

class_permission_macros :: { ClassPermissionDefs }
                        : class_permission_macros class_permission_macro { $2 : $1 }
                        |                                                { [] }

class_permission_macro :: { ClassPermissionDef }
class_permission_macro : 'define' '(' "`" identifier "'" ',' "`" nested_id_set opt_deprecated_cp_macro "'" ')' { ClassPermissionDef (fromId $4) $8 $9 }

opt_deprecated_cp_macro :: { Maybe RefPolicyWarnLine }
opt_deprecated_cp_macro : 'refpolicywarn' { Just (RefPolicyWarnLine $1) }
                        |                 { Nothing }

-------------- support definitions

support_defs :: { SupportDefs }
             : support_macros { reverse $1 }

support_macros :: { SupportDefs }
               : support_macros support_macro { $2 : $1 }
               |                              { [] }

support_macro :: { SupportDef }
              : 'define' '(' "`" identifier "'" ',' "`" policy "'" ')' { SupportDef (fromId $4) $8 }

-------------- global booleans

global_booleans :: { GlobalBooleans }
                : global_boolean_macros { reverse $1 }

global_boolean_macros :: { GlobalBooleans }
                      : global_boolean_macros global_boolean_macro { $2 : $1 }
                      |                                            { [] }

global_boolean_macro :: { GlobalBoolean }
              : xmldocs  gen_boolean { let (t,i,b) = $2 in GlobalBoolean t (Just $1) i b }
              | gen_boolean          { let (t,i,b) = $1 in GlobalBoolean t Nothing i b }

gen_boolean          :: { (BoolType, BoolId, Bool) }
                     : 'gen_bool' '(' identifier ',' bool_val ')'     { (BoolType, fromId $3, $5) }
                     | 'gen_tunable' '(' identifier ',' bool_val ')'  { (TunableType, fromId $3, $5) }

-------------- ifdef declarations

gen_ifdefs           :: { [IfdefDecl] }
                     : gen_ifdefs_macros    { reverse $1 }

gen_ifdefs_macros    :: { [IfdefDecl] }
                     : gen_ifdefs_macros gen_ifdef_macro { $2 : $1 }
                     |                                   { [] }

gen_ifdef_macro      :: { IfdefDecl }
gen_ifdef_macro      : 'gen_ifdef' '(' identifier ',' bool_val ')' { IfdefDecl (fromId $3) (Just $5) }
                     | 'gen_ifdef' '(' identifier ')'              { IfdefDecl (fromId $3) Nothing }

-------------- module configuration

modules_conf :: { [ModuleConf] }
modules_conf : modules_conf module_conf { $2 : $1 }
             |                          { [] }

module_conf  :: { ModuleConf }
module_conf  : identifier '=' identifier {% return ModuleConf `ap` return (fromId $1) `ap` checkModuleConfSetting $3 }
             | identifier '=' 'module'   { ModuleConf (fromId $1) Module }


{
#endif

strictMkId :: Token -> P Identifier
strictMkId (T p (IDENTIFIER s)) = i == i `seq` return i
  where i = memoMkId p s
strictMkId _ = error "strictMkId"

checkModuleConfSetting :: Identifier -> P ModuleConfSetting
checkModuleConfSetting = check "Illegal module configuration" [("base", Base), ("off", Off)]

parseInterface :: FilePath.FilePath -> String -> Either String Interface
parseInterface f = mkParser (pInterface . scan f)

parseImplementation :: FilePath.FilePath -> String -> Either String Implementation
parseImplementation f = mkParser (pImplementation . scan f)

parseFileContexts :: FilePath.FilePath -> String -> Either String FileContexts
parseFileContexts f = mkParser (pFileContexts . scan f)

parseClassPermissionDefs :: FilePath.FilePath -> String -> Either String ClassPermissionDefs
parseClassPermissionDefs f = mkParser (pClassPermissionDefs . scan f)

parseSupportDefs :: FilePath.FilePath -> String -> Either String SupportDefs
parseSupportDefs f = mkParser (pSupportDefs . scan f)

parseGlobalBooleans :: FilePath.FilePath -> String -> Either String GlobalBooleans
parseGlobalBooleans f = mkParser (pGlobalBooleans . scan f)

parseModulesConf :: FilePath.FilePath -> String -> Either String [ModuleConf]
parseModulesConf f = mkParser (pModulesConf . scan f)

parseIfdefDecls :: FilePath.FilePath -> String -> Either String [IfdefDecl]
parseIfdefDecls f = mkParser (pIfdefDecls . scan f)

xmlParse :: Show a => XML.P a -> XMLDoc -> P a
xmlParse p (XMLDoc s) = either (throwError . (("xmlParse: "++s++"\n")++)) return 
                         (PR.parse p (parseXML (XML.extract s)))

}
