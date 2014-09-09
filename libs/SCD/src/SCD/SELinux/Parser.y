{ 

{- |
Module      :  $Header$
Description :  Parser for SELinux policies
Copyright   :  (c) Galois, Inc.
License     :  see the file LICENSE

Maintainer  :  Joe Hurd
Stability   :  provisional
Portability :  portable

Parser for SELinux policies
-}

module SCD.SELinux.Parser(AccessVectors, parsePolicy, parseAccessVectors,
  parseClasses, parseInitialSids, fromIds, projectSignedIds,
  projectSelf, memoMkId, mkSourceTarget, checkProtocol,
  mkPortNumber, checkFileType, mkOctet, mkIPV4Address, mkIPV6Address, check)
  where

import Text.Happy.ParserMonad(P, parseError, mkParser, Pos)
import Control.Monad(ap)
import Control.Monad.Error(throwError)
import Data.Tree(Forest, Tree(..))
import Data.Bits((.&.))
import Data.Word(Word8, Word16)
import Data.NonEmptyList(NonEmptyList, singleton, cons, append)
import Data.Foldable(toList)
import Data.List(groupBy)
import qualified Data.NonEmptyList as NE
import Prelude hiding (FilePath, mapM)
import qualified System.FilePath as FilePath
import Data.Traversable(mapM)
import Numeric(readHex)
--import Data.Memo(memo)

import SCD.SELinux.Syntax( Policy(..), CommonPerm(..), AvPerm(..),
      TeRbac(..), Stmt(..), AvRuleBlock(..), AvRule(..),
      RequireStmt(..), Require(..), CondExpr(..), Op(..),
      Constraint(..), ConstraintExpr(..), ConstraintExprPrim(..),
      ContextIndex(..), COp(..), CEqOp(..), RoleMlsOp(..),
      SidContext(..), PortContext(..), NetInterfaceContext(..),
      NodeContext(..), Protocol(..), FileSystemUse(..),
      GenFileSystemContext(..), FilePath(..), FileType(..),
      IPV4Address(..), IPV6Address(..), IPAddressMask(..),
      SecurityContext(..), Transition(..), SourceTarget(..),
      AllowDeny(..), Self(..), NeverAllow(..), SignedId(..), Sign(..),
      User(..), Permissions(..), StarTilde(..), Identifier,
      IsIdentifier(..), ClassId, PermissionId, TypeId, AttributeId,
      TypeOrAttributeId, Sid, BoolId, UserId, RoleId, NetInterfaceId,
      FileSystemId)

import SCD.SELinux.Lexer(Token(..), TokenConstructor(..), scan)

}

%name pPolicy policy
%name pInitialSids initial_sids
%name pAccessVectors access_vectors
%name pClasses opt_classes

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
'user'                          { T _ USER }
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
'genfscon'                      { T _ GENFSCON }
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

%left 'or'
%left 'xor'
%left 'and'
%right 'not'
%left 'eq' '!='
%%

policy                  :: { Policy }
                        : base_policy { $1 }
                        {- | module_policy -}

base_policy             :: { Policy }
                        : classes initial_sids access_vectors
                          {- opt_mls -} te_rbac users opt_constraints
                          initial_sid_contexts {-opt_fs_contexts-} opt_fs_uses opt_genfs_contexts net_contexts
                          { let (cp,av) = $3; (pc,nic,nodes) = $10
                            in Policy{ classes      = NE.reverse $1
                                     , initialSids  = NE.reverse $2
                                     , commonPerms  = cp
                                     , avPerms      = av
                                     , teRbacs      = reverse $4
                                     , users        = NE.reverse $5
                                     , constraints  = $6
                                     , sidContexts  = NE.reverse $7
                                     , fileSystemUses = $8
                                     , genFileSystemContexts = $9
                                     , portContexts = pc
                                     , netInterfaceContexts = nic
                                     , nodeContexts = nodes
                                     }
                          }

opt_classes             :: { [ClassId] }
                        : classes         { toList $1 }
                        |                 { [] }

classes                 :: { NonEmptyList ClassId }
                        : class_def         { singleton $1 }
                        | classes class_def { $2 `cons` $1 }

class_def               :: { ClassId }
                        : 'class' identifier { fromId $2 }

initial_sids            :: { NonEmptyList Sid }
                        : initial_sid_def                         { singleton $1 }
                        | initial_sids initial_sid_def            { $2 `cons` $1 }

initial_sid_def         :: { Sid }
                        : 'sid' identifier                        { fromId $2 }

access_vectors          :: { AccessVectors }
                        : opt_common_perms av_perms { ($1,NE.reverse $2) }

opt_common_perms        :: { [CommonPerm] }
                        : common_perms     { reverse (toList $1) }
                        |                  { [] }

common_perms            :: { NonEmptyList CommonPerm }
                        : common_perms_def               { singleton $1 }
                        | common_perms common_perms_def  { $2 `cons` $1 }

common_perms_def        :: { CommonPerm }
                        : 'common' identifier '{' identifier_list '}' { CommonPerm (fromId $2) (fromIds (NE.reverse $4)) }

av_perms                :: { NonEmptyList AvPerm }
                        : av_perms_def           { singleton $1 }
                        | av_perms av_perms_def  { $2 `cons` $1 }

av_perms_def            :: { AvPerm }
                        : 'class' identifier '{' identifier_list '}'  { AvPermClass (fromId $2) (Left (fromIds (NE.reverse $4))) }
                        | 'class' identifier 'inherits' identifier    { AvPermClass (fromId $2) (Right (fromId $4,[])) }
                        | 'class' identifier 'inherits' identifier 
                          '{' identifier_list '}'                     { AvPermClass (fromId $2) (Right (fromId $4,fromIds (reverse (toList $6)))) }
{-
opt_mls                 : mls
                        | 
                        ;
mls                     : sensitivities dominance opt_categories levels mlspolicy
                        ;
sensitivities           : sensitivity_def 
                        | sensitivities sensitivity_def
                        ;
sensitivity_def         : SENSITIVITY identifier alias_def ';'
                        {if (define_sens()) return -1;}
                        | SENSITIVITY identifier ';'
                        {if (define_sens()) return -1;}
                        ;
-}
alias_def               :: { NonEmptyList TypeId }
                        : 'alias' nested_ids { fromIds $2 }
{-
                        ;
dominance               : DOMINANCE identifier 
                        {if (define_dominance()) return -1;}
                        | DOMINANCE '{' identifier_list '}' 
                        {if (define_dominance()) return -1;}
                        ;
opt_categories          : categories
                        |
                        ;
categories              : category_def 
                        | categories category_def
                        ;
category_def            : CATEGORY identifier alias_def ';'
                        {if (define_category()) return -1;}
                        | CATEGORY identifier ';'
                        {if (define_category()) return -1;}
                        ;
levels                  : level_def 
                        | levels level_def
                        ;
level_def               : LEVEL identifier ':' id_comma_list ';'
                        {if (define_level()) return -1;}
                        | LEVEL identifier ';' 
                        {if (define_level()) return -1;}
                        ;
mlspolicy               : mlspolicy_decl
                        | mlspolicy mlspolicy_decl
                        ;
mlspolicy_decl          : mlsconstraint_def
                        | mlsvalidatetrans_def
                        ;
mlsconstraint_def       : MLSCONSTRAIN names names cexpr ';'
                        { if (define_constraint((constraint_expr_t*)$4)) return -1; }
                        ;
mlsvalidatetrans_def    : MLSVALIDATETRANS names cexpr ';'
                        { if (define_validatetrans((constraint_expr_t*)$3)) return -1; }
                        ;
-}

te_rbac                 :: { [TeRbac] }
                        : te_rbac_decl         { [$1] }
                        | ';'                  { [] }
                        | te_rbac te_rbac_decl { $2 : $1 }
                        | te_rbac ';'          { $1 }

te_rbac_decl            :: { TeRbac }
                        : te_decl         { $1 }
                        | rbac_decl       { $1 }
                        | cond_stmt_def   { $1 }
                        | optional_block  { $1 }

rbac_decl               :: { TeRbac }
                        : 'role' identifier 'types' nested_signed ';'               { Role (fromId $2) (toList $4) }
                        | 'role' identifier ';'                                     { Role (fromId $2) [] }
                        | 'dominance' '{' roles '}'                                 { Dominance (NE.reverse $3) }
                        | 'role_transition' nested_ids nested_signed identifier ';' { RoleTransition (fromIds $2) $3 (fromId $4) }
-- this would introduce reduce/reduce conflicts with the TeAvTab Allow case:
--                      | 'allow' nested_ids nested_ids ';'                         { RoleAllow (fromIds $2) (fromIds $3) }
                        | 'allow' source_types target_types ';'                     {% return RoleAllow `ap` projectSignedIds (return . fromId . toId) $2
                                                                                                        `ap` projectSignedIds projectSelf $3 }

te_decl                 :: { TeRbac }
                        : 'attribute' identifier ';'                                                   { Attribute (fromId $2) }
                        | 'type' identifier alias_def opt_attr_list ';'                                { Type (fromId $2) (toList $3) $4 }
                        | 'type' identifier opt_attr_list ';'                                          { Type (fromId $2) [] $3 }
                        | 'typealias' identifier alias_def ';'                                         { TypeAlias (fromId $2) $3 }
                        | 'typeattribute' identifier id_comma_list ';'                                 { TypeAttribute (fromId $2) (NE.reverse (fromIds $3)) }
                        | 'bool' identifier bool_val ';'                                               { BoolDef (fromId $2) $3 }    
--                        | range_trans_def     { $1 }
                        | 'neverallow' na_source_types na_target_types ':' nested_ids permissions  ';' { TeNeverAllow (mkSourceTarget $2 $3 $5) $6 }
                        | stmt                                                                         { Stmt $1 }


stmt                    :: { Stmt }
                        : 'type_transition' source_types source_types ':' nested_ids identifier ';'    { Transition TypeTransition (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'type_member'     source_types source_types ':' nested_ids identifier ';'    { Transition TypeMember     (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'type_change'     source_types source_types ':' nested_ids identifier ';'    { Transition TypeChange     (mkSourceTarget $2 $3 $5) (fromId $6) }
                        | 'allow'      source_types target_types ':' nested_ids permissions  ';'       { TeAvTab Allow (mkSourceTarget $2 $3 $5) $6 }
                        | 'auditallow' source_types target_types ':' nested_ids permissions ';'        { TeAvTab AuditAllow (mkSourceTarget $2 $3 $5) $6 }
                        | 'auditdeny'  source_types target_types ':' nested_ids permissions ';'        { TeAvTab AuditDeny (mkSourceTarget $2 $3 $5) $6 }
                        | 'dontaudit'  source_types target_types ':' nested_ids permissions ';'        { TeAvTab DontAudit (mkSourceTarget $2 $3 $5) $6 }

opt_attr_list           :: { [AttributeId] }
                        : ',' id_comma_list { fromIds (reverse (toList $2)) }
                        |                   { [] }

bool_val                :: { Bool }
                        : 'true'  { True }
                        | 'false' { False }

cond_stmt_def           :: { TeRbac }
                        : 'if' cond_expr '{' cond_pol_list '}' cond_else { CondStmt $2 (reverse $4) $6 }

cond_else               :: { [RequireStmt] }
                        : 'else' '{' cond_pol_list '}' { reverse $3 }
                        |                              { [] }
cond_expr               :: { CondExpr }
                        : '(' cond_expr ')'         { $2 }
                        | 'not' cond_expr           { Not $2 }
                        | cond_expr 'and' cond_expr { Op $1 And $3 }
                        | cond_expr 'or' cond_expr  { Op $1 Or $3 }
                        | cond_expr 'xor' cond_expr { Op $1 Xor $3 }
                        | cond_expr 'eq' cond_expr  { Op $1 Equals $3 }
                        | cond_expr '!=' cond_expr  { Op $1 Notequal $3 }
                        | identifier                { Var (fromId $1) }


cond_pol_list           :: { [RequireStmt] }
                        : cond_pol_list cond_rule_def { $2 : $1 }
                        |                    { [] }

cond_rule_def           :: { RequireStmt }
                        : stmt          { RequireStmt $1 }
			| require_block { Require $1 }

{-
range_trans_def         : RANGE_TRANSITION names names mls_range_def ';'
                        { if (define_range_trans(0)) return -1; }
                        | RANGE_TRANSITION names names ':' names mls_range_def ';'
                        { if (define_range_trans(1)) return -1; }
                        ;
-}
 

roles                   :: { NonEmptyList (Tree RoleId) }
                        : role_def       { singleton $1 }
                        | roles role_def { $2 `cons` $1 }

role_def                :: { Tree RoleId }
                        : 'role' identifier ';'           { Node (fromId $2) [] }
                        | 'role' identifier '{' roles '}' { Node (fromId $2) (reverse (toList $4)) }

opt_constraints         :: { [Constraint] }
                        : constraints { reverse (toList $1) }
                        |             { [] }

constraints             :: { NonEmptyList Constraint }
                        : constraint_decl             { singleton $1 }
                        | constraints constraint_decl { $2 `cons` $1 }

constraint_decl         :: { Constraint } 
                        : 'constrain' nested_ids nested_ids cexpr ';' { Constrain (fromIds $2) (fromIds $3) $4 }
                        | 'validatetrans' nested_ids vexpr ';'        { ValidateTrans (fromIds $2) $3 }

cexpr                   :: { ConstraintExpr }
                        : '(' cexpr ')'     { $2 }
                        | 'not' cexpr       { CNot $2 }
                        | cexpr 'and' cexpr { COp $1 CAnd $3 }
                        | cexpr 'or' cexpr  { COp $1 COr $3 }
                        | cexpr_prim        { ConstraintExprPrim $1 }

vexpr                   :: { ConstraintExpr }
                        : '(' vexpr ')'     { $2 }
                        | 'not' vexpr       { CNot $2 }
                        | vexpr 'and' vexpr { COp $1 CAnd $3 }
                        | vexpr 'or' vexpr  { COp $1 COr $3 }
                        | vexpr_prim        { ConstraintExprPrim $1 }

cexpr_prim              :: { ConstraintExprPrim }
                        : 'u1' op 'u2'               { CUsers $2 }
                        | 'r1' role_mls_op 'r2'      { CRoles $2 }
                        | 't1' op 't2'               { CTypes $2 }
                        | 'u1' op nested_ids         { CUserSet C1 $2 (fromIds $3) }
                        | 'u2' op nested_ids         { CUserSet C2 $2 (fromIds $3) }
                        | 'r1' op nested_ids         { CRoleSet C1 $2 (fromIds $3) }
                        | 'r2' op nested_ids         { CRoleSet C2 $2 (fromIds $3) }
                        | 't1' op nested_ids         { CTypeSet C1 $2 (fromIds $3) }
                        | 't2' op nested_ids         { CTypeSet C2 $2 (fromIds $3) }
                        | 'sameuser'                 { CUsers CEquals }
                        | 'source' 'role' nested_ids { CRoleSet C1 CEquals (fromIds $3) }
                        | 'target' 'role' nested_ids { CRoleSet C2 CEquals (fromIds $3) }
                        | 'role' role_mls_op         { CRoles $2 }
                        | 'source' 'type' nested_ids { CTypeSet C1 CEquals (fromIds $3) }
                        | 'target' 'type' nested_ids { CTypeSet C2 CEquals (fromIds $3) }

vexpr_prim              :: { ConstraintExprPrim }
                        : 'u3' op nested_ids         { CUserSet C3 $2 (fromIds $3) }
                        | 'r3' op nested_ids         { CRoleSet C3 $2 (fromIds $3) }
                        | 't3' op nested_ids         { CTypeSet C3 $2 (fromIds $3) }
                        | cexpr_prim                 { $1 }

op                      :: { CEqOp }
                        : 'eq' { CEquals }
                        | '!='     { CNotequal }

role_mls_op             :: { RoleMlsOp }
                        : op        { CEqOp $1 }
                        | 'dom'     { Dom }
                        | 'domby'   { DomBy }
                        | 'incomp'  { InComp }

users                   :: { NonEmptyList User }
                        : user_def        { singleton $1 }
                        | users user_def  { $2 `cons` $1 }

user_def                :: { User }
                        : 'user' identifier 'roles' nested_ids {-opt_mls_user-} ';' { User (fromId $2) (fromIds $4) }

{-
opt_mls_user            : LEVEL mls_level_def RANGE mls_range_def
                        |
                        ;
-}

initial_sid_contexts    :: { NonEmptyList (SidContext SecurityContext) }
                        : initial_sid_context_def                      { singleton $1 }
                        | initial_sid_contexts initial_sid_context_def { $2 `cons` $1 } 

initial_sid_context_def :: { SidContext SecurityContext }
                        :  'sid' identifier security_context_def       { SidContext (fromId $2) $3 }

net_contexts            :: { ([PortContext SecurityContext],[NetInterfaceContext SecurityContext],[NodeContext SecurityContext]) }
                        : opt_port_contexts opt_netif_contexts opt_node_contexts { ($1,$2,$3) }

opt_port_contexts       :: { [PortContext SecurityContext] }
                        : port_contexts { reverse (toList $1) }
                        |               { [] }

port_contexts           :: { NonEmptyList (PortContext SecurityContext) }
                        : port_context_def               { singleton $1 }
                        | port_contexts port_context_def { $2 `cons` $1 }

port_context_def        :: { PortContext SecurityContext }
                        : 'portcon' identifier number security_context_def            {% return PortContext `ap` checkProtocol $2 
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return $4 }
                        | 'portcon' identifier number '-' number security_context_def {% return PortContext `ap` checkProtocol $2 
                                                                                                            `ap` return (mkPortNumber $3)
                                                                                                            `ap` return (mkPortNumber $5)
                                                                                                            `ap` return $6 }

opt_netif_contexts      :: { [NetInterfaceContext SecurityContext] }
                        : netif_contexts { reverse (toList $1) }
                        |                { [] }

netif_contexts          :: { NonEmptyList (NetInterfaceContext SecurityContext) }
                        : netif_context_def                { singleton $1 }
                        | netif_contexts netif_context_def { $2 `cons` $1 }

netif_context_def       :: { NetInterfaceContext SecurityContext }
                        : 'netifcon' identifier security_context_def security_context_def { NetInterfaceContext (fromId $2) $3 $4 }

opt_node_contexts       :: { [NodeContext SecurityContext] }
                        : node_contexts { reverse (toList $1) }
                        |               { [] }

node_contexts           :: { NonEmptyList (NodeContext SecurityContext) }
                        : node_context_def               { singleton $1 }
                        | node_contexts node_context_def { $2 `cons` $1 }

node_context_def        :: { NodeContext SecurityContext }
                        : 'nodecon' ipv4_addr ipv4_addr security_context_def { NodeContext (IPV4AddrMask $2 $3) $4 }
                        | 'nodecon' ipv6_addr ipv6_addr security_context_def { NodeContext (IPV6AddrMask $2 $3) $4 }

opt_fs_uses             :: { [FileSystemUse SecurityContext ] }
                        : fs_uses { reverse (toList $1) }
                        |         { [] }

fs_uses                 :: { NonEmptyList (FileSystemUse SecurityContext) }
                        : fs_use_def         { singleton $1 }
                        | fs_uses fs_use_def { $2 `cons` $1 }

fs_use_def              :: { FileSystemUse SecurityContext }
                        : 'fs_use_xattr' identifier security_context_def ';' { FSUseXattr (fromId $2) $3 }
                        | 'fs_use_task' identifier security_context_def ';'  { FSUseTask (fromId $2) $3 }
                        | 'fs_use_trans' identifier security_context_def ';' { FSUseTrans (fromId $2) $3 }

opt_genfs_contexts      :: { [GenFileSystemContext SecurityContext] }
                        : genfs_contexts { reverse (toList $1) }
                        |                { [] }

genfs_contexts          :: { NonEmptyList (GenFileSystemContext SecurityContext) }
                        : genfs_context_def                { singleton $1 }
                        | genfs_contexts genfs_context_def { $2 `cons` $1 }

genfs_context_def       :: { GenFileSystemContext SecurityContext}
                        : 'genfscon' identifier path '-' identifier security_context_def {% return (GenFSCon (fromId $2) $3) `ap` fmap Just (checkFileType $5) `ap` return $6 }
                        | 'genfscon' identifier path '-' '-' security_context_def        { GenFSCon (fromId $2) $3 (Just PlainFile) $6 }
                        | 'genfscon' identifier path security_context_def                { GenFSCon (fromId $2) $3 Nothing $4 }

security_context_def    :: { SecurityContext }
                        : identifier ':' identifier ':' identifier {-opt_mls_range_def-} { SecurityContext (fromId $1) (fromId $3) (fromId $5) }

{-
                        ;
opt_mls_range_def       : ':' mls_range_def
                        |       
                        ;
mls_range_def           : mls_level_def '-' mls_level_def
                        {if (insert_separator(0)) return -1;}
                        | mls_level_def
                        {if (insert_separator(0)) return -1;}
                        ;
mls_level_def           : identifier ':' id_comma_list
                        {if (insert_separator(0)) return -1;}
                        | identifier
                        {if (insert_separator(0)) return -1;}
                        ;
-}

id_comma_list           :: { NonEmptyList Identifier }
                        : identifier                   { singleton $1 }
                        | id_comma_list ',' identifier { $3 `cons` $1 }

tilde                   : '~' { () }

asterisk                : '*' { () }

source_types            :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : identifier '-' identifier { SignedId Positive (fromId $1) `cons` singleton (SignedId Negative (fromId $3)) }
                        | nested_signed             { $1 }

target_types            :: { NonEmptyList (SignedId Self) }
                        : self '-' self      { SignedId Positive $1 `cons` singleton (SignedId Negative $3) }
                        | nested_self_signed { $1 }

self                    :: { Self }
                        : identifier { NotSelf (fromId $1) }
                        | 'self'     { Self }

na_source_types         :: { NeverAllow TypeOrAttributeId }
                        : asterisk            { NAStarTilde Star }
                        | tilde nested_signed { NAStarTilde (Tilde $2) }
                        | source_types        { NeverAllow $1 }

na_target_types         :: { NeverAllow Self }
                        : asterisk            { NAStarTilde Star }
                        | tilde nested_self_signed { NAStarTilde (Tilde $2) }
                        | target_types { NeverAllow $1 }

permissions             :: { Permissions }
                        : nested_ids       { Permissions (fromIds $1) }
                        | asterisk         { PStarTilde Star }
                        | tilde nested_ids { PStarTilde (Tilde (fromIds $2)) }

identifier_list         :: { NonEmptyList (Identifier) }
                        : identifier                 { singleton $1 }
                        | identifier_list identifier { $2 `cons` $1 }


nested_signed_set       :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : '{' nested_signed_list '}' { $2 }

nested_signed_list      :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : nested_signed_element                    { $1 }
                        | nested_signed_list nested_signed_element { $1 `append` $2 }

nested_signed           :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : identifier        { singleton (SignedId Positive (fromId $1)) }
                        | nested_signed_set { $1 }

nested_signed_element   :: { NonEmptyList (SignedId TypeOrAttributeId) }
                        : '-' identifier { singleton (SignedId Negative (fromId $2)) }
                        | nested_signed  { $1 }

-- same for identifiers+self

nested_self_signed_set  :: { NonEmptyList (SignedId Self) }
                        : '{' nested_self_signed_list '}' { $2 }

nested_self_signed_list :: { NonEmptyList (SignedId Self) }
                        : nested_self_signed_element                         { $1 }
                        | nested_self_signed_list nested_self_signed_element { $1 `append` $2 }

nested_self_signed      :: { NonEmptyList (SignedId Self) }
                        : self                   { singleton (SignedId Positive $1) }
                        | nested_self_signed_set { $1 }

nested_self_signed_element   :: { NonEmptyList (SignedId Self) }
                        : '-' self           { singleton (SignedId Negative $2) }
                        | nested_self_signed { $1 }

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

path                    :: { FilePath } 
                        : Path       { FilePath $1 }

number                  :: { Integer } 
                        : Number     { $1 } 

ipv4_addr               :: { IPV4Address }
                        : Ipv4addr  { mkIPV4Address $1 }

ipv6_addr               :: { IPV6Address }
                        : Ipv6addr  { mkIPV6Address $1 }

avrules_block           :: { AvRuleBlock }
                        : avrule_decls avrule_user_defs { AvRuleBlock (reverse $1) (reverse $2) }

avrule_decls            :: { [AvRule] }
                        : avrule_decls avrule_decl { $2 : $1 }
                        | avrule_decls ';'         { $1 }
                        | avrule_decl              { [$1] }
                        | ';'                      { [] }

avrule_decl             :: { AvRule }
                        : te_rbac_decl   { TeRbac $1 }
                        | require_block  { AvRuleRequire $1 }

require_block           :: { NonEmptyList Require }
                        : 'require' '{' require_list '}' { NE.reverse $3 }

require_list            :: { NonEmptyList Require }
                        : require_list require_decl      { $2 `cons` $1 }
                        | require_decl                   { singleton $1 }

require_decl            :: { Require }
                        : 'class' identifier nested_ids ';' { RequireClass (fromId $2) (fromIds $3) }
                        | 'role'        id_comma_list ';' { RequireRole (fromIds (NE.reverse $2)) }
                        | 'type'        id_comma_list ';' { RequireType (fromIds (NE.reverse $2)) }
                        | 'attribute'   id_comma_list ';' { RequireAttribute (fromIds (NE.reverse $2)) }
                        | 'user'        id_comma_list ';' { RequireUser (fromIds (NE.reverse $2)) }
                        | 'bool'        id_comma_list ';' { RequireBool (fromIds (NE.reverse $2)) }
--                        | 'sensitivity' id_comma_list ';' { RequireSensitivity (fromIds (NE.reverse $2)) }
--                        | 'category'    id_comma_list ';' { RequireCategory (fromIds (NE.reverse $2)) }


optional_block          :: { TeRbac }
                        : 'optional' '{' avrules_block '}' optional_else { Optional $3 $5 }

optional_else           :: { Maybe AvRuleBlock }
                        : 'else' '{' avrules_block '}' { Just $3 }
                        |                              { Nothing }

avrule_user_defs        :: { [User] }
                        : avrule_user_defs user_def { $2 : $1 }
                        |                           { [] }

{
type AccessVectors = ([CommonPerm],NonEmptyList AvPerm)

parsePolicy :: FilePath.FilePath -> String -> Either String Policy
parsePolicy f = mkParser (pPolicy . scan f)

parseInitialSids :: FilePath.FilePath -> String -> Either String (NonEmptyList Sid)
parseInitialSids f = mkParser (pInitialSids . scan f)

parseAccessVectors :: FilePath.FilePath -> String -> Either String AccessVectors
parseAccessVectors f = mkParser (pAccessVectors . scan f)

parseClasses :: FilePath.FilePath -> String -> Either String [ClassId]
parseClasses f = mkParser (pClasses . scan f)

-- auxiliary

mkSourceTarget :: st -> tt -> NonEmptyList Identifier -> SourceTarget st tt
mkSourceTarget st tt cls = SourceTarget{ sourceTypes = st
                                       , targetTypes = tt
                                       , targetClasses = fromIds cls
                                       }

fromIds :: (IsIdentifier i, Functor l) => l Identifier -> l i
fromIds = fmap fromId

mkPortNumber :: Integer -> Word16
mkPortNumber i = fromInteger i

mkOctet :: Integer -> Word8
mkOctet i = fromInteger i

mkIPV4Address :: String -> IPV4Address
mkIPV4Address s = 
  case map read (filter (/=".") (groupBy (\a b -> (a=='.')==(b=='.')) s)) of
    [a,b,c,d] -> IPV4Address a b c d
    _         -> error $ "mkIPV4Address: "++ s

mkIPV6Address :: String -> IPV6Address
mkIPV6Address str = IPV6Address a b c d e f g h
  where [a,b,c,d,e,f,g,h] = res
        splits = split ':' str
        numsplits = length splits
        res = foldr (parseComponents $ 9 - numsplits) [] splits -- Yes, 9

        parseComponents :: Int -> String -> [Word16] -> [Word16]
        parseComponents ns "" acc = (replicate ns 0) ++ acc
        parseComponents _ instr acc = 
          let [(cur', _)] = readHex instr
          in cur':acc
        -- 
        split :: (Eq a) => a -> [a] -> [[a]]
        split _ [] = []
        split x (f:(g:rest)) | f == x && g == x = []:(split x rest)
        split x (f:rest) | f == x = split x rest
        split x instr =
          case span (/= x) instr of
            ([],rest) -> split x rest
            (first,rest) -> first:split x rest

strictMkId :: Token -> P Identifier
strictMkId (T p (IDENTIFIER s)) = i == i `seq` return i
  where i = memoMkId p s
strictMkId _ = error "strictMkId"

memoMkId :: Pos -> String -> Identifier
memoMkId p = {-memo-} mkId' p -- "Pure" memoization result in a slight slow-down.

check :: String -> [(String,a)] -> Identifier -> P a
check err l i = maybe (throwError (err++": "++s)) return (lookup s l)
  where s = idString i

checkFileType :: Identifier -> P FileType
checkFileType = check "Illegal file type" [("b",BlockFile), ("c",CharacterFile), ("d",DirectoryFile), ("p",FifoFile), ("l",LinkFile), ("s",SocketFile)]

checkProtocol :: Identifier -> P Protocol
checkProtocol = check "Illegal protocol" [("tcp", Tcp), ("TCP", Tcp), ("udp", Udp), ("UDP", Udp)]

-- TODO: Better error messages (once we collect position information in identifiers, for example)
projectSignedIds :: Show t => (t -> P RoleId) -> NonEmptyList (SignedId t) -> P (NonEmptyList RoleId)
projectSignedIds proj = mapM (projectSignedId proj)

projectSignedId :: Show t => (t -> P RoleId) -> SignedId t -> P RoleId
projectSignedId r (SignedId Positive t) = r t
projectSignedId _ e                     = throwError $ "Illegal expression in role allow statement: "++show e

projectSelf :: Self -> P RoleId
projectSelf (NotSelf t) = return (fromId (toId t))
projectSelf Self        = throwError $ "Illegal expression in role allow statement: self"

}
