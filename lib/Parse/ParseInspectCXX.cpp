//===--- ParseInspectCXX.cpp - Pattern Matching Stmt & Expr Parser --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file impletents `inspect` pattern matching statement and expression
// parser for PR1308R0 C++ standard proposal.
//
//===----------------------------------------------------------------------===//

#include "clang/AST/PrettyDeclStackTrace.h"
#include "clang/Basic/Attributes.h"
#include "clang/Basic/PrettyStackTrace.h"
#include "clang/Parse/Parser.h"
#include "clang/Parse/RAIIObjectsForParser.h"
#include "clang/Sema/DeclSpec.h"
#include "clang/Sema/LoopHint.h"
#include "clang/Sema/Scope.h"
#include "clang/Sema/TypoCorrection.h"

using namespace clang;

StmtResult Parser::ParseCXXInspectStmt() {
  assert(Tok.is(tok::kw_inspect) && "Not an inspect stmt!");
  SourceLocation InspectLoc = ConsumeToken();

  if (!getLangOpts().PatternMatching) {
    Diag(Tok, diag::err_pattern_matching_not_enabled);
    SkipUntil(tok::semi);
    return StmtError();
  }

  ParseScope inspectScope(this, Scope::DeclScope | Scope::ControlScope);

  StmtResult InitStmt;
  Sema::ConditionResult Cond;
  // TODO: Change ConditionKind ?
  if (ParseParenExprOrCondition(&InitStmt, Cond, InspectLoc, 
        Sema::ConditionKind::Switch))
    return StmtError();

  assert(Tok.is(tok::l_brace) && "Must open new block after `inspect`!"); 
  ConsumeBrace();
  SkipUntil(tok::r_brace);

  inspectScope.Exit();  

  return StmtEmpty();
}
