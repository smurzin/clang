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
    Diag(Tok, diag::err_inspect_requires_pattern_matching);
    SkipUntil(tok::semi);
    return StmtError();
  }

  ParseScope InspectScope(this, Scope::DeclScope | Scope::ControlScope);

  StmtResult InitStmt;
  Sema::ConditionResult Cond;
  if (ParseParenExprOrCondition(&InitStmt, Cond, InspectLoc,
        Sema::ConditionKind::Inspect))
    return StmtError();

  StmtResult Inspect =
       Actions.ActOnStartOfInspectStmt(InspectLoc, InitStmt.get(), Cond);

  if (Inspect.isInvalid()) {
    if (Tok.is(tok::l_brace)) {
        ConsumeBrace();
        SkipUntil(tok::r_brace);
    } else {
        SkipUntil(tok::semi);
    }
    return Inspect;
  }

  StmtResult Body = ParseCXXInspectStmtBody();

  InspectScope.Exit();

  return Actions.ActOnFinishInspectStmt(InspectLoc, Inspect.get(), Body.get());
}

StmtResult Parser::ParseCXXInspectStmtBody() {
    BalancedDelimiterTracker T(*this, tok::l_brace);
    if (T.consumeOpen()) {
        SkipUntil(tok::r_brace, SkipUntilFlags::StopAtSemi);
        return StmtError();
    }

    // Parse all patterns
    while (!Tok.is(tok::r_brace) && !Tok.is(tok::eof)) {
        StmtResult PatternStmt = ParseCXXInspectPattern();
        if (PatternStmt.isInvalid()) {
            break;
        }

        llvm::outs() << "\n";

        if (!Tok.is(tok::fatarrow)) {
            Diag(Tok, diag::err_expected) << tok::fatarrow;
            break;
        }
        ConsumeToken();

        StmtResult MatchedStmt = ParseStatement();
        if (MatchedStmt.isInvalid()) {
            Diag(Tok, diag::err_expected_statement);
            break;
        }
    }

    if (T.consumeClose()) {
        SkipUntil(tok::semi);
        return StmtError();
    }

    return StmtEmpty();
}

StmtResult Parser::ParseCXXInspectPattern() {
    switch (Tok.getKind()) {
    case tok::l_square: {
            BalancedDelimiterTracker T(*this, tok::l_square);
            assert(!T.consumeOpen());

            llvm::outs() << "[ ";

            bool first = true;
            do {
                if (!first) {
                    llvm::outs() << ", ";
                    ConsumeToken();
                }

                StmtResult InnerPatternStmt = ParseCXXInspectPattern();

                if (InnerPatternStmt.isInvalid()) {
                    Diag(Tok, diag::err_expected_pattern);
                    return StmtError();
                }

                first = false;
            } while (Tok.is(tok::comma) && !Tok.is(tok::eof));

            if (T.consumeClose()) {
                return StmtError();
            }

            llvm::outs() << " ]";
            break;
        }
    case tok::l_paren: {
            BalancedDelimiterTracker T(*this, tok::l_paren);
            assert(!T.consumeOpen());

            llvm::outs() << "( ";

            ExprResult Res = ParseCXXInspectPatternExpr();
            if (Res.isInvalid()) {
                Diag(Tok, diag::err_expected_expression);
                return StmtError();
            }

            if (T.consumeClose()) {
                return StmtError();
            }

            llvm::outs() << " )";

            break;
        }
    case tok::kw_nullptr:
        llvm::outs() << "nullptr";
        ConsumeToken(); // Append == nullptr pattern
        break;
    case tok::star: {
            llvm::outs() << "* ";
            ConsumeToken(); // Append pointee pattern
            return ParseCXXInspectPattern();
        }
    case tok::identifier: {
            Token const& Next = NextToken();

            if (Next.is(tok::unknown)) { // We are at identifier @ pattern branch
                llvm::outs() << "identifier @ ";
                ConsumeToken(); // New identifier
                ConsumeToken(); // @
                return ParseCXXInspectPattern();
            }

            if (Next.isOneOf(tok::r_square, tok::comma, tok::fatarrow)) {
                // TODO Handle `_` special case
                llvm::outs() << "identifier";
                ConsumeToken(); // New identifier
                break;
            }

            LLVM_FALLTHROUGH;
        }
    default: {
            if (isTypeIdUnambiguously()) {
                llvm::outs() << "TypeId ";

                TypeResult Res = ParseTypeName();
                if (Res.isInvalid()) {
                    return StmtError();
                }

                return ParseCXXInspectPattern();
            }

            ExprResult Res = ParseCXXInspectPatternExpr();
            if (Res.isInvalid()) {
                Diag(Tok, diag::err_expected_expression);
                return StmtError();
            }

            break;
        }
    }

    return StmtEmpty();
}

ExprResult Parser::ParseCXXInspectPatternExpr() {
    llvm::outs() << "constant-expr";

    EnterExpressionEvaluationContext ConstantEvaluated(Actions, Sema::ExpressionEvaluationContext::ConstantEvaluated);
    ExprResult LHS(ParseCastExpression(false, false, NotTypeCast));
    return  ParseRHSOfBinaryExpression(LHS, prec::Conditional);
}
