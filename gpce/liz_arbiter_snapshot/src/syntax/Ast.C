// Copyright (C) 2009-2013, Texas A&M University.
// Copyright (C) 2014, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of Liz, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//


// This file defines various Liz AST object representations.

#include <fstream>
#include <liz/Ast>

namespace liz {
   // -- DescriptionAst --
   DescriptionAst::DescriptionAst(const vector<Token>& ts)
         : vector<Token>(ts)
   { }
   
   // -- SourceFileAst --
   SourceFileAst::SourceFileAst(const std::string& f)
         : path(f)
   { }

   // -- Atomic --
   Atomic::Atomic(const Token& t)
         : structure::unary<Token>(t)
   { }

   // -- EnclosureAst --
   EnclosureAst::EnclosureAst(const BracketAst* x, const Ast* y)
         : structure::binary<const BracketAst*, const Ast*>(x, y)
   { }

   // -- LiteralAst --
   LiteralAst::LiteralAst(const Token& t)
         : Atomic(t)
   { }

   // -- IdentifierAst --
   IdentifierAst::IdentifierAst(const Token& t)
         : Atomic(t)
   { }

   WildcardAst::WildcardAst(const Token& t)
         : Atomic(t)
   { }

   // -- OperatorAst --
   OperatorAst::OperatorAst(const Token& t)
         : Atomic(t)
   { }

   BracketAst::BracketAst(const Token& t1, const Token& t2)
         : structure::binary<Token>(t1, t2)
   { }

   // -- BiSectionAst --
   BiSectionAst::BiSectionAst(const OperatorAst* x)
         : structure::unary<const OperatorAst*>(x)
   { }

   // -- LeftSectionAst --
   LeftSectionAst::LeftSectionAst(const Ast* x, const OperatorAst* y)
         : structure::binary<const Ast*, const OperatorAst*>(x, y)
   { }

   // -- RightSectionAst --
   RightSectionAst::RightSectionAst(const OperatorAst* x, const Ast* y)
         : structure::binary<const OperatorAst*, const Ast*>(x, y)
   { }

   // -- CollectAst --
   CollectAst::CollectAst(const Sequence<Iterator>& i, const Ast* x)
         : structure::binary<Sequence<Iterator>, const Ast*>(i, x)
   { }

   AssertAst::AssertAst(const Token& t, const Ast* x)
         : structure::binary<Token, const Ast*>(t, x)
   { }

   // -- RestrictAst --
   RestrictAst::RestrictAst(const Ast* x, const Ast* t)
         : structure::binary<const Ast*>(x, t)
   { }

   // -- UnaryAst --
   UnaryAst::UnaryAst(const OperatorAst* op, const Ast* arg)
         : structure::binary<const OperatorAst*, const Ast*>(op, arg)
   { }

   // -- BinaryAst --
   BinaryAst::BinaryAst(const OperatorAst* op, const Ast* x, const Ast* y)
         : structure::ternary<const OperatorAst*, const Ast*>(op, x, y)
   { }

   // -- DotAst --
   DotAst::DotAst(const Ast* x, const Ast* y)
         : structure::binary<const Ast*>(x, y)
   { }

   // -- JuxtaposeAst --
   JuxtaposeAst::JuxtaposeAst(const Ast* x, const Ast* y)
         : structure::binary<const Ast*>(x, y)
   { }

   // -- ExprStmtAst --
   ExprStmtAst::ExprStmtAst(const Token& t, const Ast* x)
         : structure::binary<Token, const Ast*>(t, x)
   { }

   // -- FilterAst --
   FilterAst::FilterAst(const Ast* x, const Ast* c)
         : structure::binary<const Ast*>(x, c)
   { }

   // -- ParameterAst --
   ParameterAst::ParameterAst(const IdentifierAst* n,
                              const Ast* t, const Ast* x)
         : structure::ternary<const IdentifierAst*, const Ast*>(n, t, x)
   { }

   // -- SignatureAst --
   SignatureAst::SignatureAst(const FixityForm* n, const Ast* t, const Ast* i)
         : structure::ternary<const FixityForm*, const Ast*>(n, t, i)
   { }

   const FixityForm*
   SignatureAst::form() const {
      return first();
   }

   const Ast*
   SignatureAst::type() const {
      return second();
   }

   // -- WhereAst --
   WhereAst::WhereAst(const Ast* x, const Sequence<DeclarativeAst>& ds)
         : structure::binary<const Ast*, Sequence<DeclarativeAst>>(x, ds)
   { }

   // -- DefinitionAst --
   DefinitionAst::DefinitionAst(const FixityForm* n,
                                const Ast* t, const Ast* x)
         : structure::ternary<const FixityForm*, const Ast*>(n, t, x)
   { }

   const FixityForm*
   DefinitionAst::form() const {
      return first();
   }

   const Ast*
   DefinitionAst::type() const {
      return second();
   }

   // -- RuleAst --
   RuleAst::RuleAst(const FixityForm* n, const Ast* t, const Ast* x)
         : structure::ternary<const FixityForm*, const Ast*>(n, t, x)
   { }

   const FixityForm*
   RuleAst::form() const {
      return first();
   }

   const Ast*
   RuleAst::type() const {
      return second();
   }

   PostulateAst::PostulateAst(const FixityForm* f, const Ast* t)
         : structure::binary<const FixityForm*, const Ast*>(f, t)
   { }

   AliasAst::AliasAst(const IdentifierAst* a, const Ast* v)
         : structure::binary<const IdentifierAst*, const Ast*>(a, v)
   { }

   const FixityForm*
   PostulateAst::form() const {
      return first();
   }

   const Ast*
   PostulateAst::type() const {
      return second();
   }

   ExPostulateAst::ExPostulateAst(const IdentifierAst* n,
                                  const IdentifierAst* t, const Ast* c)
         : structure::ternary<const IdentifierAst*, const IdentifierAst*,
                              const Ast*>(n, t, c)
   { }

   const IdentifierAst*
   ExPostulateAst::name() const {
      return first();
   }

   const IdentifierAst*
   ExPostulateAst::type() const {
      return second();
   }

   const Ast*
   ExPostulateAst::constraint() const {
      return third();
   }

   AssociatedAst::AssociatedAst(const IdentifierAst* a, const IdentifierAst* v)
         : structure::binary<const IdentifierAst*, const IdentifierAst*>
            (a, v)
   { }

   ProlongAst::ProlongAst(const FixityForm* f, const Ast* t, const Ast* x)
         : structure::ternary<const FixityForm*, const Ast*>(f, t, x)
   { }

   const FixityForm*
   ProlongAst::form() const {
      return first();
   }

   const Ast*
   ProlongAst::type() const {
      return second();
   }

   // -- IfAst --
   IfAst::IfAst(const Ast* c, const Ast* t, const Ast* f)
         : structure::ternary<const Ast*>(c, t, f)
   { }

   // -- ApplyAst --
   ApplyAst::ApplyAst(const Ast* f, const EnclosureAst* a)
         : structure::binary<const Ast*, const EnclosureAst*>(f, a)
   { }

   ArrowAst::ArrowAst(const Ast* s, const Ast* t)
         : structure::binary<const Ast*>(s, t)
   { }

   // -- IntervalAst --
   IntervalAst::IntervalAst(const Ast* x, const Ast* y)
         : structure::binary<const Ast*>(x, y)
   { }

   // -- CaseAst --
   CaseAst::CaseAst(const Ast* key, const Ast* stmt)
         : structure::binary<const Ast*>(key, stmt)
   { }

   // -- MatchAst --
   MatchAst::MatchAst(const Ast* cond, const Sequence<CaseAst>& cases)
         : structure::binary<const Ast*, Sequence<CaseAst>>(cond, cases)
   { }

   // -- List --
   List::List(const AstSequence& s)
         : structure::unary<AstSequence>(s)
   { }

   // -- CompoundAst --
   CompoundAst::CompoundAst(const AstSequence& s)
         : List(s)
   { }

   // -- DatatypeAst --
   DatatypeAst::DatatypeAst(const Token& t, const Sequence<SignatureAst>& x)
         : structure::binary<Token, Sequence<SignatureAst>>(t, x)
   { }

   // -- AssignmentAst --
   AssignmentAst::AssignmentAst(const Ast* x, const Ast* y)
         : structure::binary<const Ast*>(x, y)
   { }

   // -- QuantifiedAst --
   QuantifiedAst::QuantifiedAst(const OperatorAst* q, const Parameters& p,
                                const Ast* c, const Ast* x)
         : AbstractionAst(p, x), quant(q), constraint_(c)
   { }

   // -- LambdaAst --
   LambdaAst::LambdaAst(const Parameters& p, const Ast* x)
         : AbstractionAst(p, x)
   { }

   // -- ForIterator --
   ForIterator::ForIterator(const IdentifierAst* v, const Ast* x)
         : structure::binary<const IdentifierAst*, const Ast*>(v, x)
   { }

   //  -- WhileIterator --
   WhileIterator::WhileIterator(const Ast* c)
         : structure::unary<const Ast*>(c)
   { }

   // -- UntilIterator --
   UntilIterator::UntilIterator(const Ast* c)
         : structure::unary<const Ast*>(c)
   { }

   // -- ProvisoIterator --
   ProvisoIterator::ProvisoIterator(const Ast* c)
         : structure::unary<const Ast*>(c)
   { }

   // -- RepeatAst --
   RepeatAst::RepeatAst(const Sequence<Iterator>& i, const Ast* x)
         : structure::binary<Sequence<Iterator>, const Ast*>(i, x)
   { }

   // -- ReturnAst --
   ReturnAst::ReturnAst(const Token& t, const Ast* x)
         : structure::binary<Token, const Ast*>(t, x)
   { }

   // -- LeaveAst --
   LeaveAst::LeaveAst(const Token& t, const Ast* x)
         : structure::binary<Token, const Ast*>(t, x)
   { }

   // -- ThrowAst --
   ThrowAst::ThrowAst(const Token& t, const Ast* x)
         : structure::binary<Token, const Ast*>(t, x)
   { }

   // -- SequenceAst --
   SequenceAst::SequenceAst(const AstSequence& s)
         : List(s)
   { }

   // -- CallForm --
   CallForm::CallForm(const Token& f, const Parameters& x, const Ast* c)
         : structure::ternary<Token, Parameters, const Ast*>(f, x, c)
    { }

   LiteralForm::LiteralForm(const Token& t)
         : Atomic(t)
   { }
   
   // -- AlphabethicForm --
   AlphabethicForm::AlphabethicForm(const Token& t)
         : Atomic(t)
   { }

   OperatorForm::OperatorForm(const Token& t)
         : Atomic(t)
   { }

   // -- PrefixForm --
   PrefixForm::PrefixForm(const OperatorAst* f, const ParameterAst* x)
         : structure::binary<const OperatorAst*, const ParameterAst*>(f, x)
   { }

   // -- SuffixForm --
   SuffixForm::SuffixForm(const OperatorAst* f, const ParameterAst* x)
         : structure::binary<const OperatorAst*, const ParameterAst*>(f, x)
   { }

   // -- InfixForm --
   InfixForm::InfixForm(const OperatorAst* f,
                        const ParameterAst* x, const ParameterAst* y)
         : structure::ternary<const OperatorAst*, const ParameterAst*>(f, x, y)
   { }

   // -- ClosedForm --
   ClosedForm::ClosedForm(const BracketAst* x, const Parameters& y)
         : structure::binary<const BracketAst*, Parameters>(x, y)
   { }

   // -------------
   // -- PathAst --
   // -------------
   PathAst::PathAst(const IdentifierAst* d, const Ast* p)
         : structure::binary<const IdentifierAst*, const Ast*>(d, p)
   { }

   // -- ImportAst --
   ImportAst::ImportAst(const Ast* x)
         : structure::unary<const Ast*>(x)
   { }

   // -- AstFactory --
   static void
   slurp_lines_into(std::istream& is, input::Source& src) {
      std::string str;
      while (std::getline(is, str))
         src.append(str);
   }

   SourceFileAst*
   AstFactory::make_source_file(const std::string& f) {
      std::ifstream is(f.c_str());
      if (!is)
         cannot_open(f);
      auto file = srcs.make(f);
      slurp_lines_into(is, file->source);
      return file;
   }

   const DescriptionAst*
   AstFactory::make_description(const vector<Token>& ts) {
      return descs.make(ts);
   }
   
   const LiteralAst*
   AstFactory::make_literal(const Token& t) {
      return lits.make(t);
   }

   const IdentifierAst*
   AstFactory::make_identifier(const Token& t) {
      return ids.make(t);
   }

   const WildcardAst*
   AstFactory::make_wildcard(const Token& t) {
      return wilds.make(t);
   }
   
   const OperatorAst*
   AstFactory::make_operator(const Token& t) {
      return ops.make(t);
   }

   const BracketAst*
   AstFactory::make_bracket(const Token& t1, const Token& t2) {
      return brackets.make(t1, t2);
   }

   const LiteralForm*
   AstFactory::make_literal_form(const Token& t) {
      return litforms.make(t);
   }

   const AlphabethicForm*
   AstFactory::make_alphabetic_form(const Token& t) {
      return alphaforms.make(t);
   }

   const OperatorForm*
   AstFactory::make_operator_form(const Token& t) {
      return operforms.make(t);
   }

   const BiSectionAst*
   AstFactory::make_bisection(const OperatorAst* x) {
      return bisects.make(x);
   }

   const ClosedForm*
   AstFactory::make_closed_form(const BracketAst* x, const Parameters& y) {
      return closeds.make(x, y);
   }

   const LeftSectionAst*
   AstFactory::make_left_section(const Ast* x, const OperatorAst* y) {
      return lsects.make(x, y);
   }

   const RightSectionAst*
   AstFactory::make_right_section(const OperatorAst* x, const Ast* y) {
      return rsects.make(x, y);
   }

   const AssertAst*
   AstFactory::make_assert(const Token& t, const Ast* x) {
      return asserts.make(t, x);
   }

   const RestrictAst*
   AstFactory::make_restrict(const Ast* x, const Ast* t) {
      return restricts.make(x, t);
   }

   const DotAst*
   AstFactory::make_dot(const Ast*x, const Ast* y) {
      return dots.make(x, y);
   }

   const JuxtaposeAst*
   AstFactory::make_juxtapose(const Ast* x, const Ast* y) {
      return juxtaposes.make(x, y);
   }

   const EnclosureAst*
   AstFactory::make_enclosure(const BracketAst* x, const Ast* y) {
      return encs.make(x, y);
   }

   // Build a unary expression AST with the operator indicated 
   // by `op' and operand designated by `arg'.
   const UnaryAst*
   AstFactory::make_ast(const OperatorAst* op, const Ast* arg) {
      return uns.make(op, arg);
   }
   
   // Builds a binary expression AST object with operation given by
   // `op' and operands designated by `lhs' and `rhs'.
   const BinaryAst*
   AstFactory::make_ast(const OperatorAst* o, const Ast* x, const Ast* y) {
      return bis.make(o, x, y);
   }

   const ExprStmtAst*
   AstFactory::make_expr_stmt(const Token& t, const Ast* x) {
      return exprs.make(t, x);
   }

   const FilterAst*
   AstFactory::make_filter(const Ast* x, const Ast* c) {
      return fltrs.make(x, c);
   }
   
   const IfAst*
   AstFactory::make_if(const Ast* cond, const Ast* conseq, const Ast* alt) {
      return ifs.make(cond, conseq, alt);
   }
   
   const ApplyAst*
   AstFactory::make_apply(const Ast* fun, const EnclosureAst* args) {
      return calls.make(fun, args);
   }

   const ArrowAst*
   AstFactory::make_arrow(const Ast* s, const Ast* t) {
      return arrows.make(s, t);
   }

   const IntervalAst*
   AstFactory::make_interval(const Ast* x, const Ast* y) {
      return intvls.make(x, y);
   }

   const AssignmentAst*
   AstFactory::make_assignment(const Ast* x, const Ast* y) {
      return assgns.make(x, y);
   }

   const QuantifiedAst*
   AstFactory::make_quantified(const OperatorAst* q, const Parameters& parms,
                               const Ast* constraint, const Ast* bound) {
      return quants.make(q, parms, constraint, bound);
   }

   const LambdaAst*
   AstFactory::make_lambda(const Parameters& parms, const Ast* x) {
      return lams.make(parms, x);
   }
   
   const CompoundAst*
   AstFactory::make_compound(const AstSequence& a) {
      return cmps.make(a);
   }

   const DatatypeAst*
   AstFactory::make_datatype(const Token& t, const Sequence<SignatureAst>& x) {
      return datatypes.make(t, x);
   }

   const CaseAst*
   AstFactory::make_case(const Ast* key, const Ast* stmt) {
      return cases.make(key, stmt);
   }

   const MatchAst*
   AstFactory::make_match(const Ast* expr, const Sequence<CaseAst>& cases) {
      return sws.make(expr, cases);
   }
   
   const ForIterator*
   AstFactory::make_for(const IdentifierAst* v, const Ast* x) {
      return fors.make(v, x);
   }
   
   const WhileIterator*
   AstFactory::make_while(const Ast* cond) {
      return wls.make(cond);
   }
   
   const UntilIterator*
   AstFactory::make_until(const Ast* cond) {
      return ntls.make(cond);
   }

   const ProvisoIterator*
   AstFactory::make_proviso(const Ast* cond) {
      return pros.make(cond);
   }

   const RepeatAst*
   AstFactory::make_repeat(const Sequence<Iterator>& iters, const Ast* body) {
      return rpts.make(iters, body);
   }

   const ReturnAst*
   AstFactory::make_return(const Token& t, const Ast* x) {
      return returns.make(t, x);
   }

   const LeaveAst*
   AstFactory::make_leave(const Token& t, const Ast* x) {
      return leaves.make(t, x);
   }

   const ThrowAst*
   AstFactory::make_throw(const Token& t, const Ast* x) {
      return throws.make(t, x);
   }

   const CollectAst*
   AstFactory::make_collect(const Sequence<Iterator>& iters, const Ast* body) {
      return clls.make(iters, body);
   }

   const WhereAst*
   AstFactory::make_where(const Ast* x, const Sequence<DeclarativeAst>& ds) {
      return wheres.make(x, ds);
   }

   const SequenceAst*
   AstFactory::make_sequence(const AstSequence& a) {
      return seqs.make(a);
   }

   const SignatureAst*
   AstFactory::make_signature(const FixityForm* n, const Ast* t, const Ast* i) {
      return sigs.make(n, t, i);
   }

   const PostulateAst*
   AstFactory::make_postulate(const FixityForm* n, const Ast* t) {
      return pos.make(n, t);
   }

   const AliasAst*
   AstFactory::make_alias(const IdentifierAst* a, const Ast* v) {
      return aliases.make(a, v);
   }

   const ExPostulateAst*
   AstFactory::make_expostulate(const IdentifierAst* n, const IdentifierAst* t,
                                const Ast* c)
   {
      return expos.make(n, t, c);
   }

   const AssociatedAst*
   AstFactory::make_associated(const IdentifierAst* a, const IdentifierAst* v)
   {
      return assocs.make(a, v);
   }

   const DefinitionAst*
   AstFactory::make_definition(const FixityForm* n,
                               const Ast* t, const Ast* x) {
      return defs.make(n, t, x);
   }

   const ProlongAst*
   AstFactory::make_prolong(const FixityForm* n, const Ast* t, const Ast* x) {
      return prolongs.make(n, t, x);
   }

   const RuleAst*
   AstFactory::make_rule(const FixityForm* n, const Ast* t, const Ast* x) {
      return rules.make(n, t, x);
   }

   const CallForm*
   AstFactory::make_call_form(const Token& f, const Parameters& x, const Ast* c)
   {
      return forms.make(f, x, c);
   }

   const PrefixForm*
   AstFactory::make_prefix_form(const OperatorAst* o, const ParameterAst* p) {
      return prefixes.make(o, p);
   }

   const SuffixForm*
   AstFactory::make_suffix_form(const OperatorAst* o, const ParameterAst* p) {
      return suffixes.make(o, p);
   }

   const InfixForm*
   AstFactory::make_infix_form(const OperatorAst* o, const ParameterAst* x,
                               const ParameterAst* y) {
      return infixes.make(o, x, y);
   }
   
   const ParameterAst*
   AstFactory::make_parameter(const IdentifierAst* n, const Ast* t,
                              const Ast* x) {
      return parms.make(n, t, x);
   }

   const PathAst*
   AstFactory::make_path(const IdentifierAst* x, const Ast* p) {
      return paths.make(x, p);
   }

   const ImportAst*
   AstFactory::make_import(const Ast* x) {
      return imports.make(x);
   }

   const Token*
   anchor(const FixityForm* x) {
      struct V : FixityForm::Visitor {
         const Token* result;
         V() : result() { }
         void visit(const LiteralForm& x) { result = x.token(); }
         void visit(const AlphabethicForm& x) { result = x.token(); }
         void visit(const OperatorForm& x) { result = x.token(); }
         void visit(const PrefixForm& x) { result = anchor(x.operation()); }
         void visit(const SuffixForm& x) { result = anchor(x.operation()); }
         void visit(const InfixForm& x) { result = anchor(x.lhs()); }
         void visit(const ClosedForm& x) { result = anchor(x.bracket()); }
         void visit(const CallForm& x) { result = &x.function(); }
      };
      V v;
      x->accept(v);
      return v.result;
   }

   const Token*
   anchor(const Ast* x) {
      struct V : Ast::Visitor {
         const Token* result;
         V() : result() { }

         void visit(const LiteralAst& x) { result = x.token(); }
         void visit(const IdentifierAst& x) { result = x.token(); }
         void visit(const WildcardAst& x) { result = x.token(); }
         void visit(const OperatorAst& x) { result = x.token(); }
         void visit(const BracketAst& x) { result = &x.first(); }
         void visit(const EnclosureAst& x) { result = &x.bracket()->first(); }
         void visit(const BiSectionAst& x) {
            result = anchor(x.operation());
         }
         void visit(const LeftSectionAst& x) { result = anchor(x.lhs()); }
         void visit(const RightSectionAst& x) {
            result = anchor(x.operation());
         }
         void visit(const AssertAst& x) { result = &x.keyword(); }
         void visit(const RestrictAst& x) { result = anchor(x.expression()); }
         void visit(const UnaryAst& x) { result = anchor(x.operation()); }
         void visit(const BinaryAst& x) { result = anchor(x.lhs()); }
         void visit(const DotAst& x) { result = anchor(x.scope()); }
         void visit(const JuxtaposeAst& x) { result = anchor(x.operation()); }
         void visit(const IntervalAst& x) { result = anchor(x.start()); }
         void visit(const FilterAst& x) { result = anchor(x.expr()); }
         void visit(const ApplyAst& x) { result = anchor(x.operation()); }
         void visit(const ArrowAst& x) { result = anchor(x.source()); }
         void visit(const CollectAst& x) { result = anchor(x.body()); }
         void visit(const ParameterAst& x) { result = anchor(x.name()); }
         void visit(const SignatureAst& x) { result = anchor(x.form()); }
         void visit(const ProlongAst& x) { result = anchor(x.form()); }
         void visit(const DefinitionAst& x) { result = anchor(x.form()); }
         void visit(const PostulateAst& x) { result = anchor(x.form()); }
         void visit(const AliasAst& x) { result = anchor(x.alias()); }
         void visit(const ExPostulateAst& x) { result = x.name()->token(); }
         void visit(const AssociatedAst& x) { result = x.alias()->token(); }
         void visit(const RuleAst& x) { result = anchor(x.form()); }
         void visit(const ExprStmtAst& x) {
            if (auto y = x.expression())
               result = anchor(y);
            else
               result = &x.semicolon();
         }
         void visit(const MatchAst& x) { result = anchor(x.scrutinee()); }
         void visit(const CaseAst& x) { result = anchor(x.label()); }
         void visit(const IfAst& x) { result = anchor(x.condition()); }
         void visit(const RepeatAst& x) { result = anchor(x.body()); }
         void visit(const LeaveAst& x) { result = x.keyword(); }
         void visit(const ReturnAst& x) { result = x.keyword(); }
         void visit(const ThrowAst& x) { result = x.keyword(); }
         void visit(const CompoundAst& x) { result = anchor(x.at(0)); }
         void visit(const DatatypeAst& x) { result = &x.sort(); }
         void visit(const AssignmentAst& x) { result = anchor(x.lhs()); }
         void visit(const SequenceAst& x) { result = anchor(x.at(0)); }
         void visit(const QuantifiedAst& x) {
            result = anchor(x.quantifier());
         }
         void visit(const LambdaAst& x) { result = anchor(x.body()); }
         void visit(const WhereAst& x) { result = anchor(x.expression()); }
         void visit(const SourceFileAst& x) { result = &x.tokens.front(); }
         void visit(const PathAst& x) { result = anchor(x.dirname()); }
         void visit(const ImportAst& x) { result = anchor(x.path()); }
         void visit(const DescriptionAst& x) { result = &x.front(); }
      };

      V v;
      x->accept(v);
      return v.result;
   }
}
