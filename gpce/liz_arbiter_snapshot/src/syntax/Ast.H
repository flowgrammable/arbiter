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


// This file defines various Liz AST object representations.

#ifndef LIZ_AST_INCLUDED
#define LIZ_AST_INCLUDED

#include <vector>
#include <liz/storage>
#include <liz/Token>

namespace liz {
   using std::vector;

   // -- Ast --
   // General notion of Liz AST, base class of all AST data structures.
   struct Ast {
      struct Visitor;           // Base class for AST visitors.
      virtual void accept(Visitor&) const = 0;
   };

   // -- Helper class: override Ast::accept().
   template<typename T>
   struct ast_impl : Ast {
      void accept(Visitor&) const;
   };

   // -- DescriptionAst --
   struct DescriptionAst : ast_impl<DescriptionAst>, vector<Token> {
      explicit DescriptionAst(const vector<Token>&);
   };

   // -- Sequence ---
   // There are situations where an AST object consists of
   // sequence of ASTs obejcts.  That is the case for example
   // for compound statements, or function argument list.
   template<typename T>
   struct Sequence : std::vector<const T*> {
      // FIXME: Remove these when we have inherited constructors.
      Sequence() { }
      Sequence(const T* x) : std::vector<const T*>(1, x) { }
      explicit Sequence(std::size_t n, const T* x = nullptr)
            : std::vector<const T*>(n, x)
      { }
      Sequence(std::initializer_list<const T*> l)
            : std::vector<const T*>(l)
      { }

      Sequence& append(const Sequence& seq) {
         copy(seq.begin(), seq.end(), std::back_inserter(*this));
         return *this;
      }

      // convenience syntax in conditionals.
      explicit operator bool() const { return not this->empty(); }
   };

   using AstSequence = Sequence<Ast>;

   // Return the number of elements in this sequence.
   template<typename T>
   inline std::size_t
   length(const Sequence<T>& seq) {
      return seq.size();
   }

   // -- Atomic --
   // This is the representation of atomic syntactic objects such
   // as integer literals, etc.
   struct Atomic : structure::unary<Token> {
      explicit Atomic(const Token&);
      const Token* token() const { return &operand(); }
   };

   // -- LiteralAst --
   // Representation of a literal value in the input program source.
   struct LiteralAst : ast_impl<LiteralAst>, Atomic {
      explicit LiteralAst(const Token&);
   };

   // -- IdentifierAst --
   // Representation of identifiers in input program source.
   struct IdentifierAst : ast_impl<IdentifierAst>, Atomic {
      explicit IdentifierAst(const Token&);
   };

   // -- WildcardAst --
   struct WildcardAst : ast_impl<WildcardAst>, Atomic {
      explicit WildcardAst(const Token&);
   };
   
   // -- OperatorAst --
   // Representation of an operator name.
   struct OperatorAst : ast_impl<OperatorAst>, Atomic {
      explicit OperatorAst(const Token&);
   };

   // -- BracketAst --
   struct BracketAst : ast_impl<BracketAst>, structure::binary<Token> {
      BracketAst(const Token&, const Token&);
   };

   // -- BiSectionAst --
   struct BiSectionAst : ast_impl<BiSectionAst>,
                         structure::unary<const OperatorAst*> {
      explicit BiSectionAst(const OperatorAst*);
      const OperatorAst* operation() const { return operand(); }
   };

   // -- LeftSectionAst --
   struct LeftSectionAst : ast_impl<LeftSectionAst>,
                           structure::binary<const Ast*, const OperatorAst*> {
      LeftSectionAst(const Ast*, const OperatorAst*);
      const Ast* lhs() const { return first(); }
      const OperatorAst* operation() const { return second(); }
   };

   // -- RightSectionAst --
   struct RightSectionAst : ast_impl<RightSectionAst>,
                            structure::binary<const OperatorAst*, const Ast*> {
      RightSectionAst(const OperatorAst*, const Ast*);
      const OperatorAst* operation() const { return first(); }
      const Ast* rhs() const { return second(); }
   };

   // -- DotAst --
   struct DotAst : ast_impl<DotAst>, structure::binary<const Ast*> {
      DotAst(const Ast*, const Ast*);
      const Ast* scope() const { return first(); }
      const Ast* member() const { return second(); }
   };

   // -- EnclosureAst --
   struct EnclosureAst : ast_impl<EnclosureAst>,
                         structure::binary<const BracketAst*, const Ast*> {
      EnclosureAst(const BracketAst*, const Ast*);
      const BracketAst* bracket() const { return first(); }
      const Ast* expr() const { return second(); }
   };

   // -- JuxtaposeAst --
   struct JuxtaposeAst : ast_impl<JuxtaposeAst>,
                         structure::binary<const Ast*> {
      JuxtaposeAst(const Ast*, const Ast*);
      const Ast* operation() const { return first(); }
      const Ast* argument() const { return second(); }
   };

   // -- AssertAst --
   struct AssertAst : ast_impl<AssertAst>,
                      structure::binary<Token, const Ast*> {
      AssertAst(const Token&, const Ast*);
      const Token& keyword() const { return first(); }
      const Ast* predicate() const { return second(); }
   };

   // -- RestrictAst --
   struct RestrictAst : ast_impl<RestrictAst>, structure::binary<const Ast*> {
      RestrictAst(const Ast*, const Ast*);
      const Ast* expression() const { return first(); }
      const Ast* type() const { return second(); }
   };

   // -- UnaryAst --
   // This datatype represents unary expression ASTs.
   struct UnaryAst : ast_impl<UnaryAst>,
                     structure::binary<const OperatorAst*, const Ast*> {
      UnaryAst(const OperatorAst*, const Ast*);
      const OperatorAst* operation() const { return first(); }
      const Ast* argument() const { return second(); }
   };

   // -- BinaryAst --
   // Representation of binary expression ASTs.
   struct BinaryAst : ast_impl<BinaryAst>,
                      structure::ternary<const OperatorAst*, const Ast*> {
      BinaryAst(const OperatorAst*, const Ast*, const Ast*);
      const OperatorAst* operation() const { return first(); }
      const Ast* lhs() const { return second(); }
      const Ast* rhs() const { return third(); }
   };

   // -- FilterAst --
   struct FilterAst : ast_impl<FilterAst>, structure::binary<const Ast*> {
      FilterAst(const Ast*, const Ast*);
      const Ast* expr() const { return first(); }
      const Ast* condition() const { return second(); }
   };

   // --------------
   // -- ArrowAst --
   // --------------
   struct ArrowAst : ast_impl<ArrowAst>, structure::binary<const Ast*> {
      ArrowAst(const Ast*, const Ast*);
      const Ast* source() const { return first(); }
      const Ast* target() const { return second(); }
   };

   // -----------
   // -- IfAst --
   // Representation an `if' statement.
   struct IfAst : ast_impl<IfAst>, structure::ternary<const Ast*> {
      IfAst(const Ast*, const Ast*, const Ast*);
      const Ast* condition() const { return first(); }
      const Ast* consequence() const { return second(); }
      const Ast* alternate() const { return third(); }
   };

   // -- ApplyAst --
   // Representation of a call.
   struct ApplyAst : ast_impl<ApplyAst>,
                     structure::binary<const Ast*, const EnclosureAst*> {
      ApplyAst(const Ast*, const EnclosureAst*);
      const Ast* operation() const { return first(); }
      const EnclosureAst* arguments() const { return second(); }
   };

   // -- ExprStmtAst --
   struct ExprStmtAst : ast_impl<ExprStmtAst>,
                        structure::binary<Token, const Ast*> {
      ExprStmtAst(const Token&, const Ast*);
      const Token& semicolon() const { return first(); }
      const Ast* expression() const { return second(); }
   };

   // -- ParameterAst --
   // Representation of parameter declaration.
   struct ParameterAst : ast_impl<ParameterAst>,
                         structure::ternary<const IdentifierAst*, const Ast*> {
      ParameterAst(const IdentifierAst*, const Ast*, const Ast*);
      const IdentifierAst* name() const { return first(); }
      const Ast* type() const { return second(); }
      const Ast* initializer() const { return third(); }
   };

   using Parameters = Sequence<ParameterAst>;

   // -- FixityForm --
   struct FixityForm {
      struct Visitor;
      virtual void accept(Visitor&) const = 0;
   };

   // -- DeclarativeAst --
   struct DeclarativeAst : Ast {
      virtual const FixityForm* form() const = 0;
      virtual const Ast* type() const = 0;
   };

   template<typename T>
   struct declarative : DeclarativeAst {
      void accept(Visitor&) const;
   };

   // -- WhereAst --
   struct WhereAst : ast_impl<WhereAst>,
                     structure::binary<const Ast*, Sequence<DeclarativeAst>> {
      WhereAst(const Ast*, const Sequence<DeclarativeAst>&);
      const Ast* expression() const { return first(); }
      const Sequence<DeclarativeAst>& locals() const { return second(); }
   };

   // -- SignatureAst --
   struct SignatureAst : declarative<SignatureAst>,
                         structure::ternary<const FixityForm*, const Ast*> {
      SignatureAst(const FixityForm*, const Ast*, const Ast*);
      const FixityForm* form() const;
      const Ast* type() const;
      const Ast* implementation() const { return third(); }
   };

   // -- DatatypeAst --
   struct DatatypeAst : ast_impl<DatatypeAst>,
                        structure::binary<Token, Sequence<SignatureAst>> {
      DatatypeAst(const Token&, const Sequence<SignatureAst>&);
      const Token& sort() const { return first(); }
      const Sequence<SignatureAst>& members() const { return second(); }
   };

   // -- IntervalAst --
   struct IntervalAst : ast_impl<IntervalAst>, structure::binary<const Ast*> {
      IntervalAst(const Ast*, const Ast*);
      const Ast* start() const { return first(); }
      const Ast* finish() const { return second(); }
   };

   // -- AssignmentAst --
   struct AssignmentAst : ast_impl<AssignmentAst>,
                          structure::binary<const Ast*> {
      AssignmentAst(const Ast*, const Ast*);
      const Ast* lhs() const { return first(); }
      const Ast* rhs() const { return second(); }
   };

   template<typename T>
   struct fixity : FixityForm {
      void accept(Visitor&) const;
   };

   // -- AlphabethicForm --
   struct AlphabethicForm : fixity<AlphabethicForm>, Atomic {
      explicit AlphabethicForm(const Token&);
   };

   // -- OperatorForm --
   struct OperatorForm : fixity<OperatorForm>, Atomic {
      explicit OperatorForm(const Token&);
   };

   // -- LiteralForm --
   struct LiteralForm : fixity<LiteralForm>, Atomic {
      explicit LiteralForm(const Token&);
   };

   // -- PrefixForm --
   struct PrefixForm : fixity<PrefixForm>,
                       structure::binary<const OperatorAst*,
                                         const ParameterAst*> {
      PrefixForm(const OperatorAst*, const ParameterAst*);
      const OperatorAst* operation() const { return first(); }
      const ParameterAst* parameter() const { return second(); }
   };

   // -- SuffixForm --
   struct SuffixForm : fixity<SuffixForm>,
                       structure::binary<const OperatorAst*,
                                         const ParameterAst*> {
      SuffixForm(const OperatorAst*, const ParameterAst*);
      const OperatorAst* operation() const { return first(); }
      const ParameterAst* parameter() const { return second(); }
   };

   // -- InfixForm --
   struct InfixForm : fixity<InfixForm>,
                      structure::ternary<const OperatorAst*,
                                         const ParameterAst*> {
      InfixForm(const OperatorAst*, const ParameterAst*, const ParameterAst*);
      const OperatorAst* operation() const { return first(); }
      const ParameterAst* lhs() const { return second(); }
      const ParameterAst* rhs() const { return third(); }
   };

   // -- ClosedForm --
   struct ClosedForm : fixity<ClosedForm>,
                       structure::binary<const BracketAst*, Parameters> {
      ClosedForm(const BracketAst*, const Parameters&);
      const BracketAst* bracket() const { return first(); }
      const Parameters& parameters() const { return second(); }
   };

   // -- CallForm --
   struct CallForm : fixity<CallForm>,
                     structure::ternary<Token, Parameters, const Ast*> {
      CallForm(const Token&, const Parameters&, const Ast*);
      const Token& function() const { return first(); }
      const Parameters& parameters() const { return second(); }
      const Ast* restriction() const { return third(); }
   };

   // -- DefinitionAst --
   struct DefinitionAst : declarative<DefinitionAst>,
                          structure::ternary<const FixityForm*, const Ast*> {
      DefinitionAst(const FixityForm*, const Ast*, const Ast*);
      const FixityForm* form() const;
      const Ast* type() const;
      const Ast* initializer() const { return third(); }
   };

   // -- RuleAst --
   struct RuleAst : declarative<RuleAst>,
                    structure::ternary<const FixityForm*, const Ast*> {
      RuleAst(const FixityForm*, const Ast*, const Ast*);
      const FixityForm* form() const;
      const Ast* type() const;
      const Ast* initializer() const { return third(); }
   };

   // -- ProlongAst --
   struct ProlongAst : declarative<ProlongAst>,
                       structure::ternary<const FixityForm*, const Ast*> {
      ProlongAst(const FixityForm*, const Ast*, const Ast*);
      const FixityForm* form() const;
      const Ast* type() const;
      const Ast* extension() const { return third(); }
   };

   // -- PostulateAst --
   struct PostulateAst : declarative<PostulateAst>,
                         structure::binary<const FixityForm*, const Ast*> {
      PostulateAst(const FixityForm*, const Ast*);
      const FixityForm* form() const;
      const Ast* type() const;
   };

   // -- AliasAst --
   struct AliasAst : ast_impl<AliasAst>,
                     structure::binary<const IdentifierAst*, const Ast*> {
      AliasAst(const IdentifierAst*, const Ast*);
      const IdentifierAst* alias() const { return first(); }
      const Ast* value() const { return second(); }
   };

   // -- ExPostulateAst --
   struct ExPostulateAst : ast_impl<ExPostulateAst>,
                           structure::ternary<const IdentifierAst*,
                              const IdentifierAst*, const Ast*> {
      ExPostulateAst(const IdentifierAst*, const IdentifierAst*, const Ast*);
      const IdentifierAst* name() const;
      const IdentifierAst* type() const;
      const Ast* constraint() const;
   };

   // -- AssociatedAst --
   struct AssociatedAst : ast_impl<AssociatedAst>,
                          structure::binary<const IdentifierAst*,
                                            const IdentifierAst*> {
      AssociatedAst(const IdentifierAst*, const IdentifierAst*);
      const IdentifierAst* alias() const { return first(); }
      const IdentifierAst* value() const { return second(); }
   };

   // -- AbstractionAst --
   template<typename T>
   struct AbstractionAst : structure::binary<Parameters, T> {
      AbstractionAst(const Parameters& p, const T& x)
            : structure::binary<Parameters, T>(p, x) { }
      const Parameters& parameters() const { return this->first(); }
      const ParameterAst* parameter(int i) const { return parameters().at(i); }
      const T& body() const { return this->second(); }
   };
   
   // -- QuantifiedAst --
   // Representation of a quantified proposition.
   struct QuantifiedAst : ast_impl<QuantifiedAst>,
                          AbstractionAst<const Ast*> {
      QuantifiedAst(const OperatorAst*, const Parameters&, const Ast*,
                    const Ast*);
      const OperatorAst* quantifier() const { return quant; }
      const Ast* constraint() const { return constraint_; }
   private:
      const OperatorAst* quant;
      const Ast* constraint_;
   };

   // -- LambdaAst --
   // A lambda-abstracted expressions.
   struct LambdaAst : ast_impl<LambdaAst>, AbstractionAst<const Ast*> {
      LambdaAst(const Parameters&, const Ast*);
   };

   // --- Iterator --
   struct Iterator {
      struct Visitor;
      virtual void accept(Visitor&) const = 0;
   };

   template<typename>
   struct iter_impl : Iterator {
      void accept(Visitor&) const;
   };

   // -- ForIterator --
   struct ForIterator : iter_impl<ForIterator>,
                        structure::binary<const IdentifierAst*, const Ast*> {
      ForIterator(const IdentifierAst*, const Ast*);
      const IdentifierAst* variable() const { return first(); }
      const Ast* sequence() const { return second(); }
   };

   // -- WhileIterator --
   // Representation of a while-iterator.
   struct WhileIterator : iter_impl<WhileIterator>,
                          structure::unary<const Ast*> {
      explicit WhileIterator(const Ast*);
      const Ast* condition() const { return operand(); }
   };

   // -- UntilIterator --
   struct UntilIterator : iter_impl<UntilIterator>,
                          structure::unary<const Ast*> {
      explicit UntilIterator(const Ast*);
      const Ast* condition() const { return operand(); }
   };

   // -- ProvisoIterator --
   struct ProvisoIterator : iter_impl<ProvisoIterator>,
                            structure::unary<const Ast*> {
      explicit ProvisoIterator(const Ast*);
      const Ast* condition() const { return operand(); }
   };

   // -- Iterator::Visitor --
   struct Iterator::Visitor {
      virtual void visit(const ForIterator&) = 0;
      virtual void visit(const WhileIterator&) = 0;
      virtual void visit(const UntilIterator&) = 0;
      virtual void visit(const ProvisoIterator&) = 0;
   };

   // -- CollectAst --
   struct CollectAst : ast_impl<CollectAst>,
                       structure::binary<Sequence<Iterator>, const Ast*> {
      CollectAst(const Sequence<Iterator>&, const Ast*);
      const Sequence<Iterator>& iterators() const { return first(); }
      const Ast* body() const { return second(); }
   };

   // -- RepeatAst --
   struct RepeatAst : ast_impl<RepeatAst>,
                      structure::binary<Sequence<Iterator>, const Ast*> {
      RepeatAst(const Sequence<Iterator>&, const Ast*);
      const Sequence<Iterator>& iterators() const { return first(); }
      const Ast* body() const { return second(); }
   };

   // -- CaseAst --
   // Representation of a case-statement.
   struct CaseAst : ast_impl<CaseAst>, structure::binary<const Ast*> {
      CaseAst(const Ast*, const Ast*);
      const Ast* label() const { return first(); }
      const Ast* statement() const { return second(); }
   };

   // -- MatchAst --
   // Representation of a switch-statement.
   struct MatchAst : ast_impl<MatchAst>,
                      structure::binary<const Ast*, Sequence<CaseAst>> {
      MatchAst(const Ast*, const Sequence<CaseAst>&);
      const Ast* scrutinee() const { return first(); }
      const Sequence<CaseAst>& branches() const { return second(); }
   };

   // -- ReturnAst --
   struct ReturnAst : ast_impl<ReturnAst>,
                      structure::binary<Token, const Ast*> {
      ReturnAst(const Token&, const Ast*);
      const Token* keyword() const { return &first(); }
      const Ast* expression() const { return second(); }
   };

   // -- LeaveAst --
   struct LeaveAst : ast_impl<LeaveAst>,
                     structure::binary<Token, const Ast*> {
      LeaveAst(const Token&, const Ast*);
      const Token* keyword() const { return &first(); }
      const Ast* expression() const { return second(); }
   };

   // -- ThrowAst --
   struct ThrowAst : ast_impl<ThrowAst>,
                     structure::binary<Token, const Ast*> {
      ThrowAst(const Token&, const Ast*);
      const Token* keyword() const { return &first(); }
      const Ast* expression() const { return second(); }
   };

   // -- ListAst --
   // Representation of either a compound statement of expression list.
   struct List : structure::unary<AstSequence> {
      using const_iterator = AstSequence::const_iterator;
      explicit List(const AstSequence&);
      const Ast* at(int i) const { return sequence().at(i); }
      const AstSequence& sequence() const { return operand(); }
      const_iterator begin() const { return sequence().begin(); }
      const_iterator end() const { return sequence().end(); }
   };

   // Return the number of element in a list of asts.
   inline std::size_t
   length(const List* x) {
      return x == nullptr ? 0 : x->sequence().size();
   }

   // -- CompoundAst --
   // Representation of compound statements.
   struct CompoundAst : ast_impl<CompoundAst>, List {
      CompoundAst(const AstSequence&);
   };

   // -- SequenceAst --
   // Representation of sequence of AST objects as an independent AST object.
   struct SequenceAst : ast_impl<SequenceAst>, List {
      explicit SequenceAst(const AstSequence&);
   };

   // -- Fragment --
   struct Fragment {
      input::Source source;
      TokenStream tokens;
      AstSequence asts;
   };

   // -- SourceFileAst --
   struct SourceFileAst : ast_impl<SourceFileAst>, Fragment {
      const std::string path;

      explicit SourceFileAst(const std::string&);
   };

   // -- PathAst --
   struct PathAst : ast_impl<PathAst>,
                    structure::binary<const IdentifierAst*, const Ast*> {
      PathAst(const IdentifierAst*, const Ast*);
      const IdentifierAst* dirname() const { return first(); }
      const Ast* subpath() const { return second(); }
   };

   // -- ImportAst --
   struct ImportAst : ast_impl<ImportAst>, structure::unary<const Ast*> {
      ImportAst(const Ast*);
      const Ast* path() const { return operand(); }
   };

   // Type recovery functions.  These functions perform a type
   // safety conversion to the `real' type of the AST object.
   // Otherwise, raise an exception.

   template<typename T>
   const T* is(const Ast* x) {
      return dynamic_cast<const T*>(x);
   }
   
   template<typename T>
   const T& get(const Ast* x) {
      return dynamic_cast<const T&>(*x);
   }
   
   // Print a prefix form (i.e. s-expression) representation of
   // the AST object onto the output stream.
   std::ostream& prefix_form(std::ostream&, const Ast*);
   
   // Same as prefix_form, except that it renders the prefix form
   // as a string object.
   std::string show(const Ast*);
   std::string show(const AstSequence*);

   // -- Ast::Visitor --
   struct Ast::Visitor {
      virtual void visit(const LiteralAst&) = 0;
      virtual void visit(const IdentifierAst&) = 0;
      virtual void visit(const WildcardAst&) = 0;
      virtual void visit(const OperatorAst&) = 0;
      virtual void visit(const BracketAst&) = 0;
      virtual void visit(const EnclosureAst&) = 0;
      virtual void visit(const BiSectionAst&) = 0;
      virtual void visit(const LeftSectionAst&) = 0;
      virtual void visit(const RightSectionAst&) = 0;
      virtual void visit(const AssertAst&) = 0;
      virtual void visit(const RestrictAst&) = 0;
      virtual void visit(const UnaryAst&) = 0;
      virtual void visit(const BinaryAst&) = 0;
      virtual void visit(const DotAst&) = 0;
      virtual void visit(const JuxtaposeAst&) = 0;
      virtual void visit(const IntervalAst&) = 0;
      virtual void visit(const FilterAst&) = 0;
      virtual void visit(const ApplyAst&) = 0;
      virtual void visit(const ArrowAst&) = 0;
      virtual void visit(const CollectAst&) = 0;
      virtual void visit(const ParameterAst&) = 0;
      virtual void visit(const SignatureAst&) = 0;
      virtual void visit(const DefinitionAst&) = 0;
      virtual void visit(const ProlongAst&) = 0;
      virtual void visit(const PostulateAst&) = 0;
      virtual void visit(const AliasAst&) = 0;
      virtual void visit(const ExPostulateAst&) = 0;
      virtual void visit(const AssociatedAst&) = 0;
      virtual void visit(const RuleAst&) = 0;
      virtual void visit(const ExprStmtAst&) = 0;
      virtual void visit(const MatchAst&) = 0;
      virtual void visit(const CaseAst&) = 0;
      virtual void visit(const IfAst&) = 0;
      virtual void visit(const RepeatAst&) = 0;
      virtual void visit(const LeaveAst&) = 0;
      virtual void visit(const ReturnAst&) = 0;
      virtual void visit(const ThrowAst&) = 0;
      virtual void visit(const CompoundAst&) = 0;
      virtual void visit(const DatatypeAst&) = 0;
      virtual void visit(const AssignmentAst&) = 0;
      virtual void visit(const SequenceAst&) = 0;
      virtual void visit(const QuantifiedAst&) = 0;
      virtual void visit(const LambdaAst&) = 0;
      virtual void visit(const WhereAst&) = 0;
      virtual void visit(const SourceFileAst&) = 0;
      virtual void visit(const PathAst&) = 0;
      virtual void visit(const ImportAst&) = 0;
      virtual void visit(const DescriptionAst&) = 0;
   };

   // -- AstFactory --
   // This class is responsible for creating Liz ASTs node objects.
   struct AstFactory {
      const DescriptionAst* make_description(const vector<Token>&);
      // creaate an ast object for a parsed input source file.
      SourceFileAst* make_source_file(const std::string&);
      // create an AST object for a literal value
      const LiteralAst* make_literal(const Token&);
      // create an identifier ast
      const IdentifierAst* make_identifier(const Token&);
      const WildcardAst* make_wildcard(const Token&);
      // create an operator name ast
      const OperatorAst* make_operator(const Token&);
      const BracketAst* make_bracket(const Token&, const Token&);
      const BiSectionAst* make_bisection(const OperatorAst*);
      const LiteralForm* make_literal_form(const Token&);
      const AlphabethicForm* make_alphabetic_form(const Token&);
      const OperatorForm* make_operator_form(const Token&);
      const LeftSectionAst* make_left_section(const Ast*, const OperatorAst*);
      const RightSectionAst* make_right_section(const OperatorAst*, const Ast*);
      const AssertAst* make_assert(const Token&, const Ast*);
      const RestrictAst* make_restrict(const Ast*, const Ast*);
      const DotAst* make_dot(const Ast*, const Ast*);
      const JuxtaposeAst* make_juxtapose(const Ast*, const Ast*);
      // create a parenthesized expression list
      const EnclosureAst* make_enclosure(const BracketAst*, const Ast*);
      // create unary expressions
      const UnaryAst* make_ast(const OperatorAst*, const Ast*);
      // create bianry expressions
      const BinaryAst* make_ast(const OperatorAst*, const Ast*, const Ast*);
      const FilterAst* make_filter(const Ast*, const Ast*);
      // create an AST object for an `if' statement.
      const IfAst* make_if(const Ast*, const Ast*, const Ast*);
      // create an AST object for a function call
      const ApplyAst* make_apply(const Ast*, const EnclosureAst*);
      const ExprStmtAst* make_expr_stmt(const Token&, const Ast*);
      const IntervalAst* make_interval(const Ast*, const Ast*);
      const AssignmentAst* make_assignment(const Ast*, const Ast*);
      // create an AST for a quantified logical formula
      const QuantifiedAst*
      make_quantified(const OperatorAst*, const Parameters&, const Ast*,
                      const Ast*);
      // create a lambda abstraction AST
      const LambdaAst* make_lambda(const Parameters&, const Ast*);
      const ArrowAst* make_arrow(const Ast*, const Ast*);
      // create an AST object for a `case' statement.
      const CaseAst* make_case(const Ast*, const Ast*);
      // create an AST for a `switch' statement.
      const MatchAst* make_match(const Ast*, const Sequence<CaseAst>&);
      const ForIterator* make_for(const IdentifierAst*, const Ast*);
      const WhileIterator* make_while(const Ast*);
      const UntilIterator* make_until(const Ast*);
      const ProvisoIterator* make_proviso(const Ast*);
      const RepeatAst* make_repeat(const Sequence<Iterator>&, const Ast*);
      const ReturnAst* make_return(const Token&, const Ast*);
      const LeaveAst* make_leave(const Token&, const Ast*);
      const ThrowAst* make_throw(const Token&, const Ast*);
      const CollectAst* make_collect(const Sequence<Iterator>&, const Ast*);
      const WhereAst* make_where(const Ast*, const Sequence<DeclarativeAst>&);
      // create a container of AST objects.
      const SequenceAst* make_sequence(const AstSequence&);
      // create an AST for a compound statement.
      const CompoundAst* make_compound(const AstSequence&);
      const CallForm*
      make_call_form(const Token&, const Parameters&, const Ast*);
      const ClosedForm* make_closed_form(const BracketAst*, const Parameters&);
      const PrefixForm* make_prefix_form(const OperatorAst*, const ParameterAst*);
      const SuffixForm* make_suffix_form(const OperatorAst*, const ParameterAst*);
      const InfixForm* make_infix_form(const OperatorAst*,
                                       const ParameterAst*,
                                       const ParameterAst*);
      const DefinitionAst*
      make_definition(const FixityForm*, const Ast*, const Ast*);
      const RuleAst* make_rule(const FixityForm*, const Ast*, const Ast*);
      const SignatureAst* make_signature(const FixityForm*, const Ast*,
                                         const Ast* = nullptr);
      const DatatypeAst* make_datatype(const Token&, const Sequence<SignatureAst>&);
      const PostulateAst* make_postulate(const FixityForm*, const Ast*);
      const AliasAst* make_alias(const IdentifierAst*, const Ast*);
      const ExPostulateAst*
      make_expostulate(const IdentifierAst*, const IdentifierAst*, const Ast*);
      const AssociatedAst*
      make_associated(const IdentifierAst*, const IdentifierAst*);
      const ProlongAst* make_prolong(const FixityForm*, const Ast*, const Ast*);
      // create an AST for a parameter declaration.
      const ParameterAst*
      make_parameter(const IdentifierAst*, const Ast*, const Ast* = nullptr);
      const PathAst* make_path(const IdentifierAst*, const Ast*);
      const ImportAst* make_import(const Ast*);
   private:
      Factory<DescriptionAst> descs;
      Factory<SourceFileAst> srcs;
      Factory<LiteralAst> lits;
      Factory<IdentifierAst> ids;
      Factory<WildcardAst> wilds;
      Factory<OperatorAst> ops;
      Factory<BracketAst> brackets;
      Factory<LiteralForm> litforms;
      Factory<AlphabethicForm> alphaforms;
      Factory<OperatorForm> operforms;
      Factory<ClosedForm> closeds;
      Factory<BiSectionAst> bisects;
      Factory<LeftSectionAst> lsects;
      Factory<RightSectionAst> rsects;
      Factory<AssertAst> asserts;
      Factory<DotAst> dots;
      Factory<JuxtaposeAst> juxtaposes;
      Factory<EnclosureAst> encs;
      Factory<RestrictAst> restricts;
      Factory<UnaryAst> uns;
      Factory<BinaryAst> bis;
      Factory<FilterAst> fltrs;
      Factory<IfAst> ifs;
      Factory<ApplyAst> calls;
      Factory<ArrowAst> arrows;
      Factory<IntervalAst> intvls;
      Factory<ExprStmtAst> exprs;
      Factory<CaseAst> cases;
      Factory<MatchAst> sws;
      Factory<ForIterator> fors;
      Factory<WhileIterator> wls;
      Factory<UntilIterator> ntls;
      Factory<ProvisoIterator> pros;
      Factory<RepeatAst> rpts;
      Factory<ReturnAst> returns;
      Factory<LeaveAst> leaves;
      Factory<ThrowAst> throws;
      Factory<CollectAst> clls;
      Factory<CompoundAst> cmps;
      Factory<DatatypeAst> datatypes;
      Factory<AssignmentAst> assgns;
      Factory<SequenceAst> seqs;
      Factory<ParameterAst> parms;
      Factory<CallForm> forms;
      Factory<PrefixForm> prefixes;
      Factory<SuffixForm> suffixes;
      Factory<InfixForm> infixes;
      Factory<DefinitionAst> defs;
      Factory<ProlongAst> prolongs;
      Factory<PostulateAst> pos;
      Factory<AliasAst> aliases;
      Factory<ExPostulateAst> expos;
      Factory<AssociatedAst> assocs;
      Factory<RuleAst> rules;
      Factory<SignatureAst> sigs;
      Factory<QuantifiedAst> quants;
      Factory<WhereAst> wheres;
      Factory<LambdaAst> lams;
      Factory<PathAst> paths;
      Factory<ImportAst> imports;
   };

   template<typename T>
   void
   ast_impl<T>::accept(Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   declarative<T>::accept(Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   iter_impl<T>::accept(Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename F>
   struct ast_visitor : Ast::Visitor, F {
      template<typename... Args>
      ast_visitor(Args... args) : F{args...} { }

      void visit(const LiteralAst& x) { (*this)(x); }
      void visit(const IdentifierAst& x) { (*this)(x); }
      void visit(const WildcardAst& x) { (*this)(x); }
      void visit(const OperatorAst& x) { (*this)(x); }
      void visit(const BracketAst& x) { (*this)(x); }
      void visit(const EnclosureAst& x) { (*this)(x); }
      void visit(const BiSectionAst& x) { (*this)(x); }
      void visit(const LeftSectionAst& x) { (*this)(x); }
      void visit(const RightSectionAst& x) { (*this)(x); }
      void visit(const AssertAst& x) { (*this)(x); }
      void visit(const RestrictAst& x) { (*this)(x); }
      void visit(const UnaryAst& x) { (*this)(x); }
      void visit(const BinaryAst& x) { (*this)(x); }
      void visit(const DotAst& x) { (*this)(x); }
      void visit(const JuxtaposeAst& x) { (*this)(x); }
      void visit(const ArrowAst& x) { (*this)(x); }
      void visit(const IntervalAst& x) { (*this)(x); }
      void visit(const FilterAst& x) { (*this)(x); }
      void visit(const ApplyAst& x) { (*this)(x); }
      void visit(const CollectAst& x) { (*this)(x); }
      void visit(const ParameterAst& x) { (*this)(x); }
      void visit(const SignatureAst& x) { (*this)(x); }
      void visit(const DefinitionAst& x) { (*this)(x); }
      void visit(const ProlongAst& x) { (*this)(x); }
      void visit(const PostulateAst& x) { (*this)(x); }
      void visit(const AliasAst& x) { (*this)(x); }
      void visit(const ExPostulateAst& x) { (*this)(x); }
      void visit(const AssociatedAst& x) { (*this)(x); }
      void visit(const RuleAst& x) { (*this)(x); }
      void visit(const ExprStmtAst& x) { (*this)(x); }
      void visit(const MatchAst& x) { (*this)(x); }
      void visit(const CaseAst& x) { (*this)(x); }
      void visit(const IfAst& x) { (*this)(x); }
      void visit(const RepeatAst& x) { (*this)(x); }
      void visit(const LeaveAst& x) { (*this)(x); }
      void visit(const ReturnAst& x) { (*this)(x); }
      void visit(const ThrowAst& x) { (*this)(x); }
      void visit(const CompoundAst& x) { (*this)(x); }
      void visit(const DatatypeAst& x) { (*this)(x); }
      void visit(const AssignmentAst& x) { (*this)(x); }
      void visit(const SequenceAst& x) { (*this)(x); }
      void visit(const QuantifiedAst& x) { (*this)(x); }
      void visit(const LambdaAst& x) { (*this)(x); }
      void visit(const WhereAst& x) { (*this)(x); }
      void visit(const SourceFileAst& x) { (*this)(x); }
      void visit(const PathAst& x) { (*this)(x); }
      void visit(const ImportAst& x) { (*this)(x); }
      void visit(const DescriptionAst& x) { (*this)(x); }
   };

   // -- FixityForm::Visitor --
   struct FixityForm::Visitor {
      virtual void visit(const LiteralForm&) = 0;
      virtual void visit(const OperatorForm&) = 0;
      virtual void visit(const AlphabethicForm&) = 0;
      virtual void visit(const PrefixForm&) = 0;
      virtual void visit(const SuffixForm&) = 0;
      virtual void visit(const InfixForm&) = 0;
      virtual void visit(const ClosedForm&) = 0;
      virtual void visit(const CallForm&) = 0;
   };

   template<typename T>
   void
   fixity<T>::accept(Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }
      
   const Token* anchor(const Ast*);
   const Token* anchor(const FixityForm*);
}


#endif  // LIZ_AST_INCLUDED
