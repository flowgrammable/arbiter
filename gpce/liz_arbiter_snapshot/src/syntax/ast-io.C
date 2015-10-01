// Copyright (C) 2009-2013, Texas A&M University.
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

#include <algorithm>
#include <iterator>
#include <liz/Ast>
#include "Output.H"
#include "sexpr-io.H"

namespace liz {
   using namespace io;

   static Output& sexpr(Output& out, const Token& t) {
      std::copy(begin(t), end(t), std::ostream_iterator<char>(out.stream()));
      return out;
   }

   static void
   sexpr(io::Output& out, const BracketAst& x) {
      out << '(' << "%bracket" << ' ';
      sexpr(out, x.first());
      sexpr(out, x.second());
      out << ')';
   }
   

   static Output& sexpr(Output& out, const Ast* x);

   template<typename T>
   static Output&
   sexpr(Output& out, const vector<T>& seq) {
      const std::size_t n = seq.size();
      if (n == 0)
         return out;
      out.nibble(tab_unit);
      for (std::size_t i = 0; i < seq.size(); ++i) {
         out << newline() << indent();
         sexpr(out, seq[i]);
      }
      return out.nibble(-tab_unit);
   }

   template<typename T>
   static void
   sexpr(Output& out, const char* t, const vector<T>& seq) {
      out << '(' << t;
      sexpr(out, seq);
      out << ')';
   }

   template<typename T>
   static Output&
   sexpr(Output& out, const vector<T>* seq) {
      if (seq == nullptr)
         out << '(' << ')';
      else
         sexpr(out, *seq);
      return out;
   }

   static void
   sexpr(Output& out, const Iterator* i) {
      struct V : Iterator::Visitor {
         Output& out;
         V(Output& o) : out(o) { }

         void visit(const WhileIterator& i) {
            out << '(' << "%while" << ' ';
            sexpr(out, i.condition());
            out << ')';
         }

         void visit(const UntilIterator& i) {
            out << '(' << "%until" << ' ';
            sexpr(out, i.condition());
            out << ')';
         }

         void visit(const ProvisoIterator& i) {
            out << '(' << "%proviso" << ' ';
            sexpr(out, i.condition());
            out << ')';
         }

         void visit(const ForIterator& i) {
            out << '(' << "%for" << ' ';
            sexpr(out, i.variable());
            out << ' ';
            sexpr(out, i.sequence());
            out << ')';
         }
      };

      V v(out);
      i->accept(v);
   }

   template<typename T>
   static bool is_atomic(const vector<T>& x) {
      return x.empty();
   }

   template<typename T>
   static void
   sexpr(Output& out, const char* tag, const structure::unary<T>& x) {
      out << '(' << tag << ' ';
      sexpr(out, x.operand());
      out << ')';
   }

   template<typename U, typename V>
   static void
   sexpr(Output& out, const char* tag, const structure::binary<U, V>& x) {
      out << '(' << tag;
      out.nibble(tab_unit) << newline() << indent();
      sexpr(out, x.first());
      out << newline() << indent();
      sexpr(out, x.second());
      out << ')';
      out.nibble(-tab_unit) << newline() << indent();
   }

   template<typename U, typename V, typename W>
   static void
   sexpr(Output& out, const char* tag, const structure::ternary<U, V, W>& x) {
      out << '(' << tag;
      out.nibble(tab_unit) << newline() << indent();
      sexpr(out, x.first());
      out << newline() << indent();
      sexpr(out, x.second());
      out << newline() << indent();
      sexpr(out, x.third());
      out << ')';
      out.nibble(-tab_unit) << newline() << indent();
   }

   static void
   sexpr(io::Output& out, const FixityForm* x) {
      struct V : FixityForm::Visitor {
         io::Output& out;
         V(io::Output& o) : out(o) { }
         void visit(const LiteralForm& x) { sexpr(out, "%lit", x); }
         void visit(const AlphabethicForm& x) { sexpr(out, "%alpha", x); }
         void visit(const OperatorForm& x) { sexpr(out, "%operator", x); }
         void visit(const PrefixForm& x) { sexpr(out, "%prefix", x); }
         void visit(const SuffixForm& x) { sexpr(out, "%suffix", x); }
         void visit(const InfixForm& x) { sexpr(out, "%infix", x); }
         void visit(const ClosedForm& x) { sexpr(out, "%closed", x); }
         void visit(const CallForm& x) { sexpr(out, "%call", x); }
      };
      V v{ out };
      x->accept(v);
   }

   static void
   sexpr(Output& out, const ExprStmtAst& x) {
      out << '(' << "%stmt";
      out.nibble(tab_unit) << newline() << indent();
      sexpr(out, x.expression());
      out << ')';
      out.nibble(-tab_unit) << newline() << indent();
   }

   static void
   sexpr(Output& out, const SourceFileAst& x) {
      out << '(' << "%path" << ' ' << x.path << ')';
      sexpr(out, x.asts);
   }

   static Output&
   sexpr(Output& out, const Ast* ast) {
      struct V : Ast::Visitor {
         void visit(const LiteralAst& x)  { sexpr(out, x); }
         void visit(const IdentifierAst& x) { sexpr(out, x); }
         void visit(const WildcardAst& x) { sexpr(out, "%wild", x); }
         void visit(const OperatorAst& x) { sexpr(out, "%operator", x); }
         void visit(const BracketAst& x) { sexpr(out, x); }
         void visit(const EnclosureAst& x) { sexpr(out, "%enclosure", x); }
         void visit(const BiSectionAst& x)  { sexpr(out, "%section", x); }
         void visit(const LeftSectionAst& x) { sexpr(out, "%lsection", x); }
         void visit(const RightSectionAst& x) { sexpr(out, "%rsection", x); }
         void visit(const AssertAst& x) { sexpr(out, "%assert", x); }
         void visit(const RestrictAst& x) { sexpr(out, "%restrict", x); }
         void visit(const UnaryAst& x)  { sexpr(out, "%unary", x); }
         void visit(const BinaryAst& x)  { sexpr(out, "%binary", x); }
         void visit(const DotAst& x)  { sexpr(out, "%dot", x); }
         void visit(const JuxtaposeAst& x) { sexpr(out, "%juxtapose", x); }
         void visit(const ArrowAst& x) { sexpr(out, "%arrow", x); }
         void visit(const IntervalAst& x) { sexpr(out, "%interval", x); }
         void visit(const FilterAst& x) { sexpr(out, "%filter", x); }
         void visit(const ApplyAst& x) { sexpr(out, "%apply", x); }
         void visit(const CollectAst& x) { sexpr(out, "%collect", x); }
         void visit(const ParameterAst& x) { sexpr(out, "%param", x); }
         void visit(const SignatureAst& x) { sexpr(out, "%sig", x); }
         void visit(const DefinitionAst& x) { sexpr(out, "%def", x); }
         void visit(const ProlongAst& x) { sexpr(out, "%prolong", x); }
         void visit(const PostulateAst& x) { sexpr(out, "%postulate", x); }
         void visit(const AliasAst& x) { sexpr(out, "%alias", x); }
         void visit(const ExPostulateAst& x) { sexpr(out, "%expostulate", x); }
         void visit(const AssociatedAst& x) { sexpr(out, "%alias", x); }
         void visit(const RuleAst& x) { sexpr(out, "%rule", x); }
         void visit(const ExprStmtAst& x) { sexpr(out, x); }
         void visit(const MatchAst& x) { sexpr(out, "%match", x); }
         void visit(const CaseAst& x) { sexpr(out, "%case", x); }
         void visit(const IfAst& x) { sexpr(out,"%if", x); }
         void visit(const RepeatAst& x) { sexpr(out, "%repeat", x); }
         void visit(const LeaveAst& x) { sexpr(out, "%leave", x); }
         void visit(const ReturnAst& x) { sexpr(out, "%return", x); }
         void visit(const ThrowAst& x) { sexpr(out, "%throw", x); }
         void visit(const CompoundAst& x) { sexpr(out, "%compound", x); }
         void visit(const DatatypeAst& x) { sexpr(out, "%datatype", x); }
         void visit(const AssignmentAst& x) { sexpr(out, "%assign", x); }
         void visit(const SequenceAst& x) { sexpr(out, "%seq", x); }
         void visit(const QuantifiedAst& x) { sexpr(out, "%quantified", x); }
         void visit(const LambdaAst& x) { sexpr(out, "%lambda", x); }
         void visit(const WhereAst& x) { sexpr(out, "%where", x); }
         void visit(const SourceFileAst& x) { sexpr(out, x); }
         void visit(const PathAst& x) { sexpr(out, "%path", x); }
         void visit(const ImportAst& x) { sexpr(out, "%import", x); }
         void visit(const DescriptionAst& x) { sexpr(out, "%descr", x); }

         V(Output& o) : out(o) { }
      private:
         Output& out;
      };
      
      if (ast == nullptr)
         return out << "%nil";
      else if (const IdentifierAst* id = is<IdentifierAst>(ast))
         return sexpr(out, *id->token());

      V v(out);
      ast->accept(v);
      return out;
   }

   // Return a string representation of the prefix form of an ast.
   std::string show(const Ast* ast) {
      std::ostringstream os;
      Output out(os);
      sexpr(out, ast);
      return os.str();
   }

   // Return a string representation of the prefix form of an ast.
   std::string show(const AstSequence* ast) {
      std::ostringstream os;
      Output out(os);
      sexpr(out, ast);
      return os.str();
   }

   std::ostream& prefix_form(std::ostream& os, const Ast* x) {
      Output out(os);
      sexpr(out, x);
      return os;
   }
}
