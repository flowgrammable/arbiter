// Copyright (C) 2010-2013, Texas A&M University.
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

#include <iostream>
#include <algorithm>
#include "Elaborator.H"

namespace liz {
   namespace {
      struct format_expression_list {
         const Arguments& seq;
         format_expression_list(const Arguments& s) : seq(s) { }
         int size() const { return seq.size(); }
         const Expression& operator[](int i) const {
            return *seq[i].code();
         }
      };

      std::ostream&
      operator<<(std::ostream& os, format_expression_list l) {
         for (int i = 0; i < l.size(); ++i) {
            if (i != 0)
               os << ", ";
            os << l[i];
         }
         return os;
      }

      static void
      pretty_print(std::ostream& os, const Name* n) {
         struct V : Name::Visitor {
            std::ostream& os;
            V(std::ostream& s) : os(s) { }

            void visit(const Identifier& x) { os << x.symbol(); }
            void visit(const Literal& x) { os << x.symbol(); }
            void visit(const Operator& x) { os << '(' << x.symbol() << ')'; }
         };
         V v(os);
         n->accept(v);
      }

      struct ElementaryVisitor : Expression::Visitor {
         std::ostream& os;
         explicit ElementaryVisitor(std::ostream& s) : os(s) { }

         void visit(const Bool& x) { os << std::boolalpha << bool(x); }
         void visit(const Int& x) { os << int(x); }
         void visit(const Uint& x) { os << uintmax_t(x); }
         void visit(const Double& x) { os << double(x); }
         void visit(const String& x) { os << '"' << x.rep() << '"'; }
         void visit(const Postulate& x) { os << x.name()->symbol(); }
         void visit(const Constructor& x) { os << x.name()->symbol(); }
         void visit(const LinkName& x) { os << x.name()->symbol(); }
         void visit(const Signature& x) { os << x.link_name().name()->symbol(); }
         void visit(const Formal& x) {
            if (auto n = x.name())
               os << n->symbol();
            else
               os << "parm-" << x.position() << '-' << x.level();
         }
         void visit(const Function& x) { os << x.name(); }
      };

      struct PrimaryVisitor : ElementaryVisitor {
         explicit PrimaryVisitor(std::ostream& s) : ElementaryVisitor(s) { }
         void visit(const Expression& x) { prefix_form(os, &x); }
      };

      static void
      pretty_print(std::ostream& os, const Arguments& args) {
         bool need_comma = false;
         for (auto& a : args) {
            if (need_comma)
               os << ", ";
            os << pretty(a.code());
            need_comma = true;
         }
      }

      static void
      pretty_print(std::ostream& os, const Instance& x) {
         os << pretty(x.constructor());
         os << '(';
         for (std::size_t i = 0; i < x.arguments().size(); ++i) {
            if (i != 0)
               os << ", ";
            os << *x.arguments()[i];
         }
         os << ')';
      }

      static bool
      show_path(const Component& x) {
         auto y = is<Namespace>(x.whole());
         return y != nullptr and y->name() != nullptr;
      }
      
      static void
      pretty_print(std::ostream& os, const Component& x) {
         if (show_path(x))
            os << pretty(x.whole().code()) << '.';
         pretty_print(os, x.name());
      }

      static void
      pretty_print(std::ostream& os, const CallExpression& x) {
         os << pretty(x.function().code())
            << '(';
         pretty_print(os, x.arguments());
         os << ')';
      }

      static void
      pretty_print(std::ostream& os, const UnaryExpression& x) {
         os << pretty(x.function().code())
            << '(' << pretty(x.argument().code()) << ')';
      }

      static void
      pretty_print(std::ostream& os, const BinaryExpression& x) {
         os << pretty(x.function().code())
            << '(' << pretty(x.lhs().code())
            << ", " << pretty(x.rhs().code()) << ')';
      }

      struct PostfixVisitor : PrimaryVisitor {
         explicit PostfixVisitor(std::ostream& s) : PrimaryVisitor(s) { }

         void visit(const Instance& x) { pretty_print(os, x); }
         void visit(const CallExpression& x) { pretty_print(os, x); }
         void visit(const Component& x) { pretty_print(os, x); }
         void visit(const UnaryExpression& x) { pretty_print(os, x); }
         void visit(const Negate& x) { pretty_print(os, x); }
         void visit(const Not& x) { pretty_print(os, x); }
         void visit(const Complement& x) { pretty_print(os, x); }
         void visit(const BinaryExpression& x) { pretty_print(os, x); }
         void visit(const Plus& x) { pretty_print(os, x); }
         void visit(const Dash& x) { pretty_print(os, x); }
         void visit(const Star& x) { pretty_print(os, x); }
         void visit(const Slash& x) { pretty_print(os, x); }
         void visit(const Div& x) { pretty_print(os, x); }
         void visit(const Rem& x) { pretty_print(os, x); }
         void visit(const Mod& x) { pretty_print(os, x); }
         void visit(const Langle& x) { pretty_print(os, x); }
         void visit(const Rangle& x) { pretty_print(os, x); }
         void visit(const Langleq& x) { pretty_print(os, x); }
         void visit(const Rangleq& x) { pretty_print(os, x); }
         void visit(const Eqeq& x) { pretty_print(os, x); }
         void visit(const Excleq& x) { pretty_print(os, x); }
      };

      
      struct ExpressionVisitor : PostfixVisitor {
         explicit ExpressionVisitor(std::ostream& s) : PostfixVisitor(s) { }

         void visit(const Read& x) { os << pretty(x.address().code()); }
         void visit(const Type& x) { os << show(x); }
         void visit(const TypeExpression& x) { os << pretty(x.expr().code()); }
         void visit(const BinaryLogical& x) {
            os << '(' << *x.lhs().code();
            if (x.operation() == logical::conjunction)
               os << " and ";
            else if (x.operation() == logical::disjunction)
               os << " or ";
            else if (x.operation() == logical::implication)
               os << " => ";
            else
               os << " ??? ";
            os << *x.rhs().code() << ')';
         }

         void visit(const Constraint& x) {
            pretty_print(os, x.constructor()->name());
            os << '(' << format_expression_list(x.arguments()) << ')';
         }
      };
   }

   std::ostream&
   operator<<(std::ostream& os, const Expression& x) {
      ExpressionVisitor v(os);
      x.accept(v);
      return os;
   }

   std::ostream&
   operator<<(std::ostream& os, pretty p) {
      if (auto x = p.expression())
         os << *x;
      return os;
   }
}
