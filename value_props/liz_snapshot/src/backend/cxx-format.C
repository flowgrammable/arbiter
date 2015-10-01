// Copyright (C) 2012, Texas A&M University.
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

#include "cxx.H"
#include "Output.H"

namespace liz {
   namespace cxx {
      // ---------------------
      // -- Pretty Printing --
      // ---------------------

      template<typename V, typename T>
      static void
      format_as(const T* x, io::Output& out) {
         V v { out };
         x->accept(v);
      }
      
      static void format_absdecl_type_prefix(const Type*, io::Output&);
      static void format_absdecl_type_suffix(const Type*, io::Output&);
      static void format(const Expr*, io::Output&);
      static void format(const Stmt*, io::Output&);
      static void format(const Decl*, io::Output&);

      static void
      format(const Type* t, io::Output& out) {
         format_absdecl_type_prefix(t, out);
         format_absdecl_type_suffix(t, out);
      }

      namespace {
         struct type_specifier : Type::Visitor {
            io::Output& out;
            type_specifier(io::Output& o) : out(o) { }

            void visit(const NamedType& t) { out << t.name(); }
            void visit(const ExprAsType& t) {
               format(t.expression(), out);
            }

            void visit(const ConstType& t) {
               if (auto x = is<PtrType>(t.main_variant())) {
                  format_absdecl_type_prefix(x, out);
                  out << ' ' << "const";
               }
               else {
                  out << "const" << ' ';
                  format_absdecl_type_prefix(t.main_variant(), out);
               }
            }

            void visit(const PtrType& t) {
               format_as<type_specifier>(t.pointee(), out);
            }

            void visit(const RefType& t) {
               format_as<type_specifier>(t.referee(), out);
            }

            void visit(const UintType& t) {
               out << "Uint<";
               format(t.precision(), out);
               out << '>';
            }

            void visit(const ArrayType& t) {
               format_as<type_specifier>(t.element_type(), out);
            }

            void visit(const FunType& t) {
               format_as<type_specifier>(t.target(), out);
            }

            void visit(const AnonType& t) {
               out << "std::tuple<";
               bool first = true;
               for (auto m: t.members()) {
                  if (not first) out << ", ";
                  format(m->type(), out);
                  first = false;
               }
               out << ">";
            }

            void visit(const InstType& t) {
               format(t.constructor(), out);
               out << '<';
               for (std::size_t i = 0; i < t.arguments().size(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(t.arguments()[i], out);
               }
               out << '>';
            }
         };

         struct absdecl_type_prefix : type_specifier {
            absdecl_type_prefix(io::Output& o) : type_specifier(o) { }

            void visit(const PtrType& t) {
               type_specifier::visit(t);
               if (is<FunType>(t.pointee()) or is<ArrayType>(t.pointee()))
                  out << '(';
               out << '*';
            }

            void visit(const RefType& t) {
               type_specifier::visit(t);
               if (is<FunType>(t.referee()) or is<ArrayType>(t.referee()))
                  out << '(';
               out << '&';
            }
         };

         struct absdecl_type_suffix : Type::Visitor {
            io::Output& out;
            absdecl_type_suffix(io::Output& o) : out(o) { }

            void visit(const NamedType&) { }

            void visit(const ExprAsType&) { }

            void visit(const ConstType& t) {
               format_absdecl_type_suffix(t.main_variant(), out);
            }

            void visit(const RefType& t) {
               format_absdecl_type_suffix(t.referee(), out);
            }

            void visit(const UintType&) { }

            void visit(const PtrType& t) {
               format_absdecl_type_suffix(t.pointee(), out);
            }

            void visit(const ArrayType& t) {
               out << '[' << t.upper_bound() << ']';
               format_absdecl_type_suffix(t.element_type(), out);
            }

            void visit(const FunType& t) {
               out << '(';
               for (std::size_t i = 0; i < t.arity(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(t.source()[i], out);
               }
               out << ')';
               format_absdecl_type_suffix(t.target(), out);
            }

            void visit(const AnonType&) { }

            void visit(const InstType&) { }
         };
      }

      static void
      format_absdecl_type_prefix(const Type* t, io::Output& out) {
         format_as<absdecl_type_prefix>(t, out);
      }

      static void
      format_absdecl_type_suffix(const Type* t, io::Output& out) {
         format_as<absdecl_type_suffix>(t, out);
      }


      template<typename F>
      struct default_to : Expr::Visitor, F {
         // FIXME: use inheriting ctor when available.
         explicit default_to(io::Output& out) : F{out} { }
         void visit(const Constant<bool>& x) { (*this)(x); }
         void visit(const Constant<Character>& x) { (*this)(x); }
         void visit(const Constant<offset_type>& x) { (*this)(x); }
         void visit(const Constant<double>& x) { (*this)(x); }
         void visit(const Constant<std::string>& x) { (*this)(x); }
         void visit(const IdExpr& x) { (*this)(x); }
         void visit(const DotExpr& x) { (*this)(x); }
         void visit(const ArrowExpr& x) { (*this)(x); }
         void visit(const ScopeExpr& x) { (*this)(x); }
         void visit(const SubscriptExpr& x) { (*this)(x); }
         void visit(const CallExpr& x) { (*this)(x); }
         void visit(const ObjectExpr& x) { (*this)(x); }
         void visit(const PostIncrementExpr& x) { (*this)(x); }
         void visit(const PostDecrementExpr& x) { (*this)(x); }
         void visit(const DerefExpr& x) { (*this)(x); }
         void visit(const NegExpr& x) { (*this)(x); }
         void visit(const NotExpr& x) { (*this)(x); }
         void visit(const ComplExpr& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Mult>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Div>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Rem>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Mod>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Plus>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Minus>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Lshift>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Rshift>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Less>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Greater>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::LessEq>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::GreaterEq>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Eq>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::NotEq>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Bitand>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Bitxor>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Bitor>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::And>& x) { (*this)(x); }
         void visit(const BinaryExpr<Operation::Or>& x) { (*this)(x); }
         void visit(const ConditionalExpr& x) { (*this)(x); }
         void visit(const AssignExpr& x) { (*this)(x); }
      };

      namespace {
         struct paren_expr {
            io::Output& out;
            void operator()(const Expr& x) {
               out << '('; format(&x, out); out << ')';
            }
         };

         struct primary_expr : default_to<paren_expr> {
            primary_expr(io::Output& out) : default_to<paren_expr>{ out } { }

            void visit(const Constant<bool>& x) {
               out.stream() << std::boolalpha << x.value();
            }

            void visit(const Constant<Character>& x) {
               out.stream() << '\'' << x.value() << '\'';
            }
            
            void visit(const Constant<offset_type>& x) {
               out.stream() << x.value();
            }
            
            void visit(const Constant<double>& x) {
               out.stream() << x.value();
            }
            
            void visit(const Constant<std::string>& x) {
               out.stream() << '"' << x.value() << '"';
            }

            void visit(const Constant<Key>& k) {
               out.stream() << "std::vector<int>";
               pretty_range(out, k.value().begin(), k.value().end());
            }
            
            void visit(const IdExpr& x) {
               if (Symbol s = x.name())
                  out << s;
            }

            void visit(const ScopeExpr& x) {
               format_as<primary_expr>(x.scope(), out);
               out << "::" << x.member();
            }

            void visit(const InstExpr& x) {
               format_as<primary_expr>(x.primary(), out);
               out << '<';
               for (std::size_t i = 0; i < x.arguments().size(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(x.arguments()[i], out);
               }
               out << '>';
            }

            void visit(const InstOnTypes& x) {
               format_as<primary_expr>(x.primary(), out);
               out << '<';
               for (std::size_t i = 0; i < x.arguments().size(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(x.arguments()[i], out);
               }
               out << '>';
            }

            void visit(const Region& s) {
               out << '{';
               if (not s.empty()) {
                  out.nibble(io::tab_unit);
                  for (auto& f : s) {
                     out << io::newline() << io::indent();
                     format(f, out);
                  }
                  out.nibble(-io::tab_unit) << io::newline() << io::indent();
               }
               out << '}';
            }

            void visit(const ReturnExpr& x) {
               out << "return";
               if (x.expr() != nullptr) {
                  out << ' ';
                  format(x.expr(), out);
               }
            }
         };

         static void
         format_tuple_object_expr(io::Output& out, const AnonType& at,
                                  const Sequence<Expr>& exprs)
         {
            out << "std::make_tuple<";
            {
               bool first = true;
               for (auto m: at.members()) {
                  if (not first) out << ", ";
                  format(m->type(), out);
                  first = false;
               }
            }
            out << ">(";
            {
               bool first = true;
               for (auto e: exprs) {
                  if (not first) out << ", ";
                  format(e, out);
                  first = false;
               }
            }
            out << ")";
         }

         struct postfix_expr : primary_expr {
            postfix_expr(io::Output& out) : primary_expr{ out } { }

            void visit(const DotExpr& x) {
               format_as<postfix_expr>(x.object(), out);
               out << '.' << x.member();
            }
            
            void visit(const ArrowExpr& x) {
               format_as<postfix_expr>(x.object(), out);
               out << "->" << x.member();
            }

            void visit(const SubscriptExpr& x) {
               format_as<postfix_expr>(x.array(), out);
               out << '[';
               format(x.index(), out);
               out << ']';
            }

            void visit(const CallExpr& x) {
               format_as<postfix_expr>(x.operation(), out);
               out << '(';
               for (std::size_t i = 0; i < x.arguments().size(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(x.arguments()[i], out);
               }
               out << ')';
            }

            void visit(const ObjectExpr& x) {
               if (auto t = dynamic_cast<const AnonType*>(x.type()))
                  format_tuple_object_expr(out, *t, x.arguments());
               else {
                  const std::size_t n = x.arguments().size();
                  format(x.type(), out);
                  out << '{';
                  if (n != 0) {
                     out << ' ';
                     format(x.arguments()[0], out);
                     for (std::size_t i = 1; i != n; ++i) {
                        out << ", ";
                        format(x.arguments()[i], out);
                     }
                  }
                  out << " }";
               }
            }

            void visit(const PostIncrementExpr& x) {
               format_as<postfix_expr>(x.operand(), out);
               out << "++";
            }

            void visit(const PostDecrementExpr& x) {
               format_as<postfix_expr>(x.operand(), out);
               out << "--";
            }
         };

         struct unary_expr : postfix_expr {
            unary_expr(io::Output& out) : postfix_expr(out) { }

            void visit(const DerefExpr& x) {
               out << '*';
               format_as<unary_expr>(x.address(), out);
            }

            void visit(const NegExpr& x) {
               out << '-';
               format_as<unary_expr>(x.operand(), out);
            }

            void visit(const NotExpr& x) {
               out << "not" << ' ';
               format_as<unary_expr>(x.operand(), out);
            }

            void visit(const ComplExpr& x) {
               out << '~';
               format_as<unary_expr>(x.operand(), out);
            }
         };

         // NOTE: We to not generate expressions using C-style cast-notation.

         struct pm_expr : unary_expr {
            pm_expr(io::Output& out) : unary_expr(out) { }
         };

         struct mult_expr : pm_expr {
            mult_expr(io::Output& out) : pm_expr(out) { }

            void visit(const BinaryExpr<Operation::Mult>& x) {
               format_as<mult_expr>(x.lhs(), out);
               out << " * ";
               format_as<pm_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Div>& x) {
               format_as<mult_expr>(x.lhs(), out);
               out << " / ";
               format_as<pm_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Rem>& x) {
               format_as<mult_expr>(x.lhs(), out);
               out << " % ";
               format_as<pm_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Mod>& x) {
               out << "mod" << '(';
               format_as<mult_expr>(x.lhs(), out);
               out << ", ";
               format_as<pm_expr>(x.rhs(), out);
               out << ')';
            }
         };

         struct add_expr : mult_expr {
            add_expr(io::Output& out) : mult_expr(out) { }

            void visit(const BinaryExpr<Operation::Plus>& x) {
               format_as<add_expr>(x.lhs(), out);
               out << " + ";
               format_as<mult_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Minus>& x) {
               format_as<add_expr>(x.lhs(), out);
               out << " - ";
               format_as<mult_expr>(x.rhs(), out);
            }
         };

         struct shift_expr : add_expr {
            shift_expr(io::Output& out) : add_expr(out) { }

            void visit(const BinaryExpr<Operation::Lshift>& x) {
               format_as<shift_expr>(x.lhs(), out);
               out << " << ";
               format_as<add_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Rshift>& x) {
               format_as<shift_expr>(x.lhs(), out);
               out << " >> ";
               format_as<add_expr>(x.rhs(), out);
            }
         };

         struct rel_expr : shift_expr {
            rel_expr(io::Output& out) : shift_expr(out) { }

            void visit(const BinaryExpr<Operation::Less>& x) {
               format_as<rel_expr>(x.lhs(), out);
               out << " < ";
               format_as<shift_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::Greater>& x) {
               format_as<rel_expr>(x.lhs(), out);
               out << " > ";
               format_as<shift_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::LessEq>& x) {
               format_as<rel_expr>(x.lhs(), out);
               out << " <= ";
               format_as<shift_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::GreaterEq>& x) {
               format_as<rel_expr>(x.lhs(), out);
               out << " >= ";
               format_as<shift_expr>(x.rhs(), out);
            }
         };

         struct eq_expr : rel_expr {
            eq_expr(io::Output& out) : rel_expr(out) { }

            void visit(const BinaryExpr<Operation::Eq>& x) {
               format_as<eq_expr>(x.lhs(), out);
               out << " == ";
               format_as<rel_expr>(x.rhs(), out);
            }

            void visit(const BinaryExpr<Operation::NotEq>& x) {
               format_as<eq_expr>(x.lhs(), out);
               out << " != ";
               format_as<rel_expr>(x.rhs(), out);
            }
         };

         struct bitand_expr : eq_expr {
            bitand_expr(io::Output& out) : eq_expr(out) { }

            void visit(const BinaryExpr<Operation::Bitand>& x) {
               format_as<bitand_expr>(x.lhs(), out);
               out << " & ";
               format_as<eq_expr>(x.rhs(), out);
            }
         };

         struct bitxor_expr : bitand_expr {
            bitxor_expr(io::Output& out) : bitand_expr(out) { }

            void visit(const BinaryExpr<Operation::Bitxor>& x) {
               format_as<bitxor_expr>(x.lhs(), out);
               out << " ^ ";
               format_as<bitand_expr>(x.rhs(), out);
            }
         };

         struct bitor_expr : bitxor_expr {
            bitor_expr(io::Output& out) : bitxor_expr(out) { }

            void visit(const BinaryExpr<Operation::Bitor>& x) {
               format_as<bitor_expr>(x.lhs(), out);
               out << " | ";
               format_as<bitxor_expr>(x.rhs(), out);
            }
         };

         struct and_expr : bitor_expr {
            and_expr(io::Output& out) : bitor_expr(out) { }

            void visit(const BinaryExpr<Operation::And>& x) {
               format_as<and_expr>(x.lhs(), out);
               out << " and ";
               format_as<bitor_expr>(x.rhs(), out);
            }
         };

         struct or_expr : and_expr {
            or_expr(io::Output& out) : and_expr(out) { }

            void visit(const BinaryExpr<Operation::Or>& x) {
               format_as<or_expr>(x.lhs(), out);
               out << " or ";
               format_as<and_expr>(x.rhs(), out);
            }
         };

         struct assign_expr;

         struct cond_expr : or_expr {
            cond_expr(io::Output& out) : or_expr(out) { }

            void visit(const ConditionalExpr& x) {
               format_as<or_expr>(x.condition(), out);
               out << " ? ";
               format(x.consequence(), out);
               out << " : ";
               format_as<assign_expr>(x.alternative(), out);
            }
         };

         struct assign_expr : cond_expr {
            assign_expr(io::Output& out) : cond_expr(out) { }

            void visit(const AssignExpr& x) {
               format_as<or_expr>(x.lhs(), out);
               out << " = ";
               format_as<assign_expr>(x.rhs(), out);
            }
         };
      }

      static void
      format(const Expr* x, io::Output& out) {
         format_as<assign_expr>(x, out);
      }

      static void
      format_case(const Stmt* stmt, std::size_t i, io::Output& out) {
         out << "case " << i << ":";
         out.nibble(io::tab_unit) << io::newline() << io::indent();
         format(stmt, out);
         out.nibble(-io::tab_unit) << io::newline() << io::indent();
      }

      namespace {
         struct stmt_printer : Stmt::Visitor {
            io::Output& out;
            stmt_printer(io::Output& o) : out(o) { }

            void visit(const LabeledStmt& s) {
               out << s.label() << ':';
               out << io::newline() << io::indent();
               format(s.statement(), out);
            }

            void visit(const CaseStmt& s) {
               out << "case ";
               format(s.guard(), out);
               out << ':';
               out.nibble(io::tab_unit) << io::newline() << io::indent();
               format(s.statement(), out);
               out.nibble(-io::tab_unit) << io::newline() << io::indent();
            }

            void visit(const DefaultStmt& s) {
               out << "default:";
               out.nibble(io::tab_unit) << io::newline() << io::indent();
               format(s.statement(), out);
               out.nibble(-io::tab_unit) << io::newline() << io::indent();
            }

            void visit(const ExprStmt& s) {
               if (const Expr* x = s.expression())
                  format(x, out);
               out << ';';
            }

            void visit(const CompoundStmt& s) {
               out << '{';
               out.nibble(io::tab_unit);
               for (std::size_t i = 0; i < s.size(); ++i) {
                  out << io::newline() << io::indent();
                  format(s[i], out);
               }
               out.nibble(-io::tab_unit) << io::newline() << io::indent();
               out << '}';
            }

            void visit(const IfStmt& s) {
               out << "if (";
               format(s.condition(), out);
               out << ')';
               out.nibble(io::tab_unit) << io::newline() << io::indent();
               format(s.consequence(), out);
               out.nibble(-io::tab_unit);
               if (const Stmt* x = s.alternative()) {
                  out << io::newline() << io::indent()
                      << "else ";
                  out.nibble(io::tab_unit) << io::newline() << io::indent();
                  format(x, out);
                  out.nibble(-io::tab_unit);
               }
            }

            void visit(const SwitchStmt& s) {
               out << "switch (";
               format(s.scrutinee(), out);
               out << ") {";
               out.nibble(io::tab_unit) << io::newline() << io::indent();
               const std::size_t n = s.cases().size();
               for (std::size_t i = 0; i != n; ++i)
                  format_case(s.cases()[i], i, out);
               out.nibble(-io::tab_unit) << io::newline() << io::indent();
               out << '}';
            }

            void visit(const LoopStmt& s) {
               out << "while (true)";
               out.nibble(io::tab_unit) << io::newline() << io::indent();
               format(s.body(), out);
               out.nibble(-io::tab_unit) << io::newline() << io::indent();
            }

            void visit(const BreakStmt&) { out << "break;"; }
            void visit(const ContinueStmt&) { out << "continue;"; }

            void visit(const ReturnStmt& s) {
               out << "return";
               if (const Expr* x = s.expression()) {
                  out << ' ';
                  format(x, out);
               }
               out << ';';
            }

            void visit(const GotoStmt& s) {
               out << "goto " << s.label() << ';';
            }

            void visit(const DeclStmt& s) {
               format(s.declaration(), out);
            }
         };
      }

      static void
      format(const Stmt* s, io::Output& out) {
         if (s == 0)
            out << ';';
         else
            format_as<stmt_printer>(s, out);
      }

      static void
      format(const Sequence<Parm>& parms, io::Output& out) {
         for (std::size_t i = 0; i < parms.size(); ++i) {
            if (i != 0)
               out << ", ";
            format(parms[i], out);
         }
      }

      static void
      format_region(io::Output& out, const std::vector<Symbol> syms) {
         out << "{ ";
         if (not syms.empty()) {
            out << syms.front() << " = 0";
            for (std::size_t i = 1; i != syms.size(); ++i)
               out << ", " << syms[i];
         }
         out << '}';
      }

      static void
      format_region(io::Output& out, const Sequence<Decl>& r) {
         out << '{';
         if (not r.empty()) {
            out.nibble(io::tab_unit);
            for (auto& f : r) {
               out << io::newline() << io::indent();
               format(f, out);
            }
            out.nibble(-io::tab_unit) << io::newline() << io::indent();
         }
         out << '}';
      }

      static void
      format_variant_struct(io::Output& out, Symbol var_name, const UnionDef& d)
      {
         out << "struct " << var_name << " {";
         out.nibble(io::tab_unit) << io::newline() << io::indent();
            out << var_name << " () : liz_tag(0), dummy(0) { }"
                << io::newline() << io::indent();
            out << "int liz_tag;" << io::newline() << io::indent();
            out << "union {";
            out.nibble(io::tab_unit) << io::newline() << io::indent();
               out << "int dummy;";
               for (auto decl: d) {
                  out << io::newline() << io::indent();
                  format(decl, out);
               }
            out.nibble(-io::tab_unit) << io::newline() << io::indent();
            out << "};";
         out.nibble(-io::tab_unit) << io::newline() << io::indent();
         out << "};";
      }

      namespace {
         struct decl_printer : Decl::Visitor {
            io::Output& out;
            decl_printer(io::Output& o) : out(o) { }

            void visit(const Decl& d) {
               format_absdecl_type_prefix(d.type(), out);
               out << ' ' << d.name();
               format_absdecl_type_suffix(d.type(), out);
            }

            void visit(const DeclPack& d) {
               for (auto decl: d.specializations()) {
                  format(decl, out);
                  out << "\n\n";
               }
               format(d.primary(), out);
            }

            void visit(const TmpltDecl& x) {
               out << "template" << '<';
               for (std::size_t i = 0; i < x.parameters().size(); ++i) {
                  if (i != 0)
                     out << ", ";
                  format(x.parameters()[i], out);
               }
               out << '>' << io::newline() << io::indent();
               format(x.instance(), out);
            }

            void visit(const VarDecl& d) {
               visit(as<Decl>(d));
               out << ';';
            }

            void visit(const FieldDecl& d) {
               visit(as<Decl>(d));
               out << ';';
            }

            void visit(const VarDef& d) {
               visit(as<Decl>(d));
               out << " = ";
               format(d.initializer(), out);
               out << ';';
            }

            void visit(const FunDef& d) {
               format_absdecl_type_prefix(d.type(), out);
               out << ' ' << d.name();
               out << '(';
               format(d.parameters(), out);
               out << ')' << ' ';
               format(&d.body(), out);
            }

            void visit(const UsingDef& d) {
               out << "using " << d.name() << " = ";
               format(d.type(), out);
               out << ';';
            }

            void visit(const StructDef& d) {
               out << "struct" << ' ';
               if (Symbol n = d.name())
                  out << n << ' ';
               format_region(out, d);
               out << ';';
            }

            void visit(const EnumDef& d) {
               out << "enum ";
               if (Symbol n = d.name())
                  out << n << ' ';
               format_region(out, d);
               out << ';';
            }

            void visit(const UnionDef& d) {
               out << "union" << ' ';
               if (Symbol n = d.name())
                  out << n << ' ';
               format_region(out, d);
               out << ';';
            }

            void visit(const ScopeDef& d) {
               out << "namespace" << ' ';
               if (Symbol n = d.name())
                  out  << n << ' ';
               format_region(out, d);
               out << io::newline() << io::indent();
            }

            void visit(const VariantDef& d) {
               out << "  // ADT: " << d.name() << '\n';
               format_variant_struct(out, d.name(), *d.definition());
               for (auto ctor: d) {
                  if (ctor == nullptr)
                     continue;
                  out << "\n\n";
                  out << "  // constructor: " << ctor->name() << '\n';
                  format(ctor, out);
               }
            }
         };
      }

      void
      format_forward_decl(const FunDef* f, std::ostream& os) {
         io::Output out(os);
         if (f == nullptr)
            return;
         const FunDef& d = *f;
         format_absdecl_type_prefix(d.type(), out);
         out << ' ' << d.name() << '(';
         format(d.parameters(), out);
         out << ");";
      }

      static void
      format(const Decl* d, io::Output& out) {
         format_as<decl_printer>(d, out);
      }

      void
      format(const Decl* d, std::ostream& os) {
         io::Output out(os);
         format(d, out);
      }

      namespace {
         struct toplevel_printer : Toplevel::Visitor {
            io::Output& out;
            toplevel_printer(io::Output& o) : out(o) { }

            void visit(const TopDecl& tl) {
               format_as<decl_printer>(tl.declaration(), out);
            }

            // No longer printed since a single file is generated.
            void visit(const Include&) { }
         };
      }

      void
      format(const Toplevel* d, io::Output& out) {
         if (d != nullptr)
            format_as<toplevel_printer>(d, out);
      }

      void
      format_toplevel(const Toplevel* d, std::ostream& os) {
         io::Output out(os);
         format(d, out);
      }
   }
}

