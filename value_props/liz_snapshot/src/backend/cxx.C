// Copyright (C) 2012-2013, Texas A&M University.
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

#include "cxx.H"

namespace liz {
   namespace cxx {
      std::string
      mangle(const liz::Expression* e) {
         struct V : liz::Expression::Visitor {
            std::string result;
            V() : result() { }
            void visit(const liz::Expression& x) {
               result = show_expr(&x);
            }
            void visit(const liz::TypeExpression& x) {
               result = mangle(x.expr().code());
            }
            void visit(const liz::BasicType& x) {
               result = x.name()->symbol().string();
            }
            void visit(const liz::GenerativeType& x) {
               result = x.name()->symbol().string();
            }
            void visit(const liz::RecordType& t) {
               result = "struct { ";
               bool first = true;
               for (auto tag_t: t.components()) {
                  if (not first)
                     result += " ";
                  result += mangle(tag_t->type().code());
                  if (auto n = tag_t->tag()) {
                     result += ' ';
                     result += n->symbol().string();
                     result += ';';
                  }
                  first = false;
               }
               result += " }";
            }
            void visit(const liz::ArrayType& t) {
               result = "Array<";
               result += mangle(t.elem_type().code());
               result += '>';
            }
            void visit(const liz::Uint& x) { result = std::to_string(x.rep()); }
            void visit(const liz::Int& x) { result = std::to_string(x.rep()); }
            void visit(const liz::Key& x) {
               result += 'k';
               for (auto ints: x)
                  result += std::to_string(ints);
            }
            void visit(const liz::UintType& x) {
               result = "Uint<";
               result += mangle(&x.width());
               result += '>';
            }
            void visit(const liz::ArrowType& x) {
               result += mangle(x.target().code());
            }
            void visit(const liz::Instance& x) {
               if (auto ctor = x.constructor())
                  if (auto name = ctor->name())
                     result += name->symbol().string();
               result += '_';
               for (auto v: x.arguments()) {
                  result += '_';
                  result += mangle(v);
               }
               result += "__";
            }
         };
         V v;
         e->accept(v);
         return v.result;
      }

      Symbol
      mangle(Backend& be, const liz::Expression* e) {
         if (e == nullptr)
            return { };
         else
            return be.comp.intern(mangle(e));
      }

      Symbol
      mangle(Backend& be, Elaboration e) {
         if (e.code() == nullptr)
            return { };
         else
            return be.comp.intern(mangle(e.code()));
      }

      // -- Types --
      NamedType::NamedType(Symbol n)
            : structure::unary<Symbol>(n)
      { }

      ExprAsType::ExprAsType(const Expr* e)
            : structure::unary<const Expr*>(e)
      { }

      ConstType::ConstType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      RefType::RefType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      UintType::UintType(const Expr* e)
            : structure::unary<const Expr*>(e)
      { }

      PtrType::PtrType(const Type* t)
            : structure::unary<const Type*>(t)
      { }

      ArrayType::ArrayType(const Type* t, offset_type n)
            : structure::binary<const Type*, offset_type>(t, n)
      { }

      FunType::FunType(const Sequence<Type>& s, const Type* t)
            : structure::binary<Sequence<Type>, const Type*>(s, t)
      { }

      AnonType::AnonType(const Type* t, const Sequence<Decl>& s)
            : structure::binary<const Type*, Sequence<Decl>>(t, s)
      { }

      InstType::InstType(const Expr* e, const Sequence<Type>& args)
            : structure::binary<const Expr*, Sequence<Type>>(e, args)
      { }

      // -- Expressions --
      IdExpr::IdExpr(Symbol n)
            : structure::unary<Symbol>(n)
      { }

      InstExpr::InstExpr(const Expr* t, const Sequence<Expr>& x)
            : structure::binary<const Expr*, Sequence<Expr>>(t, x)
      { }

      InstOnTypes::InstOnTypes(const Expr* t, const Sequence<Type>& x)
            : structure::binary<const Expr*, Sequence<Type>>(t, x)
      { }

      ReturnExpr::ReturnExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      DotExpr::DotExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      ArrowExpr::ArrowExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      ScopeExpr::ScopeExpr(const Expr* x, Symbol n)
            : structure::binary<const Expr*, Symbol>(x, n)
      { }
      
      SubscriptExpr::SubscriptExpr(const Expr* x, const Expr* y)
            : structure::binary<const Expr*>(x, y)
      { }

      CallExpr::CallExpr(const Expr* x, const Sequence<Expr>& y)
            : structure::binary<const Expr*, Sequence<Expr>>(x, y)
      { }

      ObjectExpr::ObjectExpr(const cxx::Type* t, const Sequence<Expr>& x)
            : structure::binary<const cxx::Type*, Sequence<Expr>>(t, x)
      { }

      PostIncrementExpr::PostIncrementExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      PostDecrementExpr::PostDecrementExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      DerefExpr::DerefExpr(const Expr* e)
            : structure::unary<const Expr*>(e)
      { }

      NegExpr::NegExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      NotExpr::NotExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }
      
      ComplExpr::ComplExpr(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      AssignExpr::AssignExpr(const Expr* l, const Expr* r)
            : structure::binary<const Expr*>(l, r)
      { }
      
      // -- Statements --
      LabeledStmt::LabeledStmt(Symbol l, const Stmt* s)
            : structure::binary<Symbol, const Stmt*>(l, s)
      { }

      CaseStmt::CaseStmt(const Expr* x, const Stmt* s)
            : structure::binary<const Expr*, const Stmt*>(x,s)
      { }

      DefaultStmt::DefaultStmt(const Stmt* s)
            : structure::unary<const Stmt*>(s)
      { }

      ExprStmt::ExprStmt(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      CompoundStmt::CompoundStmt(const Sequence<Stmt>& seq)
            : structure::unary<Sequence<Stmt>>(seq)
      { }

      IfStmt::IfStmt(const Expr*x, const Stmt* t, const Stmt* f)
            : structure::ternary<const Expr*, const Stmt*>(x, t, f)
      { }

      SwitchStmt::SwitchStmt(const Expr* x, const Sequence<Stmt>& s)
            : structure::binary<const Expr*, Sequence<Stmt>>(x, s)
      { }

      LoopStmt::LoopStmt(const Stmt* s)
            : structure::unary<const Stmt*>(s)
      { }

      ReturnStmt::ReturnStmt(const Expr* x)
            : structure::unary<const Expr*>(x)
      { }

      GotoStmt::GotoStmt(Symbol l)
            : structure::unary<Symbol>(l)
      { }

      DeclStmt::DeclStmt(const Decl* d)
            : structure::unary<const Decl*>(d)
      { }

      // -- Declarations --
      VarDecl::VarDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      VarDef::VarDef(Symbol n, const Type* t, const Expr* x)
            : structure::ternary<Symbol, const Type*, const Expr*>(n, t, x)
      { }

      FieldDecl::FieldDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      Parm::Parm(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }
      
      FunDecl::FunDecl(Symbol n, const Type* t)
            : structure::binary<Symbol, const Type*>(n, t)
      { }

      Abstraction::Abstraction(const Sequence<Parm>& p, const Sequence<Stmt>& s)
            : structure::binary<Sequence<Parm>, CompoundStmt>(p, s)
      { }

      FunDef::FunDef(Symbol n, const Type* t, const Abstraction& x)
            : structure::ternary<Symbol, const Type*, Abstraction>(n, t, x)
      { }

      UsingDef::UsingDef(Symbol s, const Type* t)
            : structure::binary<Symbol, const Type*>(s,t)
      { }

      ScopeDef::ScopeDef(Symbol s) : n(s) { }

      StructDef::StructDef(Symbol s) : n(s) { }

      UnionDef::UnionDef(Symbol s) : n(s) { }

      EnumDef::EnumDef(Symbol s, const std::vector<Symbol>& ns)
         : std::vector<Symbol>(ns), n(s) { }

      VariantDef::VariantDef(Symbol n, const UnionDef* d,
                             const Sequence<Decl>& ds)
            : Sequence<Decl>(ds), n(n), d(d)
      { }

      TmpltDecl::TmpltDecl(const Sequence<Decl>& s, const Decl* d)
            : structure::binary<Sequence<Decl>, const Decl*>(s, d)
      { }

      DeclPack::DeclPack(const Decl* d, const Sequence<Decl>& ds)
            : structure::binary<const Decl*, Sequence<Decl>>(d, ds)
      { }

      Include::Include(const Path& p)
            : structure::unary<Path>(p)
      { }

      TopDecl::TopDecl(const Decl* d)
            : structure::unary<const Decl*>(d)
      { }

      // -- Decl::Visitor --
      void
      Decl::Visitor::visit(const Parm& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const VarDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const VarDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FieldDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FunDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const FunDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const UsingDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const ScopeDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const StructDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const UnionDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const EnumDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const VariantDef& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const TmpltDecl& d) {
         visit(as<Decl>(d));
      }

      void
      Decl::Visitor::visit(const DeclPack& d) {
         visit(as<Decl>(d));
      }

      // -- Backend --

      // Return the cached C++ translation of a Liz type.
      const cxx::Type*
      Backend::lookup(const liz::Type* t) const {
         Dict::const_iterator p = dict.find(t);
         return p == dict.end() ? 0 : p->second;
      }

      // Translate a Liz type expression to its C++ equivalent.
      // Look first for in the cache; then do the actual translation
      // if this is the first time.
      const cxx::Type*
      Backend::translate(const liz::Type* t) {
         struct V : liz::Expression::Visitor {
            Backend& cg;
            const cxx::Type* result;
            V(Backend& cg) : cg(cg), result() { }

            void visit(const liz::Expression& x) {
               internal_error("cxx::translate: unexpected IL "
                            + quote(show(&x)));
            }

            void visit(const liz::Type& t) {
               internal_error("cxx::translate: cannot translate Liz type "
                              + quote(show(t)) + " to C++ type");
            }

            void visit(const liz::BasicType& t) {
               result = cg.depot.named_type.make(t.name()->symbol());
            }

            void visit(const liz::GenerativeType& t) {
               result = cg.depot.named_type.make(t.name()->symbol());
            }

            void visit(const liz::TypeExpression& t) {
               result = cg.depot.expr_type.make
                  (cg.translate_expr(t.expr()));
            }
            
            void visit(const liz::ReferenceType& t) {
               result = cg.depot.ref_type.make(cg.translate(t.referee()));
            }

            void visit(const liz::ArrowType& t) {
               Sequence<cxx::Type> src(t.arity());
               for (std::size_t i = 0; i < t.arity(); ++i)
                  src[i] = cg.translate(t.argument(i));
               result = cg.depot.fun_type.make(src, cg.translate(t.target()));
            }

            void visit(const liz::ProductType& t) {
               Sequence<cxx::Type> src(t.arity());
               for (std::size_t i = 0; i < t.arity(); ++i)
                  src[i] = cg.translate(t.argument(i)->type());
               result = cg.depot.fun_type.make(src, cg.translate(t.target()));
            }

            void visit(const liz::ReadonlyType& t) {
               result = cg.depot.const_type.make(cg.translate(t.type()));
            }

            void visit(const liz::RestrictedType& t) {
               result = cg.translate(t.type());
            }

            void visit(const liz::UintType& t) {
               auto precision = cg.translate_expr(t.expr());
               result = cg.depot.uint_type.make(precision);
            }

            void visit(const liz::RecordType& t) {
               auto& cmpts = t.components();
               const std::size_t n = cmpts.size();
               Sequence<Decl> fields(n);
               for (std::size_t i = 0; i != n; ++i) {
                  auto sym = cmpts[i]->tag()->symbol();
                  auto t_i = cg.translate(cmpts[i]->type().code());
                  fields[i] = cg.depot.fld_decl.make(sym, t_i);
               }
               result = cg.depot.anon_type
                           .make(cg.translate(cg.comp.get_typename()), fields);
            }

            void visit(const liz::ArrayType& t) {
               auto id = cg.depot.id_expr.make(cg.comp.intern("Array"));
               Sequence<Type> args { cg.translate(t.elem_type().code()) };
               result = cg.depot.inst_type.make(id, args);
            }
         };
         
         if (const cxx::Type* x = lookup(t))
            return x;
         V v(*this);
         t->accept(v);
         return dict[t] = v.result;
      }

      Factories::Factories()
            : false_cst(false),
              true_cst(true)
      { }
      
      Backend::Backend(Compiler& c) : comp(c) { }
      
      const cxx::Expr*
      Backend::translate_name(const liz::Name* x) {
         struct V : liz::Name::Visitor {
            Backend& cg;
            const cxx::Expr* result;
            V(Backend& cg) : cg(cg), result() { }

            void visit(const liz::Identifier& x) {
               result = cg.depot.id_expr.make(x.symbol());
            }
            void visit(const liz::Operator& x) {
               result = cg.depot.id_expr.make
                  (cg.comp.intern("operator" + x.symbol().string()));
            }
            void visit(const liz::Literal& x) {
               result = cg.depot.id_expr.make
                  (cg.comp.intern("_" + x.symbol().string()));
            }
         };

         V v(*this);
         x->accept(v);
         return v.result;
      }

      // Build a C++ binary expression from a Liz binary expression.
      // FIXME: find better expression of this mappinng.
      template<typename S, typename T>
      static const cxx::Expr*
      build_binary_expr(Backend& be, Factory<S>& f, const binary_impl<T>& x) {
         const cxx::Expr* op = be.translate_expr(x.function());
         const cxx::Expr* a = be.translate_expr(x.lhs());
         const cxx::Expr* b = be.translate_expr(x.rhs());
         return f.make(op, a, b);
      }

      static bool
      is_per_coercion_type(const liz::Type* t) {
         if (auto at = is<liz::ArrowType>(t)) {
            if (at->arity() == 1)
               if (auto gt = is<GenerativeType>(at->target().code()))
                  return gt->value().code() == at->source()[0].code();
            return false;
         }
         if (auto at = is<liz::ProductType>(t)) {
            if (at->arity() != 1)
               if (auto gt = is<GenerativeType>(at->target().code()))
                  return gt->value().code() == at->source()[0]->type().code();
            return false;
         }
         return false;
      }

      static bool
      is_per_expr(FunctionElaboration e) {
         if (auto read = is<liz::Read>(e.code()))
            if (auto cmpt = is<liz::Component>(read->address().code()))
               if (auto name = cmpt->name())
                  return name->symbol().string() == "per"
                     and is_per_coercion_type(e.type());
         if (auto l = is<Lambda>(e.code()))
            if (auto name = l->name())
               return name->symbol().string() == "per";
         return false;
      }

      static const liz::Type*
      get_per_target_type(FunctionElaboration fun) {
         if (is_per_expr(fun))
            return get_target_type(fun);
         return nullptr;
      }

      static const liz::Initializer*
      get_initializer(const liz::CallExpression& x) {
         if (x.arguments().size() == 1)
            return is<Initializer>(x.arguments()[0].code());
         return  { };
      }

      const cxx::Expr*
      is_per_expr(Backend& be, const liz::CallExpression& x) {
         auto t = get_per_target_type(x.function());
         auto init = get_initializer(x);
         if (t == nullptr or init == nullptr)
            return nullptr;
         auto mt = be.translate(t);
         const std::size_t n = init->size();
         Sequence<Expr> exprs(n);
         for (std::size_t i = 0; i != n; ++i)
            exprs[i] = be.translate_expr((*init)[i].second);
         return be.depot.object_expr.make(mt, exprs);
      }

      static bool
      is_rep_coercion_type(const liz::Type* t) {
         if (auto at = is<liz::ArrowType>(t)) {
            if (at->arity() == 1)
               if (auto gt = is<GenerativeType>(at->source()[0].code()))
                  return at->target().code() == gt->value().code();
            return false;
         }
         if (auto at = is<liz::ProductType>(t)) {
            if (at->arity() != 1)
               if (auto gt = is<GenerativeType>(at->source()[0]->type().code()))
                  return at->target().code() == gt->value().code();
            return false;
         }
         return false;
      }

      static bool
      is_rep_expr(FunctionElaboration e) {
         if (auto read = is<liz::Read>(e.code()))
            if (auto cmpt = is<liz::Component>(read->address().code()))
               if (auto name = cmpt->name())
                  return name->symbol().string() == "rep"
                     and is_rep_coercion_type(e.type());
         if (auto l = is<Lambda>(e))
            if (auto name = l->name())
               return name->symbol().string() == "rep";
         return false;
      }

      const cxx::Expr*
      is_rep_expr(Backend& be, const liz::CallExpression& x) {
         if (not is_rep_expr(x.function()))
            return nullptr;
         return be.translate_expr(x.arguments()[0]);
      }

      const cxx::Expr*
      is_coercion_expr(Backend& be, const liz::CallExpression& x) {
         if (auto per_expr = is_per_expr(be, x))
            return per_expr;
         if (auto rep_expr = is_rep_expr(be, x))
            return rep_expr;
         return nullptr;
      }

      const cxx::Expr*
      translate_builtin(Backend& be, const NiladicBuiltinFunction& x) {
         return be.depot.id_expr.make(x.link_name().name()->symbol());
      }
      
      const cxx::Expr*
      translate_builtin(Backend& be, const UnaryBuiltinFunction& x) {
         auto id = be.depot.id_expr.make(x.link_name().name()->symbol());
         if (id->name().string() == "mk_array") {
            auto t = be.translate(is<liz::ArrayType>(is<ArrowType>(x.link_name().type())->target().code())->elem_type().code());
            Sequence<Type> args { t };
            return be.depot.tinst_expr.make(id, args);
         } else
            return id;
      }
      
      const cxx::Expr*
      translate_builtin(Backend& be, const BinaryBuiltinFunction& x) {
         return be.depot.id_expr.make(x.link_name().name()->symbol());
      }
      
      const cxx::Expr*
      translate_builtin(Backend& be, const TernaryBuiltinFunction& x) {
         return be.depot.id_expr.make(x.link_name().name()->symbol());
      }

      const cxx::Expr*
      Backend::translate_expr(liz::Elaboration x) {
         struct V : liz::Expression::Visitor {
            Backend& cg;
            const liz::Type* t;
            const cxx::Expr* result;
            V(Backend& cg, const liz::Type* typ) : cg(cg), t(typ), result() { }

            void visit(const liz::Expression& x) {
               internal_error("cxx::translate_expr: missed IL "
                            + quote(show(&x)));
            }

            void visit(const liz::Bool& x) {
               result = x.rep() ? &cg.depot.true_cst : &cg.depot.false_cst;
            }

            void visit(const liz::Char& x) {
               result = cg.depot.char_cst.make(x.rep());
            }

            void visit(const liz::Int& x) {
               result = cg.depot.int_cst.make(x.rep());
            }

            void visit(const liz::Uint& x) {
               result = cg.depot.int_cst.make(x.rep());
            }

            void visit(const liz::Double& x) {
               result = cg.depot.double_cst.make(x.rep());
            }

            void visit(const liz::String& x) {
               result = cg.depot.string_cst.make(x.rep().string());
            }

            void visit(const liz::Key& x) {
               result = cg.depot.key_cst.make(x);
            }

            void visit(const liz::Record& x) {
               if (x.size() == 0)
                  result = cg.depot.id_expr.make(cg.comp.intern("empty"));
               else
                  internal_error("cxx::translate_expr: cannot translate non- "
                                 "empty records");
            }

            void visit(const liz::Formal& x) {
               auto n = x.name();
               if (n == nullptr)
                  internal_error("cxx::translate_expr: use of unnamed parameter");
               result = cg.depot.id_expr.make(n->symbol());
            }

            void visit(const liz::LinkName& x) {
               result = cg.translate_name(x.name());
            }

            void visit(const liz::Constructor& x) {
               result = cg.translate_name(x.name());
            }

            void visit(const liz::RecordType& x) {
               auto region = cg.depot.region_expr.make();
               for (auto f : x.components())
                  region->push_back(cg.translate_decl(f));
               result = region;
            }

            void visit(const liz::VariantType& v) {
               auto region = cg.depot.region_expr.make();
               for (auto ctor : v.constructors())
                  region->push_back(cg.translate_ctor(ctor));
               result = region;
            }

            void visit(const liz::Namespace& x) {
               if (auto n = x.name())
                  result = cg.translate_name(n);
               else
                  result = cg.depot.id_expr.make(Symbol());
            }

            void visit(const liz::TypeExpression& x) {
               result = cg.translate_expr(x.expr());
            }

            void visit(const NiladicBuiltinFunction& x) {
               result = translate_builtin(cg, x);
            }

            void visit(const UnaryBuiltinFunction& x) {
               result = translate_builtin(cg, x);
            }

            void visit(const BinaryBuiltinFunction& x) {
               result = translate_builtin(cg, x);
            }

            void visit(const TernaryBuiltinFunction& x) {
               result = translate_builtin(cg, x);
            }

            void visit(const Lambda& x) {
               result = cg.translate_name(x.name());
            }

            void visit(const liz::Component& x) {
               const cxx::Expr* obj = cg.translate_expr(x.whole());
               auto member = x.name()->symbol();
               result = cg.depot.scope_expr.make(obj, member);
            }

            void visit(const liz::DotSelection& x) {
               const cxx::Expr* obj = cg.translate_expr(x.whole());
               result = cg.depot.dot_expr.make(obj, x.name()->symbol());
            }

            void visit(const liz::CallExpression& x) {
               if (auto per_expr = is_coercion_expr(cg, x)) {
                  result = per_expr;
                  return;
               }
               const cxx::Expr* fun = cg.translate_expr(x.function());
               Sequence<cxx::Expr> args;
               for (auto& a : x.arguments())
                  args.push_back(cg.translate_expr(a));
               if (t == cg.comp.get_typename())
                  result = cg.depot.inst_expr.make(fun, args);
               else
                  result = cg.depot.call_expr.make(fun, args);
            }

            void visit(const liz::Read& x) {
               // FIXME: lvalue-to-rvalue conversion is implicit in C++.
               // FIXME: we could attempt to make it explicit via
               // FIXME: static_cast but that will too much verbosity.
               result = cg.translate_expr(x.address());
            }

            void visit(const liz::Negate& x) {
               const cxx::Expr* e = cg.translate_expr(x.argument());
               result = cg.depot.neg_expr.make(e);
            }

            void visit(const liz::Not& x) {
               const cxx::Expr* e = cg.translate_expr(x.argument());
               result = cg.depot.not_expr.make(e);
            }

            void visit(const liz::Complement& x) {
               const cxx::Expr* e = cg.translate_expr(x.argument());
               result = cg.depot.compl_expr.make(e);
            }

            void visit(const liz::Plus& x) {
               result = build_binary_expr(cg, cg.depot.plus_expr, x);
            }

            void visit(const liz::Dash& x) {
               result = build_binary_expr(cg, cg.depot.minus_expr, x);
            }

            void visit(const liz::Star& x) {
               result = build_binary_expr(cg, cg.depot.mult_expr, x);
            }

            void visit(const liz::Slash& x) {
               result = build_binary_expr(cg, cg.depot.div_expr, x);
            }

            void visit(const liz::Div& x) {
               result = build_binary_expr(cg, cg.depot.div_expr, x);
            }

            void visit(const liz::Rem& x) {
               result = build_binary_expr(cg, cg.depot.rem_expr, x);
            }

            void visit(const liz::Mod& x) {
               result = build_binary_expr(cg, cg.depot.mod_expr, x);
            }

            void visit(const liz::Langle& x) {
               result = build_binary_expr(cg, cg.depot.lt_expr, x);
            }

            void visit(const liz::Rangle& x) {
               result = build_binary_expr(cg, cg.depot.gt_expr, x);
            }

            void visit(const liz::Langleq& x) {
               result = build_binary_expr(cg, cg.depot.le_expr, x);
            }

            void visit(const liz::Rangleq& x) {
               result = build_binary_expr(cg, cg.depot.ge_expr, x);
            }

            void visit(const liz::Eqeq& x) {
               result = build_binary_expr(cg, cg.depot.eq_expr, x);
            }

            void visit(const liz::Excleq& x) {
               result = build_binary_expr(cg, cg.depot.neq_expr, x);
            }

            void visit(const liz::And& x) {
               result = build_binary_expr(cg, cg.depot.and_expr, x);
            }

            void visit(const liz::Or& x) {
               result = build_binary_expr(cg, cg.depot.or_expr, x);
            }

            void visit(const liz::BinaryLogical& x) {
               const cxx::Expr* a = cg.translate_expr(x.lhs());
               const cxx::Expr* b = cg.translate_expr(x.rhs());
               switch (x.operation()) {
                  case liz::logical::conjunction:
                     result = cg.depot.and_expr.make(nullptr, a,b);
                     break;
                  case liz::logical::disjunction:
                     result = cg.depot.or_expr.make(nullptr, a,b);
                  case liz::logical::implication:
                    internal_error("cxx::translate_expr: C++ does not support "
                                   "implication operations");
                    break;
                  case liz::logical::equivalence:
                    internal_error("cxx::translate_expr: C++ does not support "
                                   "equivalence operations");
                    break;
               }
            }

            void visit(const liz::Instance& x) {
               std::string n;
               n += x.constructor()->name()->symbol().string();
               n += "_";
               for (auto v: x.arguments()) {
                  n += '_';
                  n += mangle(v);
               }
               n += "__";
               result = cg.depot.id_expr.make(cg.comp.intern(n));
            }

            void visit(const liz::Initializer& x) {
               const std::size_t n = x.size();
               Sequence<Expr> exprs(n);
               for (std::size_t i = 0; i != n; ++i)
                  exprs[i] = cg.translate_expr(x[i].second);
               auto mt = cg.translate(t);
               result = cg.depot.object_expr.make(mt, exprs);
            }

            void visit(const liz::Return& x) {
               auto expr = cg.translate_expr(x.expression());
               result = cg.depot.return_expr.make(expr);
            }

            void visit(const liz::Write& x) {
               auto location = cg.translate_expr(x.location());
               auto value = cg.translate_expr(x.value());
               result = cg.depot.assign_expr.make(location, value);
            }
         };

         if (not x)
            return nullptr;

         V v(*this, x.type());
         x.code()->accept(v);
         return v.result;
      }

      static bool
      is_enum_variant(const liz::Type& t) {
         if (auto v = is<VariantType>(&t))
            for (auto c: v->constructors())
               if (is<ArrowType>(c->type()))
                  return false;
         return true;
      }

      static bool
      is_enum_variant(const liz::VariantType& v) {
         for (auto c: v.constructors())
            if (is<ArrowType>(c->type()))
               return false;
         return true;
      }

      static void
      add_return_if_single_stmt(Backend& be, Sequence<Stmt>& stmts,
                                const liz::Type* t)
      {
         if (stmts.size() != 1 or t == nullptr or t == be.comp.get_void())
            return;
         if (dynamic_cast<const ReturnStmt*>(stmts.back()))
            return;
         if (auto expr_stmt = dynamic_cast<const ExprStmt*>(stmts.back())) {
            if (dynamic_cast<const ReturnExpr*>(expr_stmt->expression()))
               return;
            else
               stmts.back() = be.depot.ret_stmt.make(expr_stmt->expression());
         }
      }

      static const cxx::Stmt*
      translate_enum_clause(Backend& be, const PatternClause& pc,
                            const cxx::Expr*)
      {
         Sequence<Stmt> stmts;
         const Constructor* ctor = is<Constructor>(pc.pattern());
         if (ctor == nullptr)
            internal_error("cxx::translate_enum_clause: found a constructor "
                           "that isn't a `Constructor`: "
                           + show_expr(pc.pattern().code()));
         auto clause_stmt = be.translate_stmt(pc.action());
         if (auto blk = dynamic_cast<const CompoundStmt*>(clause_stmt)) {
            for (std::size_t i = 0; i != blk->size(); ++i)
               stmts.push_back(blk->operand()[i]);
         } else
            stmts.push_back(clause_stmt);
         add_return_if_single_stmt(be, stmts, pc.action().type());
         if (stmts.empty() or not dynamic_cast<const ReturnStmt*>(stmts.back()))
            stmts.push_back(&be.depot.brk_stmt);
         stmts.shrink_to_fit();
         return be.depot.cmpd_stmt.make(stmts);
      }

      static const cxx::Stmt*
      translate_pattern_clause(Backend& be, const PatternClause& pc,
                               const cxx::Expr* scrutinee)
      {
         Sequence<Stmt> stmts;
         const PatternInstance* ptrn = is<PatternInstance>(pc.pattern());
         if (ptrn == nullptr)
            internal_error("cxx::translate_pattern_clause: found a pattern "
                           "that isn't a `PatternInstance`: "
                           + show_expr(pc.pattern().code()));
         const std::size_t n = ptrn->formals().size();
         for (std::size_t i = 0; i != n; ++i) {
            auto id = ptrn->formals()[i]->name()->symbol();
            auto t = be.translate(be.comp.make_reference_type(ptrn->formals()[i]->type()));
            auto e = be.depot.dot_expr.make(scrutinee, ptrn->constructor()->name()->symbol());
            stmts.push_back(be.depot.decl_stmt.make(be.depot.var_def.make(id, t, e)));
         }
         auto clause_stmt = be.translate_stmt(pc.action());
         if (auto blk = dynamic_cast<const CompoundStmt*>(clause_stmt)) {
            for (std::size_t i = 0; i != blk->size(); ++i)
               stmts.push_back(blk->operand()[i]);
         } else
            stmts.push_back(clause_stmt);
         add_return_if_single_stmt(be, stmts, pc.action().type());
         if (stmts.empty() or not dynamic_cast<const ReturnStmt*>(stmts.back()))
            stmts.push_back(&be.depot.brk_stmt);
         stmts.shrink_to_fit();
         return be.depot.cmpd_stmt.make(stmts);
      }

      static Sequence<Stmt>
      translate_enum_clauses(Backend& be, const PatternClauses& pcs,
                             const cxx::Expr* scrutinee)
      {
         const std::size_t n = pcs.size();
         Sequence<Stmt> stmts(n);
         for (std::size_t i = 0; i != n; ++i)
            stmts[i] = translate_enum_clause(be, pcs[i], scrutinee);
         return stmts;
      }

      static Sequence<Stmt>
      translate_pattern_clauses(Backend& be, const PatternClauses& pcs,
                                const cxx::Expr* scrutinee)
      {
         const std::size_t n = pcs.size();
         Sequence<Stmt> stmts(n);
         for (std::size_t i = 0; i != n; ++i)
            stmts[i] = translate_pattern_clause(be, pcs[i], scrutinee);
         return stmts;
      }

      const cxx::Stmt*
      Backend::translate_stmt(Elaboration x) {
         struct V : liz::Expression::Visitor {
            Backend& cg;
            const liz::Type* t;
            const cxx::Stmt* result;
            V(Backend& cg, const liz::Type* t) : cg(cg), t(t), result() { }

            void visit(const liz::Expression& x) {
               const cxx::Expr* e = cg.translate_expr({ t, &x});
               result = cg.depot.expr_stmt.make(e);
            }

            void visit(const liz::Return& x) {
               const cxx::Expr* e = cg.translate_expr(x.expression());
               result = cg.depot.ret_stmt.make(e);
            }

            void visit(const liz::IfExpression& x) {
               const cxx::Expr* c = cg.translate_expr(x.condition());
               const cxx::Stmt* t = cg.translate_stmt(x.consequence());
               const cxx::Stmt* f = cg.translate_stmt(x.alternative());
               result = cg.depot.if_stmt.make(c, t, f);
            }

            void visit(const liz::Loop& x) {
               auto body = cg.translate_stmt(x.body());
               result = cg.depot.loop_stmt.make(body);
            }

            void visit(const liz::Leave&) {
               result = &cg.depot.brk_stmt;
            }

            void visit(const liz::Block& x) {
               Sequence<cxx::Stmt> stmts;
               for (int i = 0; i < x.size(); ++i)
                  stmts.push_back(cg.translate_stmt(x.statement(i)));
               result = cg.depot.cmpd_stmt.make(stmts);
            }

            void visit(const liz::BindExpression& x) {
               const Symbol n = x.link_name().name()->symbol();
               const cxx::Type* t = cg.translate(x.type());
               const cxx::Expr* e = cg.translate_expr(x.initializer());
               auto var_def = cg.depot.var_def.make(n, t, e);
               result = cg.depot.decl_stmt.make(var_def);
            }

            void visit(const liz::PatternMatch& x) {
               auto scrutinee = cg.translate_expr(x.scrutinee());
               if (is_enum_variant(*t)) {
                  auto clauses = translate_enum_clauses(cg, x.clauses(),
                                                        scrutinee);
                  result = cg.depot.switch_stmt.make(scrutinee, clauses);
               } else {
                  auto n = cg.comp.intern("liz_tag");
                  auto selection = cg.depot.dot_expr.make(scrutinee, n);
                  auto clauses = translate_pattern_clauses(cg, x.clauses(),
                                    scrutinee);
                  result = cg.depot.switch_stmt.make(selection, clauses);
               }
            }
         };

         if (x.code() == nullptr)
            return nullptr;
         V v(*this, x.type());
         x.code()->accept(v);
         return v.result;
      }

      Sequence<Parm>
      Backend::translate(const liz::Formals& formals) {
         Sequence<Parm> parms;
         for (std::size_t i = 0; i < formals.size(); ++i) {
            auto f = formals[i];
            const cxx::Type* t = translate(f->type());
            parms.push_back(depot.parm_decl.make(f->name()->symbol(), t));
         }
         return parms;
      }

      const cxx::Decl*
      Backend::translate_def(Symbol n, const liz::RecordType* s) {
         StructDef* def = depot.struct_def.make(n);
         for (auto f : s->components())
            def->push_back(translate_decl(f));
         return def;
      }

      const cxx::Decl*
      Backend::translate_scope_def(const liz::Namespace* s) {
         ScopeDef* ns = depot.scope_def.make(s->name()->symbol());
         for (std::size_t i = 0; i < s->stmt_count(); ++i)
            ns->push_back(translate_decl(s->statement(i).code()));
         return ns;
      }

      static const cxx::Decl*
      templatize_decl(Backend& cg, const Formals& fs, const cxx::Decl* decl) {
         const std::size_t n = fs.size();
         Sequence<Decl> parms(n);
         for (std::size_t i = 0; i < n; ++i)
            parms[i] = cg.translate_decl(fs[i]);
         return cg.depot.tmplt_decl.make(parms, decl);
      }

      const cxx::Decl*
      Backend::translate_template(const liz::Lambda* fun) {
         const cxx::Decl* decl = translate_decl(fun->body().code());
         return templatize_decl(*this, fun->formals(), decl);
      }

      // If `ctor' is a non-enumeration constant, return its
      // C++ translation.
      const cxx::Decl*
      Backend::translate_ctor(const liz::Constructor* ctor) {
         auto ft = is<ArrowType>(ctor->type());
         if (ft == nullptr)
            return nullptr;
         else if (ft->arity() == 1) {
            Symbol name = ctor->name()->symbol();
            const cxx::Type* type = translate(ft->argument(0));
            return depot.fld_decl.make(name, type);
         }
         else {
            const std::size_t n = ft->arity();
            Sequence<Decl> fields(n);
            for (std::size_t j = 0; j < n; ++j) {
               Symbol name = comp.intern("liz_field_" + show(j));
               const cxx::Type* type = translate(ft->argument(j));
               fields[j] = depot.fld_decl.make(name, type);
            }
            Symbol name = ctor->name()->symbol();
            const cxx::Type* type = depot.anon_type.make
               (translate(comp.get_typename()), fields);
            return depot.fld_decl.make(name, type);
         }
      }

      // FIXME: Implement support for constructors that take no parameters.
      static cxx::Decl*
      create_constant_ctor(Backend&, const Constructor&) {
         return nullptr;
      }

      static const cxx::Stmt*
      build_assign_stmt(Backend& be, const Expr* lhs, const Expr* rhs) {
         return be.depot.expr_stmt.make(be.depot.assign_expr.make(lhs, rhs));
      }

      static const cxx::Decl*
      create_ctor(Backend& be, const Constructor& ctor, std::size_t num) {
         // auto variant_name = get_variant_name(ctor);
         auto ctor_t = is<ArrowType>(ctor.type());
         if (ctor_t == nullptr)
            return create_constant_ctor(be, ctor);
         // Make name
         auto name = ctor.name()->symbol();
         // Translate the type
         auto t = be.translate(ctor_t);
         // Build the parameters
         const std::size_t n = ctor.arity();
         Sequence<Parm> params(n);
         for (std::size_t i = 0; i != n; ++i) {
            auto index = be.comp.intern(std::string("arg") + std::to_string(i));
            auto t_ = be.translate(ctor_t->source()[i].code());
            params[i] = be.depot.parm_decl.make(index, t_);
         }
         // Build the function body
         Sequence<Stmt> stmts(3 + n);
         auto var_name = be.depot.id_expr.make(be.comp.intern("x"));
         stmts[0] = be.depot.decl_stmt.make(be.depot.var_decl.make(var_name->name(), be.translate(ctor_t->target().code())));
         stmts[1] =
            build_assign_stmt(be, be.depot.dot_expr.make(var_name, be.comp.intern("liz_tag")), be.depot.int_cst.make(num));
         for (std::size_t i = 0; i != n; ++i)
            stmts[i+2] = build_assign_stmt(be, be.depot.dot_expr.make(var_name, ctor.name()->symbol()), be.depot.id_expr.make(be.comp.intern(std::string("arg") + std::to_string(i))));
         stmts[n+2] = be.depot.ret_stmt.make(var_name);
         Abstraction abs { params, stmts };
         return be.depot.fun_def.make(name, t, abs);
      }

      static Sequence<Decl>
      create_ctors(Backend& be, const VariantType& u) {
         const std::size_t n = u.constructors().size();
         Sequence<Decl> funcs(n);
         for (std::size_t i = 0; i != n; ++i)
            funcs[i] = create_ctor(be, *u.constructors()[i], i);
         return funcs;
      }

      const cxx::Decl*
      Backend::translate_enum_def(Symbol n, const liz::VariantType* v) {
         if (not is_enum_variant(*v))
            return nullptr;
         std::vector<Symbol> syms;
         for (auto ctor: v->constructors())
            if (auto name = ctor->name())
               syms.push_back(name->symbol());
         return depot.enum_def.make(n, syms);
      }

      const cxx::Decl*
      Backend::translate_variant_def(Symbol n, const liz::VariantType* v) {
         UnionDef* fields = depot.union_def.make(Symbol());
         for (auto c : v->constructors())
            if (auto d = translate_ctor(c))
               fields->push_back(d);
         auto ctors = create_ctors(*this, *v);
         return depot.variant_def.make(n, fields, ctors);
      }

      const cxx::Decl*
      Backend::translate_def(const liz::GenerativeType* x) {
         struct V : liz::Expression::Visitor {
            Backend* be;
            const Symbol name;
            const cxx::Decl* result;

            V(Backend* b, Symbol n) : be(b), name(n), result() { }

            void visit(const liz::Expression& x) {
               internal_error("cxx::translate_def: unexpected IL "
                              + quote(show(&x)));
            }

            void visit(const liz::BasicType& t) {
               auto alias = be->depot.named_type.make(t.name()->symbol());
               result = be->depot.using_def.make(name, alias);
            }

            void visit(const liz::RecordType& t) {
               result = be->translate_def(name, &t);
            }

            void visit(const liz::VariantType& t) {
               if (auto res = be->translate_enum_def(name, &t))
                  result = res;
               else
                  result = be->translate_variant_def(name, &t);
            }
         };

         V v(this, x->name()->symbol());
         x->value().code()->accept(v);
         return v.result;
      }

      // Generate the C++ definition for a Liz function `fun', with
      // equivalent C++ tyep `t'.
      const cxx::Decl*
      Backend::translate_fundef(const liz::Lambda* fun, const cxx::Type* t) {
         const liz::Expression* expr = fun->body().code();
         Sequence<Parm> parms = translate(fun->formals());
         Sequence<Stmt> stmts;
         if (auto blk = is<liz::Block>(expr)) {
            for (int i = 0; i < blk->size(); ++i)
               stmts.push_back(translate_stmt(blk->statement(i)));
         }
         else {
            if (is<PatternMatch>(fun->body().code()))
               stmts.push_back(translate_stmt(fun->body()));
            else if (is<liz::Loop>(fun->body().code())) {
               stmts.push_back(translate_stmt(fun->body()));
            } else {
               const cxx::Expr* e = translate_expr(fun->body());
               if (auto ret = dynamic_cast<const cxx::ReturnExpr*>(e))
                  stmts.push_back(depot.ret_stmt.make(ret->expr()));
               else
                  stmts.push_back(depot.ret_stmt.make(e));
            }
         }
         cxx::Abstraction abs(parms, stmts);
         return depot.fun_def.make(fun->name()->symbol(), t, abs);
      }

      const Decl*
      Backend::translate_decl(const liz::TagType* t) {
         return depot.fld_decl.make(t->tag()->symbol(), translate(t->type()));
      }

      static bool
      unseen_function_specialization(Backend& be, const liz::Lambda& lam)
      {
         for (auto e: be.specs_seen) {
            if (auto seen_lam = is<liz::Lambda>(e.code())) {
               auto sym = lam.link_name().name()->symbol();
               auto seen_sym = seen_lam->link_name().name()->symbol();
               if (sym == seen_sym) {
                  Elaboration t { be.comp.get_typename(), lam.type() };
                  if (structural_equivalence(t, seen_lam->type()))
                     return false;
               }
            }
         }
         return true;
      }

      static bool
      unseen_instance(Backend& be, const liz::Instance& inst) {
         for (auto e: be.specs_seen)
            if (is<liz::Instance>(e.code()))
               if (structural_equivalence(e, &inst))
                  return false;
         return true;
      }

      static bool
      exists_in_context(Elaborator& ctx, const Lambda& lam) {
         auto lnk = lam.link_name();
         if (ctx.select_if_can(lnk.name(), lnk.type()))
            return true;
         return false;
      }

      static const BinaryBuiltinFunction*
      packet_safe_spec(const Expression* e) {
         if (auto f = is<BinaryBuiltinFunction>(e))
            if (auto name = f->link_name().name())
               if (name->symbol().string() == "packet_safe")
                  return f;
         return nullptr;
      }

      Backend::register_result Backend::register_specialization(Elaboration e) {
         if (auto lam = is<liz::Lambda>(e.code())) {
            if (exists_in_context(comp, *lam))
               return Backend::register_result::already_registered;
            if (unseen_function_specialization(*this, *lam)) {
               specs_seen.push_back(e);
               return Backend::register_result::first_occurance;
            } else
               return Backend::register_result::already_registered;
         } else if (auto inst = is<liz::Instance>(e.code())) {
            if (unseen_instance(*this, *inst)) {
               specs_seen.push_back(e);
               return Backend::register_result::first_occurance;
            } else
               return Backend::register_result::already_registered;
         } else if (packet_safe_spec(e.code())) {
            return Backend::register_result::first_occurance;
         } else
            internal_error("cannot register non-lambda specializations");
         return Backend::register_result::already_registered;
      }

      static Elaboration
      realize_instance(Backend& be, const liz::Instance& inst) {
         if (inst.constructor() == nullptr)
            internal_error("seriously? Found an instance with no constructor.");
         const liz::Lambda* lam = nullptr;
         lam = is<liz::Lambda>(inst.constructor()->implementation().code());
         if (lam == nullptr)
            internal_error("instance implementation is not a lambda");
         Substitution subst;
         for (std::size_t i = 0; i != lam->formals().size(); ++i)
            subst[lam->formals()[i]] = { lam->formals()[i]->type(), inst.arguments()[i] };
         return subst_expr(static_cast<Elaborator*>(&be.comp), lam->body(), subst);
      }

      static const cxx::Decl*
      translate_type_instance(Backend& be, const liz::Instance& inst) {
         // axiom:    is<ArrowType>(inst->constructor()->type())
         //        or is<ProductType>(inst->constructor()->type())
         if (auto ctor = inst.constructor()) {
            auto name = ctor->name();
            if (name == nullptr)
               internal_error("cannot specialize instances of anonymous "
                              "constructors");
            auto e = realize_instance(be, inst);
            auto inst_name = mangle(be, &inst);
            if (auto rec_t = is<RecordType>(e.code()))
               return be.translate_def(inst_name, rec_t);
            else if (auto basic_t = is<BasicType>(e.code())) {
               auto id = be.depot.named_type.make(basic_t->name()->symbol());
               return be.depot.using_def.make(inst_name, id);
            }
            else
               internal_error("cannot translate an instance: "
                              + quote(show(e.code())));

         }
         // FIXME: Why are pointers used everywhere? Why not references?
         internal_error("seriously? Found an instance with no constructor.");
         return nullptr;
      }

      const cxx::Decl*
      specs_to_decl(Backend& be, Elaboration e) {
         if (be.register_specialization(e) ==
               Backend::register_result::first_occurance){
            auto t = be.translate(e.type());
            if (auto lam = is<liz::Lambda>(e.code())) {
               if (auto t = is<ArrowType>(lam->type()))
                  if (t->target().code() == be.comp.get_typename())
                     return nullptr;
               if (auto t = is<ProductType>(lam->type()))
                  if (t->target().code() == be.comp.get_typename())
                     return nullptr;
               return be.translate_fundef(lam, t);
            }
            else if (auto inst = is<liz::Instance>(e.code())) {
               if (e.type() == be.comp.get_typename())
                  return translate_type_instance(be, *inst);
               else
                  internal_error("trying to specialize a value instance");
            }
            internal_error("cannot translate specialization");
         }
         return nullptr;
      }

      const cxx::Decl*
      Backend::translate_decl(const liz::Expression* x) {
         struct V : liz::Expression::Visitor {
            Backend& cg;
            const cxx::Decl* result;
            
            V(Backend& cg) : cg(cg), result() { }

            void visit(const liz::Expression& x) {
               internal_error("cxx::translate_decl: unexpected IL "
                              + quote(show(&x)));
            }

            void visit(const liz::Field& x) {
               const cxx::Type* t = cg.translate(x.type());
               result = cg.depot.fld_decl.make(x.name()->symbol(), t);
            }

            void visit(const liz::Formal& x) {
               const cxx::Type* t = cg.translate(x.type());
               result = cg.depot.parm_decl.make(x.name()->symbol(), t);
            }

            void visit(const liz::BindExpression& x) {
               const Symbol n = x.link_name().name()->symbol();
               if (auto r = is<liz::RecordType>(x.initializer()))
                  result = cg.translate_def(n, r);
               else if (auto v = is<VariantType>(x.initializer())) {
                  if (auto res = cg.translate_enum_def(n, v))
                     result = res;
                  else
                     result = cg.translate_variant_def(n, v);
               } else if (auto g = is<GenerativeType>(x.initializer()))
                  result = cg.translate_def(g);
               else if (auto s = is<liz::Namespace>(x.initializer()))
                  result = cg.translate_scope_def(s);
               else if (is<liz::Constructor>(x.initializer()))
                  result = nullptr;
               else {
                  auto z = x.initializer().code();
                  if (is<liz::ArrowType>(x.type())) {
                     auto fun = is<liz::Lambda>(z);
                     auto ftype = is<liz::ArrowType>(fun->type());
                     if (fun == nullptr)
                        internal_error("cxx::translate_decl: invalid "
                                       "function initializer "
                                       + quote(show(&x)));
                     if (ftype->target() == cg.comp.get_typename())
                        result = nullptr;
                     else {
                        const cxx::Type* mt = cg.translate(x.type());
                        result = cg.translate_fundef(fun, mt);
                     }
                  } else if (is<QuantifiedType>(x.type())) {
                     result = nullptr;
                  } else {
                     const cxx::Type* mt = cg.translate(x.type());
                     const cxx::Expr* e = cg.translate_expr(x.initializer());
                     result = cg.depot.var_def.make(n, mt, e);
                  }
               }
            }

            void visit(const liz::Alias& x) {
               auto sym = x.alias().link_name().name()->symbol();
               auto liz_t = is<liz::Type>(x.equality().code());
               if (liz_t == nullptr)
                  internal_error("alias is not a type synonym");
               auto t = cg.translate(liz_t);
               result = cg.depot.using_def.make(sym, t);
            }
         };

         V v(*this);
         x->accept(v);
         return v.result;
      }

      static bool
      is_ignorable_toplevel_junk(Backend& be, const liz::Expression& e) {
         // When elaborating yields a `nullptr` (on purpose), this manifests as
         // a `void` expression.
         if (&e == be.comp.get_void())
            return true;
         return false;
      }

      const cxx::Toplevel*
      Backend::translate_top_level(const liz::Expression* x) {
         struct V : liz::Expression::Visitor {
            Backend& cg;
            const cxx::Toplevel* result;
            
            V(Backend& cg) : cg(cg), result(nullptr) { }

            void visit(const liz::Expression& x) {
               if (not is_ignorable_toplevel_junk(cg, x))
                  if (auto decl = cg.translate_decl(&x))
                     result = cg.depot.decl_toplevel.make(decl);
            }

            void visit(const liz::Import& x) {
               result = cg.depot.include_toplevel.make(x.load_unit()->path());
            }

            // FIXME: This is not the proper way of handling prolongs.
            void visit(const liz::Namespace&) { }
         };

         V v(*this);
         x->accept(v);
         return v.result;
      }
   }
}