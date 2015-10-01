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


#include <iostream>
#include <algorithm>
#include <string>
#include "Elaborator.H"

namespace liz {
   // General pattern for elaborating a definition.
   template<typename C>
   static Elaboration
   do_elaborate_definition(C& ctx, const DefinitionAst& def) {
      check_if_redeclaration(ctx);
      auto decl = ctx.commit();
      auto value = elaborate_definiens(ctx, def.initializer());
      return finish_definition(ctx, decl, value);
   }

   // -- BasicContext --
   struct BasicContext {
      BasicContext(Elaborator* e)
            : elab(e),
              home(e->current_env())
      { }
      Elaborator* elaborator() const { return elab; }
      Elaborator* operator->() const { return elaborator(); }
      ScopeRef scope() const { return home; }
      ScopeStack& scope_stack() { return *elaborator(); }
      const std::vector<Elaboration>& get_specializations() const;
   private:
      Elaborator* const elab;
      ScopeRef home;
   };

   // -----------------------------
   // -- Elaborator::DeclContext --
   // -----------------------------
   struct Elaborator::DeclContext : BasicContext {
      DeclContext(BasicContext& ctx, const Name* n)
            : BasicContext(ctx), defs(), nm(n)
      {
         elaborator()->decls.push_back(this);
      }
      ~DeclContext() { elaborator()->decls.pop_back(); }
      const Name* name() const { return nm; }
      virtual const Type* type() const = 0;
      Declaration* commit() const { return declare(scope(), { nm, type() }); }
      std::vector<Elaboration> defs;
   private:
      const Name* const nm;
      DeclContext(DeclContext&&) = delete;
   };

   void Elaborator::remember_specialization(Elaboration e) {
      if (auto tdc = top_decl_context())
         tdc->defs.push_back(e);
   }

   const std::vector<Elaboration>& BasicContext::get_specializations() const {
      return elaborator()->decls.front()->defs;
   }

   using DeclContext = Elaborator::DeclContext;

   // -- Helper functions on fixity forms
   template<typename T>
   static const T*
   is(const FixityForm* f) {
      return dynamic_cast<const T*>(f);
   }

   static inline std::string
   quote(const Token* t) {
      return quote(lexeme(*t));
   }

   static inline std::string
   quote(const Atomic* x) {
      return quote(x->token());
   }

   static std::string
   quote(const BracketAst* x) {
      return quote(lexeme(x->first()) + lexeme(x->second()));
   }

   // -- Format the declarative form of an elaboration.
   static std::ostream&
   format_as_decl(std::ostream& os, Elaboration x) {
      struct V : Expression::Visitor {
         std::ostream& os;
         const Type* type;
         V(std::ostream& o, const Type* t) : os(o), type(t) { }
         void visit(const Expression&) { os << "_ : " << pretty(type); }
         void visit(const Postulate& x) {
            os << x.name()->symbol() << " : " << pretty(x.type());
         }
         void visit(const LinkName& x) {
            os << x.symbol() << " : " << pretty(x.type());
         }
         void visit(const Component& x) {
            os << x.symbol() << " : " << pretty(x.type());
         }
      };
      V v(os, x.type());
      x.code()->accept(v);
      return os;
   }

   // -- Semantics errors --
   namespace {
      template<typename T>
      struct with_decl : T {
         template<typename... Args>
         with_decl(const DeclContext* ctx, const Args&... args)
               : T(args...),
                 nm(ctx ? ctx->name() : nullptr),
                 ty(ctx ? ctx->type() : nullptr)
         { }

      protected:
         void disclose_location_on(std::ostream& os) const {
            if (nm != nullptr) {
               os << "semantics error in the context of " << quote(nm);
               if (ty != nullptr)
                  os << " of type " << quote(show(*ty));
               else
                  os << " in the form";
               os << ':' << std::endl;
            }
            T::disclose_location_on(os);
         }

      private:
         const Name* nm;
         const Type* ty;
      };
      
      struct SemanticsErrorAt : with_decl<ErrorAt> {
         SemanticsErrorAt(const DeclContext* x,
                          const std::string& s, const Token& t)
               : with_decl<ErrorAt>(x, s, t)
         { }
      };

      struct UndeclaredError : with_decl<ErrorAt> {
         UndeclaredError(const DeclContext* x, const std::string& s,
                         const Token& t)
               : with_decl<ErrorAt>(x, s, t)
         { }
      };

      struct OverloadError : with_decl<ErrorAt> {
         OverloadError(const DeclContext* x, const std::string& s,
                       const Token& t)
               : with_decl<ErrorAt>(x, s, t)
         { }
      };
   }

   static void
   semantics_error(BasicContext& ctx, const std::string& msg) {
      auto decl_ctx = ctx->top_decl_context();
      auto loc = ctx->current_location();
      throw SemanticsErrorAt(decl_ctx, msg, loc);
   }

   static void
   semantics_error(BasicContext& ctx, const std::string& m, const Token& t) {
      auto decl_ctx = ctx->top_decl_context();
      throw SemanticsErrorAt(decl_ctx, m, t);
   }

   static void
   semantics_error(BasicContext& ctx, const std::string& m, const Ast* x) {
      if (const Token* t = anchor(x))
         return semantics_error(ctx, m, *t);
      semantics_error(ctx, m);
   }

   static void
   semantics_error(BasicContext& ctx, const std::string& m, const FixityForm* x) {
      if (const Token* t = anchor(x))
         return semantics_error(ctx, m, *t);
      semantics_error(ctx, m);
   }

   static void
   undeclared_error(BasicContext& ctx, const std::string& msg, const Ast& x) {
      auto decl_ctx = ctx->top_decl_context();
      throw UndeclaredError(decl_ctx, msg, *anchor(&x));
   }

   static void
   overload_error(BasicContext& ctx, const std::string& msg) {
      auto decl_ctx = ctx->top_decl_context();
      auto loc = ctx->current_location();
      throw OverloadError(decl_ctx, msg, loc);
   }

   // -- Coercion diagnostics --
   namespace {
      struct CoercionError : SemanticsErrorAt {
         CoercionError(const DeclContext* x, const Token& l,
                       Elaboration e, const Type* t)
               : SemanticsErrorAt(x, "", l), expr(e), target(*t)
         { }
      private:
         void format_message(std::ostream&) const;
         Elaboration expr;
         const Type& target;
      };

      void
      CoercionError::format_message(std::ostream& os) const {
         os << "could not convert expression "
            << quote(show(expr.code()))
            << " of type " << quote(show(*expr.type()))
            << " to type " << target;
      }
   }

   static void
   coercion_error(BasicContext& ctx, Elaboration expr, const Type* target) {
      auto decl_ctx = ctx->top_decl_context();
      auto loc = ctx->current_location();
      throw CoercionError(decl_ctx, loc, expr, target);
   }

   // Attempt to coerce an expression `e' to type `t'.  Return an invalid
   // elaboration on failure.
   static Elaboration
   successful_coercion(BasicContext& ctx, Elaboration e, const Type* t) {
      try {
         return ctx->coerce(e, t);
      }
      catch (const CoercionError&) { }
      return { };
   }

   // Perform an explicit lvalue-to-rvalue conversion.
   static Elaboration
   rvalue(BasicContext& ctx, const Elaboration& expr) {
      if (auto t = is<ReferenceType>(expr.type()))
         return { t->referee(), ctx->build_read(expr) };
      return expr;
   }

   // --
   static const Type*
   default_type(BasicContext& ctx, const Token* x) {
      switch (x->kind) {
      case token::literal_boolean_tok: return ctx->get_bool();
      case token::literal_character_tok: return ctx->get_char();
      case token::literal_integer_tok: return ctx->get_int();
      case token::literal_real_tok: return ctx->get_double();
      case token::literal_string_tok: return ctx->get_string();

      default:
         semantics_error(ctx, "unknown literal " + quote(x));
         return ctx->get_void();
      }
   }

   static const Literal*
   literal_name(BasicContext& ctx, const Token* t) {
      return ctx->build_literal(ctx->intern(t), default_type(ctx, t));
   }
   
   static const Literal*
   make_name(BasicContext& ctx, const LiteralAst& x) {
      return literal_name(ctx, x.token());
   }

   static const Operator*
   make_name(BasicContext& ctx, const OperatorAst& x) {
      return ctx->build_operator(x.token());
   }

   static const Operator*
   make_name(BasicContext& ctx, const BracketAst& x) {
      return make_operator(ctx.elaborator(), lexeme(x.first()) + lexeme(x.second()));
   }

   static const Identifier*
   make_name(BasicContext& ctx, const IdentifierAst& x) {
      return ctx->build_identifier(x.token());
   }

   // -- default elaboration of a type expression.
   TypeElaboration
   type_elaboration(BasicContext& ctx, const Type* t) {
      return { ctx->get_typename(), t };
   }

   // -- Helper function for building reference types.
   static TypeElaboration
   make_reference_type(BasicContext& ctx, const Type* t) {
      return type_elaboration
         (ctx, ctx->make_reference_type(type_elaboration(ctx, t)));
   }

   // -- semantics fiber section
   static Fiber fiber_section(BasicContext&, const Fiber&, const Type*);

   // Return an elaboration designating a scope, if it is nomable.
   static Elaboration
   resolve_scope(BasicContext& ctx, ScopeRef sc) {
      const Expression* x = sc->origin();
      if (auto n = is<Namespace>(x))
         return { ctx->get_namespace(), n };
      return { };
   }

   // Return an expression designating an entity declared in a given scope.
   static Elaboration
   refer_by_name(BasicContext& ctx, const Declaration& d, Elaboration scope) {
      Elaboration e = d.value();
      if (is<Postulate>(e))     // Postulates stand for themselves
         return e;
      auto t = make_reference_type(ctx, e.type());
      if (is<Formal>(e))        // Formals are really symbolic references
         return { t, e.code() };
      auto lnk = ctx->build_name(d.name(), e.type());
      if (scope)
         return { t, ctx->build_component(scope, *lnk) };
      return { t, lnk };
   }

   // Compute the semantics fiber of a name `n' in a given scope `s'
   // denotated by the elaboration `scope'.
   static Fiber
   lookup_fiber(BasicContext& ctx, ScopeRef sc, const Name* n,
                Elaboration scope) {
      Fiber f;
      for (auto& d : sc->lookup(n))
         f.push_back(refer_by_name(ctx, d, scope));
      return f;
   }

   // Compute the lexical semantics fiber of a simple symbol.
   static Fiber
   lexical_fiber(BasicContext& ctx, const Name* n) {
      for (auto& sc : ctx.scope_stack())
         if (Fiber f = lookup_fiber(ctx, sc, n, resolve_scope(ctx, sc)))
            return f;
         else if (sc.kind() != Contour::lexical)
            break;
      return Fiber();
   }

   // Like lexical_fiber, except that an empty fiber results in an error.
   template<typename X>
   static Fiber
   lexical_fiber_or_else(BasicContext& ctx, const X& x) {
      Fiber f = lexical_fiber(ctx, make_name(ctx, x));
      if (f.empty())
         undeclared_error(ctx, "there is no declaration for this name", x);
      return f;
   }

   // Return a simple name representation of an AST object, if it is one.
   static const Name*
   simple_name(BasicContext& ctx, const Ast* x) {
      struct V {
         BasicContext& ctx;
         const Name* result;

         void operator()(const LiteralAst& x) { result = make_name(ctx, x);}
         void operator()(const IdentifierAst& x) {
            result = make_name(ctx, x);
         }
         void operator()(const OperatorAst& x) {
            result = make_name(ctx, x);
         }
         void operator()(const BiSectionAst& x) {
            result = make_name(ctx, *x.operation());
         }
         void operator()(const Ast&) { }
      };
      ast_visitor<V> v { ctx, nullptr };
      x->accept(v);
      return v.result;
   }

   // Return the semantics fiber of a name in a specified scope, error
   // if the fiber is empty.
   static Fiber
   lookup_fiber(BasicContext& ctx, Namespace* ns, const Name* n,
                const Elaboration& scope) {
      Fiber f = lookup_fiber(ctx, ns->region(), n, scope);
      if (f.empty())
         semantics_error(ctx, "no declaration for " + quote(n->symbol())
                         + " in namespace " + quote(show(*ns)));
      return f;
   }

   static void
   missing_field_error(BasicContext& ctx, const RecordType* rt,
                       const std::string name)
   {
      std::ostringstream os;
      os << "expression of record type `" << pretty(rt)
         << "` has no field named " << quote(name);
      semantics_error(ctx, os.str());
   }

   static Elaboration
   select_field(BasicContext& ctx, const RecordType* rt, const Name* n,
                const Elaboration& obj) {
      for (auto& f : rt->components())
         if (f->tag() == n) {
            auto t = make_reference_type(ctx, f->type());
            return { t, ctx->build_dot(obj, { n, f->type() }) };
         }
      missing_field_error(ctx, rt, n->symbol().string());
      return { };
   }

   static const Name*
   selector_name(BasicContext& ctx, const Ast* x) {
      auto name = simple_name(ctx, x);
      if (name == nullptr)
         semantics_error(ctx, "invalid selector", x);
      return name;
   }

   static Fiber
   semantics_fiber(BasicContext& ctx, const DotAst& x) {
      auto name = selector_name(ctx, x.member());
      Elaboration e = ctx->elaborate(x.scope());
      if (auto rt = is<RecordType>(e.type()))
         return Fiber{ rvalue(ctx, select_field(ctx, rt, name, e)) };
      Elaboration f = rvalue(ctx, e);
      if (auto rt = is<RecordType>(f.type()))
         return Fiber{ select_field(ctx, rt, name, f) };
      // FIXME: next cast indicates design bug.
      auto ns = const_cast<Namespace*>(is<Namespace>(evaluate(ctx.elaborator(), f).code()));
      if (ns == nullptr)
         semantics_error(ctx, "invalid use of object of type "
                         + quote(show(*e.type()))
                         + " as scope object");
      return lookup_fiber(ctx, ns, name, f);
   }

   // Return the lexical semantics fiber of an expression.
   static Fiber
   semantics_fiber(BasicContext& ctx, const Ast* x) {
      if (auto n = simple_name(ctx, x))
         return lexical_fiber(ctx, n);
      else if (auto y = is<DotAst>(x))
         return semantics_fiber(ctx, *y);
      return Fiber{ ctx->elaborate(x) };
   }

   // -- ScopeManager --

   static void
   print_scope_stack_enter(ScopeStack& st, std::ostream& os) {
      auto n = st.size();
      os << "=============== scope >" << n << "< ===============" << std::endl;
   }

   static void
   print_scope_stack_leave(ScopeStack& st, std::ostream& os) {
      auto n = st.size();
      os << "=============== scope <" << n << "> ===============" << std::endl;
      for (auto& d : st.top()) {
         os << d.name()->symbol() << ": " << *d.value().type() << std::endl;
      }
   }
   
   ScopeManager::ScopeManager(Elaborator* c, Scope& s)
         : ctx(c) {
      if (context()->enabled(debug::scope))
         print_scope_stack_enter(*context(), ctx->debug());
      context()->push(&s);
   }
   
   ScopeManager::~ScopeManager() {
      if (context()->enabled(debug::scope))
         print_scope_stack_leave(*context(), ctx->debug());
      context()->pop();
   }

   // -- Function call debug capability --
   namespace {
      struct CallChain {
         explicit CallChain(const char* s) : fun(s) {
            ++nesting;
            std::cerr << '[' << nesting << '>'
                      << fun << ']' << std::endl;
         }
         ~CallChain() {
            std::cerr << '[' << nesting << '<'
                      << fun << ']' << std::endl;
            --nesting;
         }

      private:
         const char* fun;
         static int nesting;
      };

      int CallChain::nesting = 0;

      struct LocalScopeManager : Scope, ScopeManager {
         explicit LocalScopeManager(Elaborator* c)
               : ScopeManager(c, *this) { }

         const Expression* origin() const { return nullptr; }
      };

      struct ParameterScopeManager : LocalScopeManager {
         explicit ParameterScopeManager(Elaborator* c)
               : LocalScopeManager(c) {
            context()->increase_parameter_depth();
         }
         ~ParameterScopeManager() {
            context()->decrease_parameter_depth();
         }
      };
   }

   // Type elaboration may produce colateral artifacts.
   // Indicate what should be filtered out.
   enum class filter {
      nothing, scope
   };

   static TypeElaboration
   elaborate_type(BasicContext&, const Ast*, filter = filter::scope);

   static Elaboration elaborate_guard(BasicContext&, const Ast*);
   static void assume_property(BasicContext&, Elaboration);

   template <typename Form>
   inline static Elaboration
   elaborate_form_restriction(BasicContext&, const Form&) {
      return { };
   }

   static Elaboration
   elaborate_form_restriction(BasicContext& ctx, const CallForm& form) {
      if (form.restriction() == nullptr)
         return { };
      auto e = elaborate_guard(ctx, form.restriction());
      assume_property(ctx, e);
      return e;
   }

   static const Type*
   make_form_type(BasicContext& ctx, TypeElaboration target_t,
                  const Formals& formals, Elaboration req)
   {
      if (req)
         return ctx->make_product_type(formals, target_t, req);
      return ctx->make_arrow_type(target_t, formals);
   }

   // -- FormContext --
   namespace {
      struct FormContext : DeclContext {
         template<typename T>
         FormContext(BasicContext& ctx, const T& form, const Ast* t)
               : DeclContext(ctx, get_name(ctx, form)),
                 ty(nullptr),
                 parms_scope(ctx.elaborator()),
                 parms(elaborate_parameters(ctx, form)),
                 target_ty(elaborate_type(ctx, t)),
                 res(elaborate_form_restriction(ctx, form))
         { ty = make_form_type(ctx, target_ty, parms, res); }
         const Type* type() const override { return ty; }
         TypeElaboration target_type() const { return target_ty; }
         Elaboration restriction() const { return res; }
         const Formals& formals() const { return parms; }
         Declaration* commit() const { return declare(scope(), { name(), ty }); }
      private:
         const Type* ty;
         ParameterScopeManager parms_scope;
         Formals parms;
         TypeElaboration target_ty;
         Elaboration res;
         FormContext(FormContext&&) = delete;
      };
   }

   // ----------------
   // -- Elaborator --
   // ----------------
   static const Identifier*
   make_identifier(BasicContext& ctx, const IdentifierAst* x) {
      if (x == nullptr)
         return nullptr;
      return make_name(ctx, *x);
   }
   
   static TypeElaboration do_elaborate_arrow_type(BasicContext&, const LambdaAst&);
   static TypeElaboration
   do_elaborate_quantified_type(BasicContext&, const QuantifiedAst&);

   static TypeElaboration
   elaborate_arrow_type(BasicContext& ctx, const LambdaAst& x, filter what) {
      if (what == filter::scope) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         return do_elaborate_arrow_type(ctx, x);
      }
      return do_elaborate_arrow_type(ctx, x);
   }

   static TypeElaboration
   elaborate_quantified_type(BasicContext& ctx, const QuantifiedAst& x,
                             filter what) {
      if (what == filter::scope) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         return do_elaborate_quantified_type(ctx, x);
      }
      return do_elaborate_quantified_type(ctx, x);
   }

   // Return the type of a predicate with input types `src'.
   static const ArrowType*
   make_predicate_type(BasicContext& ctx, const InputTypes& src) {
      return ctx->make_arrow_type(ctx->get_bool(), src);
   }

   // We are elaborating a restricted type; find a normal form for the
   // restricting predicate.
   static Elaboration
   elaborate_restriction(BasicContext& ctx, const Ast* x, TypeElaboration t) {
      return ctx->elaborate(x, make_predicate_type(ctx, { t }));
   }

   static TypeElaboration
   elaborate_restricted_type(BasicContext& ctx, const FilterAst& ast,
                             filter what ) {
      auto t = elaborate_type(ctx, ast.expr(), what);
      Elaboration p = elaborate_restriction(ctx, ast.condition(), t);
      return { t.type(), ctx->make_restricted_type(t, p) };
   }

   static TypeElaboration
   elaborate_maybe_tag_type(BasicContext& ctx, const ParameterAst& x) {
      auto t = elaborate_type(ctx, x.type());
      if (auto n = make_identifier(ctx, x.name()))
         return { ctx->get_typename(), ctx->make_tag_type(n, t) };
      return t;
   }

   // Elaborate `ast' as a type expression.  Raise an exception otherwise.
   static TypeElaboration
   elaborate_type(BasicContext& ctx, const Ast* ast, filter what) {
      struct V {
         BasicContext& ctx;
         filter what;
         TypeElaboration result;

         void operator()(const LambdaAst& x) {
            result = elaborate_arrow_type(ctx, x, what);
         }
         void operator()(const FilterAst& x) {
            result = elaborate_restricted_type(ctx, x, what);
         }
         void operator()(const QuantifiedAst& x) {
            result = elaborate_quantified_type(ctx, x, what);
         }
         void operator()(const ParameterAst& x) {
            result = elaborate_maybe_tag_type(ctx, x);
         }
         void operator()(const Ast& x) {
            result = ctx->coerce_to_type(ctx->elaborate(&x));
         }
      };
      ast_visitor<V> v { ctx, what, TypeElaboration() };
      ast->accept(v);
      return evaluate(ctx.elaborator(), v.result);
   }

   // Return true if `t' has a type value (as opposed to data values.)
   // This is useful to distinguish values that are types
   // (e.g. `int', `bool') from values that are ordinary data
   // (e.g., `2', `false'.)
   static bool
   has_type_values(const Type* t, BasicContext& ctx) {
      if (t == ctx->get_typename())
         return true;
      else if (auto rt = is<RestrictedType>(t))
         return has_type_values(rt->type(), ctx);
      return false;
   }

   // Return true if the elaboration `expr' designates a type.
   static bool
   has_type_denotation(const Elaboration& expr, BasicContext& ctx) {
      return is<Type>(expr.code()) or has_type_values(expr.type(), ctx);
   }

   static const Evidence&
   is_callable(const Constraint* req, BasicContext& ctx) {
      Evidence evidence;
      if (not is<ArrowType>(req->argument(0).code()))
         evidence.set_bad();
      return *ctx->register_evidence(req, evidence);
   }

   static const Evidence&
   is_record_type(const Constraint* req, BasicContext& ctx) {
      Evidence evidence;
      if (not is<RecordType>(req->argument(0).code()))
         evidence.set_bad();
      return *ctx->register_evidence(req, evidence);
   }

   namespace {
      struct tag_type_same {
         const TagType* tag_t;
         tag_type_same(const TagType* x) : tag_t(x) { }
         bool operator()(const TagType* other) {
            if (tag_t == other)
               return true;
            if (tag_t->tag()->symbol() != other->tag()->symbol())
               return false;
            return structural_equivalence(tag_t->type(), other->type().code());
         }
      };
   }

   static bool
   is_subset_of(const Sequence<TagType>& ts, const Sequence<TagType>& us) {
      for (auto t: ts)
         if (std::find_if(us.begin(), us.end(), tag_type_same(t)) == us.end())
            return false;
      return true;
   }

   static const Evidence&
   is_projection_of(const Constraint* req, BasicContext& ctx) {
      Evidence evidence;
      if (auto arec_t = is<RecordType>(req->argument(0).code()))
         if (auto brec_t = is<RecordType>(req->argument(1).code()))
            if (is_subset_of(arec_t->components(), brec_t->components()))
               return *ctx->register_evidence(req, evidence);
      if (is<RecordType>(req->argument(1).code()))
         return *ctx->register_evidence(req, evidence);
      evidence.set_bad();
      return *ctx->register_evidence(req, evidence);
   }

   static bool
   same_name(const Name* n0, const Name* n1) {
      if (n0 == nullptr and n1 == nullptr)
         return true;
      if (n0 != nullptr and n1 != nullptr)
         return n0->symbol() == n1->symbol();
      return false;
   }

   static const Instance*
   is_type_instance(const Expression* e) {
      if (auto texpr = is<TypeExpression>(e))
         return is<Instance>(texpr->expr().code());
      return nullptr;
   }

   static void
   emit_instance_of_signatures(BasicContext& ctx, const Constructor& ctor) {
      if (auto arrow_t = is<ArrowType>(ctor.type())) {
         const std::size_t n = arrow_t->arity();
         for (std::size_t i = 0; i != n; ++i) {
            auto n = std::string("Param") + std::to_string(i);
            auto name = make_identifier(ctx.elaborator(), n.c_str());
            auto t = arrow_t->source()[i].code();
            auto sig = ctx->build_signature(name, t);
            declare(ctx->current_env(), { name, t }, sig);
         }
      }
   }

   static const Evidence&
   is_instance_of(const Constraint* req, BasicContext& ctx) {
      Evidence ev;
      const Instance* inst_t = is_type_instance(req->argument(0).code());
      auto ctor_t = is<Constructor>(req->argument(1).code());
      if (inst_t == nullptr or ctor_t == nullptr) {
         ev.set_bad();
         return *ctx->register_evidence(req, ev);
      }
      if (not same_name(inst_t->constructor()->name(), ctor_t->name())
           or not structural_equivalence({ ctx->get_typename(), inst_t->constructor()->type() }, ctor_t->type()))
      {
         ev.set_bad();
         return *ctx->register_evidence(req, ev);
      }
      emit_instance_of_signatures(ctx, *ctor_t);
      return *ctx->register_evidence(req, ev);
   }

   namespace {
      using namespace subset_key;
      using IntPair = SubsetKeyValue::value_type;
      bool operator< (IntPair a, IntPair b) {
         if (a.first == b.first)
            return a.second < b.second;
         return a.first < b.first;
      }

      bool operator== (IntPair a, IntPair b) {
         return a.first == b.first and a.second == b.second;
      }

      bool
      valid_proto_field_pair(IntPair key_elt) {
         auto proto = key_elt.first;
         auto field = key_elt.second;
         switch (proto) {
            case internal_val: switch (field) {
               case in_port_val:   case in_phy_port_val:
               case tunnel_id_val: case metadata_val:
                  return true;
               default:
                  return false;
            }
            case ethernet_val: switch (field) {
               case src_val: case dst_val: case typ_val:    return true;
               default:                                     return false;
            }
            case ipv4_val: switch (field) {
               case dscp_val:  case ecn_val: case ttl_val:
               case proto_val: case src_val: case dst_val:
                  return true;
               default:
                  return false;
            }
            case ipv6_val: switch (field) {
               case flabel_val:  case src_val: case dst_val: case ttl_val:
                  return true;
               default:
                  return false;
            }
            case tcp_val: case sctp_val: case udp_val: switch (field) {
               case src_val: case dst_val:
                 return true;
               default:
                 return false;
            }
         }
         return false;
      }
   }

   static bool
   are_consistent_keys(SubsetKeyValue keys) {
      // All pairs are protocol-field pairs
      if (not std::all_of(keys.begin(), keys.end(), valid_proto_field_pair))
         return false;
      // Must be ordered. Why?
      if (not std::is_sorted(keys.begin(), keys.end()))
         return false;
      // No duplicates.
      if (std::adjacent_find(keys.begin(), keys.end()) != keys.end())
         return false;
      return true;
   }

   static const Evidence&
   is_validkey(const Constraint* req, BasicContext& ctx) {
      Evidence evidence;
      if (auto x = is<Key>(evaluate(ctx.elaborator(), req->argument(0)).code()))
      {
         if (not are_consistent_keys(*x))
            evidence.set_bad();
         return *ctx->register_evidence(req, evidence);
      } else {
         evidence.set_bad();
         return *ctx->register_evidence(req, evidence);
      }
   }

   template<typename Iter0, typename Iter1>
      static bool
      subsequence(Iter0 f0, Iter0 l0, Iter1 f1, Iter1 l1) {
         for (; f0 != l0; ++f0) {
            f1 = std::find(f1, l1, *f0);
            if (f1 == l1)
               return false;
            ++f1;
         }
         return true;
      }

   // c++17, gimme ranges!
   template <typename Range0, typename Range1>
      static inline bool
      subsequence(const Range0& r0, const Range1& r1) {
         return subsequence(r0.begin(), r0.end(), r1.begin(), r1.end());
      }

   static const Evidence&
   is_subkey(const Constraint* req, BasicContext& ctx) {
      Evidence evidence;
      auto k0 = is<Key>(evaluate(ctx.elaborator(), req->argument(0)).code());
      auto k1 = is<Key>(evaluate(ctx.elaborator(), req->argument(1)).code());
      if (k0 == nullptr or k1 == nullptr or not subsequence(*k0, *k1))
         evidence.set_bad();
      return *ctx->register_evidence(req, evidence);
   }

   // -- Construct the substitution mapping concept `c''s parameters to
   // -- to the argument list `args'.
   static Substitution
   substitution(const Constraint* req) {
      Substitution subst;
      auto lamb = req->abstraction();
      const int n = lamb->arity();
      for (int i = 0; i < n; ++i)
         subst[lamb->parameter(i)] = req->argument(i);
      return subst;
   }

   // Return true if the constraint `c1' entail constraint `c2'.
   static bool
   entail(const Constraint* c1, const Constraint* c2,  BasicContext& ctx) {
      // 0. No real task to carry out if both trivially agree.
      if (c1 == c2 or c2 == nullptr)
         return true;
      if (c1 == nullptr)
         return false;
      // 1. Set up a subtitution for instantiating the concept
      // associated with `c1'.
      Substitution subst = substitution(c1);
      const Concept* c = c1->original_concept();

      // 2. Traverse instantiated concept and recursive look for entailment.
      for (auto& f : c->formulae()) {
         Elaboration inst = subst_expr(ctx.elaborator(), f, subst);
         if (inst.code() == c2
             or entail(is<Constraint>(inst.code()), c2, ctx))
            return true;
      }
      // 3. At this point, we give up, as there is no entailment when
      // contidering nominal constraints.
      // However, be sure to handle the built-in concept `Function'.
      return c2->constructor() == ctx->get_Function()
         and is_callable(c1, ctx);
   }

   using EqClassHandle = EquivalenceQuotient::iterator;

   static void print_eq_classes(EquivalenceQuotient&, std::ostream&);

   static void
   load_assumption(BasicContext& ctx, Elaboration expr) {
      ctx->assume(expr);
      if (auto x = is<Eqeq>(expr.code()))
         ctx->eq_classes().merge_classes(x->lhs(), x->rhs());
   }
   
   struct equal_to {
      const BasicView* const view;
      explicit equal_to(const BasicView* v) : view(v) { }
      bool operator()(const BasicView* v) const {
         return view->constructor() == v->constructor()
            and view->arguments() == v->arguments();
      }
   };

   template<typename T>
   static void
   augment_set(std::set<T>& dst, const std::set<T>& src) {
      dst.insert(src.begin(), src.end());
   }

   // Return true if the expression `lhs' implies the expression `rhs'
   static bool
   surely_subsumes(Elaboration lhs, Elaboration rhs, BasicContext& ctx) {
      struct SubsumeVisitor : Expression::Visitor {
         Elaboration goal;
         BasicContext& ctx;
         bool result;
         SubsumeVisitor(Elaboration e, BasicContext& c)
               : goal(e), ctx(c), result(false) { }

         void visit(const Expression& x) {
            result = structural_equivalence(goal, &x);
         }
         
         void visit(const Bool& x) { result = bool(x); }
         
         void visit(const Formula& x) {
            // FIXME: we don't handle existential quantifier yet.
            if (x.quantifier() == Quantifier::exists)
               return;
            result = surely_subsumes(x.body(), goal, ctx);
         }

         void visit(const BinaryLogical& x) {
            switch (x.operation()) {
            case logical::conjunction:
               result = surely_subsumes(x.lhs(), goal, ctx)
                  and surely_subsumes(x.rhs(), goal, ctx);
               break;
            case logical::disjunction:
               result = surely_subsumes(x.lhs(), goal, ctx)
                  or surely_subsumes(x.rhs(), goal, ctx);
               break;
            case logical::implication: {
               Substitution subst;
               match(goal, x.rhs().code(), subst);
               if (subst.failed())
                  return;
               Elaboration cond = subst_expr(ctx.elaborator(), x.lhs(), subst);
               cond = evaluate(ctx.elaborator(), cond);
               // FIXME: for this code to work properly, we probably
               // need some form of reflection.
               // if (const Bool* b = is<Bool>(cond))
               //  result = bool(*b);
               if (is_closed(cond)) {
                  result = bool(ctx->eval(cond).value());
                  if (result)
                     load_assumption(ctx, goal);
               }
            }
               break;
            case logical::equivalence:
               result = surely_subsumes(x.lhs(), x.rhs(), ctx)
                  and surely_subsumes(x.rhs(), x.lhs(), ctx);
               break;
            }
         }
      };

      SubsumeVisitor v(rhs, ctx);
      lhs.code()->accept(v);
      return v.result;
   }

   FunctionElaboration
   lookup_operator(Elaborator* ctx_, const std::string& n, const ArrowType* t) {
      BasicContext ctx { ctx_ };
      auto op = make_operator(ctx_, n);
      auto overloads = lexical_fiber(ctx, op);
      overloads = fiber_section(ctx, overloads, t);
      if (overloads.size() > 1 or overloads.empty())
         semantics_error(ctx, "couldn't synthesize operator " + quote(n));
      auto e = overloads.front();
      return { is<ArrowType>(e.type()), e.code() };
   }

   static bool
   can_prove(BasicContext& ctx, Elaboration expr,
             const AssumptionSet& props)
   {
      for (auto& p : props)
         if (surely_subsumes(p, expr, ctx))
            return true;
      // FIXME: Need a helper function.
      // Elaboration true_cst { ctx->get_bool(),
      //                        ctx->build_bool(true, ctx->get_bool()) };
      // return surely_subsumes(true_cst, expr, ctx);
      return false;
   }

   Elaboration
   eq_close_term_if_can(EquivalenceQuotient& c, Elaboration e) {
      EqClassHandle p = c.eq_class(e);
      if (is_closed(p->leader()))
         return p->leader();
      return e;
   }

   Elaboration
   eq_close_term(BasicContext& ctx, EquivalenceQuotient&, Elaboration);

   FunctionElaboration eq_close_term(BasicContext& ctx, EquivalenceQuotient& c,
                                     FunctionElaboration f)
   {
      auto e = eq_close_term(ctx, c, (Elaboration)f);
      f.code(e.code());
      return f;
   }

   Elaboration
   eq_close_term_rec(BasicContext& ctx, EquivalenceQuotient& c, Elaboration e) {
      struct V : Expression::Visitor {
         BasicContext& ctx;
         EquivalenceQuotient& c;
         Elaboration result;
         V(BasicContext& x, EquivalenceQuotient& y, Elaboration z)
               : ctx(x), c(y), result(z) { }
         void visit(const Expression&) { }
         void visit(const TypeExpression& x) {
            auto expr = eq_close_term(ctx, c, x.expr());
            result.code(ctx->make_type_expression(expr));
         }
         void visit(const Plus& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_plus(function, lhs, rhs));
         }
         void visit(const Dash& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_dash(function, lhs, rhs));
         }
         void visit(const Star& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_star(function, lhs, rhs));
         }
         void visit(const Slash& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_slash(function, lhs, rhs));
         }
         void visit(const Div& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_div(function, lhs, rhs));
         }
         void visit(const Quo& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_quo(function, lhs, rhs));
         }
         void visit(const Rem& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_rem(function, lhs, rhs));
         }
         void visit(const Mod& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_mod(function, lhs, rhs));
         }
         void visit(const Langle& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_langle(function, lhs, rhs));
         }
         void visit(const Rangle& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_rangle(function, lhs, rhs));
         }
         void visit(const Langleq& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_langleq(function, lhs, rhs));
         }
         void visit(const Rangleq& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_rangleq(function, lhs, rhs));
         }
         void visit(const Eqeq& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_eqeq(function, lhs, rhs));
         }
         void visit(const Excleq& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_excleq(function, lhs, rhs));
         }
         void visit(const And& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_and(function, lhs, rhs));
         }
         void visit(const Or& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_or(function, lhs, rhs));
         }
         void visit(const BinaryLogical& x) {
            auto op = x.operation();
            auto lhs = eq_close_term(ctx, c, x.lhs());
            auto rhs = eq_close_term(ctx, c, x.rhs());
            result.code(ctx->build_logical(op, lhs, rhs));
         }
         void visit(const CallExpression& x) {
            auto function = eq_close_term(ctx, c, x.function());
            const std::size_t n = x.arguments().size();
            Arguments args(n);
            for (std::size_t i = 0; i != n; ++i)
               args[i] = eq_close_term(ctx, c, x.arguments()[i]);
            result.code(ctx->build_call(function, args));
         }
         void visit(const Read& x) {
            auto address = eq_close_term(ctx, c, x.address());
            result.code(ctx->build_read(address));
         }
         void visit(const Component& x) {
            auto whole = eq_close_term(ctx, c, x.whole());
            result.code(ctx->build_component(whole, x.link_name()));
         }
         void visit(const Negate& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto argument = eq_close_term(ctx, c, x.argument());
            result.code(ctx->build_negate(function, argument));
         }
         void visit(const Not& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto argument = eq_close_term(ctx, c, x.argument());
            result.code(ctx->build_not(function, argument));
         }
         void visit(const Complement& x) {
            auto function = eq_close_term(ctx, c, x.function());
            auto argument = eq_close_term(ctx, c, x.argument());
            result.code(ctx->build_complement(function, argument));
         }
      };

      V v { ctx, c, e };
      e.code()->accept(v);
      return v.result;
   }

   Elaboration
   eq_close_term(BasicContext& ctx, EquivalenceQuotient& c, Elaboration e) {
      auto x = eq_close_term_if_can(c, e);
      if (x.code() != e.code())
         return x;
      return eq_close_term_rec(ctx, c, e);
   }

   inline const Type*
   eq_close_type(BasicContext& ctx, EquivalenceQuotient& c, const Type* t) {
      return is<Type>(eq_close_term(ctx, c, { ctx->get_typename(), t }).code());
   }

   static void
   print_eq_class(EqClassHandle c, std::ostream& os) {
      os << "  " << pretty(c->leader().code()) << " : {";
      bool first_time = true;
      for (auto& p: *c) {
         if (not first_time) { os << ", "; }
         else                { os << ' '; }
         os << pretty(p.code());
         first_time = false;
      }
      os << " }\n";
   }

   static void
   print_eq_classes(EquivalenceQuotient& classes, std::ostream& os) {
      os << "\n================ Equivalence Classes =================\n";
      os << '{';
      for (EqClassHandle p = classes.begin(); p != classes.end(); ++p)
         print_eq_class(p, os);
      os << '}';
      os << "\n======================================================\n\n";
   }

   // ---------------------
   // -- FullApplication --
   // ---------------------
   // Representation of data generated by full application.
   namespace {
      struct FullApplication
         : structure::binary<FunctionElaboration, Arguments> {
         FullApplication()
               : structure::binary<FunctionElaboration, Arguments>({ }, { })
         { }
         FullApplication(FunctionElaboration f, const Arguments& args)
               : structure::binary<FunctionElaboration, Arguments>(f, args)
         { }
         FunctionElaboration function() const { return first(); }
         const Arguments& arguments() const { return second(); }
         // courtesy conversion for use in conditionals.
         explicit operator bool() const { return function() ? true : false; }
      };
   }

   using call_builder = const CallExpression* (Elaborator::*)
            (FunctionElaboration, const Arguments&);

   // We just finished elaboration of a call to `fun' with arguments `args'.
   // Build the resulting expression with `builder'.  This generality is
   // provided so that we can specialize call to user-defined operations
   // and yet retain the abstract form.
   template<typename Builder, typename... Args>
   Elaboration
   call_with(BasicContext& ctx, Builder builder, FunctionElaboration fun,
             const Args&... args) {
      auto elab = ctx.elaborator();
      auto t = get_target_type(fun);
      return { t, (elab->*builder)(fun, args...) };
   }

   template<typename T>
   Elaboration
   call_with(BasicContext& ctx, unary_builder<T> builder, FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args[0]);
   }

   template<typename T>
   Elaboration
   call_with(BasicContext& ctx, binary_builder<T> builder, FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args[0], args[1]);
   }

   static Elaboration
   call_with(BasicContext& ctx, call_builder builder , FullApplication data) {
      auto fun  = data.function();
      auto args = data.arguments();
      return call_with(ctx, builder, fun, args);
   }

   // Return the elaboration for a function call.
   static Elaboration
   call(BasicContext& ctx, FunctionElaboration fun, const Arguments& args) {
      return call_with(ctx, &Elaborator::build_call, fun, args);
   }

   static bool
   valid_eqclass(EqClassHandle h, BasicContext& ctx) {
      return h != ctx->eq_classes().end();
   }

   // Return true is `x' is equivalent to a member of `set'.
   static bool
   in_eqclass(const Type* x, EqClassHandle set, BasicContext& ctx) {
      if (not valid_eqclass(set, ctx))
         return false;
      for (auto& e : *set)
         if (x == e.code())
            return true;
      return false;
   }

   // Build an elaboration form for equality test between the types
   // respectively designated by `lhs' and `rhs'.
   static Elaboration
   equality_expr(Elaboration lhs, Elaboration rhs, BasicContext& ctx) {
      FunctionElaboration eq_fun = ctx->get_type_eq();
      Arguments args { lhs, rhs };
      return call_with(ctx, &Elaborator::build_eqeq, { eq_fun, args} );
   }

   static const Type*
   simplify_type(Elaborator* ctx, const Type* t) {
      if (t == nullptr)
         return t;
      auto te = evaluate(ctx, TypeElaboration{ ctx->get_typename(), t });
      return ctx->coerce_to_type(te).code();
   }

   // Unfolds a variant
   static bool
   variant_unfold_case(const Type* s, const Type* t) {
      if (auto gen_t = is<GenerativeType>(s))
         if (is<VariantType>(gen_t->value()))
            return is<TypeExpression>(t);
      return false;
   }

   // Return true if the type expressions `lhs' and `rhs' are
   // equivalent in `context'.  This equivalence takes into account
   // any assumption, in form of equation of logical formula, in effect.
   static bool
   are_equivalent(const Type* s, const Type* t, BasicContext& ctx) {
      // FIXME: This line shouldn't happen.
      //  This is used because variants are recursive; a construcor in a variant
      //  refers to the type that is being defined. When we define a variable of
      //  variant type, the type of the initializer must be the type of the
      //  variable. This check is implemented by `satisfies` which has the odd
      //  habit of flipping the source and target types before it passes them to
      //  `are_equivalent`.
      if (variant_unfold_case(s,t))
         t = simplify_type(ctx.elaborator(), t);
      if (ctx->enabled(debug::eq))
         print_eq_classes(ctx->eq_classes(), ctx->debug());
      // Don't work too hard.
      if (s == t)
         return true;
      auto lhs = type_elaboration(ctx, s);
      auto rhs = type_elaboration(ctx, t);

      s = eq_close_type(ctx, ctx->eq_classes(), s);
      t = eq_close_type(ctx, ctx->eq_classes(), t);
      if (structural_equivalence({ ctx->get_typename(), s }, t))
         return true;
      if (is_closed(lhs) and is_closed(rhs))
         return false;

      if (in_eqclass(s, ctx->eq_class(rhs), ctx)
          or in_eqclass(t, ctx->eq_class(lhs), ctx))
         return true;

      // Attempt the actual proof.
      // FIXME: This is proving strange things, like `ref t ~ concept` for any
      //        type `t`.
      //return can_prove(ctx, equality_expr(lhs, rhs, ctx), ctx->assumptions());
      return false;
   }

   static void
   ensure_proposition_reduces_to_true(BasicContext& ctx, Elaboration e) {
      if (e.type() != ctx->get_bool())
         semantics_error(ctx, "not a boolean expression "
                              + quote(show_expr(e.code())));
      auto norm_e = evaluate(ctx.elaborator(), e);
      auto b = is<Bool>(norm_e.code());
      if (b == nullptr)
         semantics_error(ctx, "cannot reduce " + quote(show_expr(e.code())));
      if (not b->rep())
         semantics_error(ctx, "evaluates to false "+quote(show_expr(e.code())));
   }

   static bool
   is_instance_of_constraint(const Constraint* req) {
      if (auto name = req->constructor()->name())
         return name->symbol().string() == "InstanceOf";
      return false;
   }

   static const Evidence&
   discharge_constraint(BasicContext&, const Expression*);

   static const Evidence&
   discharge_logical_constraint(BasicContext& ctx, const BinaryLogical& e) {
      Evidence ev;
      if (e.operation() == logical::Operation::conjunction) {
         const Evidence& evl = discharge_constraint(ctx, e.lhs().code());
         const Evidence& evr = discharge_constraint(ctx, e.rhs().code());
         if (evl.good() and evr.good()) {
            ev.send_to(e.lhs().code(), &evl);
            ev.send_to(e.rhs().code(), &evr);
            return *ctx->register_evidence(&e, ev);
         }
      }
      ev.set_bad();
      return *ctx->register_evidence(&e, ev);
   }

   static void
   discharge_signature(BasicContext& ctx, const Signature* sig, Evidence& ev) {
      auto lnk = sig->link_name();
      Fiber f = lexical_fiber(ctx, lnk.name());
      Fiber result = fiber_section(ctx, f, lnk.type());
      if (result.size() == 1)
         ev.send_to(sig, result.front().code());
      else
         ev.set_bad();
   }

   // Discharge proof obligations to be satisfied by `expr' in order
   // to meet the requirement `req'.  Returns the evidence accumulated
   // so far.
   static const Evidence&
   discharge_constraint(BasicContext& ctx, const Expression* e) {
      if (const Evidence* p = ctx->find_evidence(e))
         return *p;
      // FIXME: This is used incorrectly.
      if (can_prove(ctx, { ctx->get_concept(), e }, ctx->assumptions()))
         return *ctx->register_evidence(e, Evidence());

      if (auto conn_e = is<BinaryLogical>(e))
         return discharge_logical_constraint(ctx, *conn_e);

      const Constraint* req = is<Constraint>(e);
      if (req == nullptr)
         semantics_error(ctx, "Unsupported constraint" + quote(show_expr(e)));

      if (req->constructor() == ctx->get_Function())
         return is_callable(req, ctx);
      if (req->constructor() == ctx->get_Record())
         return is_record_type(req, ctx);
      if (req->constructor() == ctx->get_ValidKey())
         return is_validkey(req, ctx);
      if (req->constructor() == ctx->get_SubKey())
         return is_subkey(req, ctx);
      if (req->constructor() == ctx->get_ProjectionOf())
         return is_projection_of(req, ctx);
      if (is_instance_of_constraint(req))
         return is_instance_of(req, ctx);

      Evidence evidence;
      const Concept* c = req->original_concept();
      Substitution subst = substitution(req);
      for (std::size_t i = 0; i < c->formulae().size() and evidence; ++i) {
         Elaboration e = subst_expr(ctx.elaborator(), c->formula(i), subst);
         if (auto f = is<Signature>(e.code())) {
            // FIXME: perform a real lookup using some fiber.
            discharge_signature(ctx, f, evidence);
         } else if (auto r = is<Constraint>(e.code())) {
            const Evidence& sub =  discharge_constraint(ctx, r);
            evidence.send_to(r, &sub);
            if (not sub.good())
               evidence.set_bad();
         } else if (ctx->get_arithmetic() == e.type()) {
            if (not can_prove_arithmetic(*ctx.elaborator(), e))
               evidence.set_bad();
         } else if (/*auto formula = */is<Formula>(e.code())) {
            semantics_error(ctx, "sorry: satisfaction of formulae not yet "
                                 "implemented");
         } else if (auto eq_ax = is<Eqeq>(e.code())) {
            auto lhs = eq_close_term(ctx, ctx->eq_classes(), eq_ax->lhs());
            auto rhs = eq_close_term(ctx, ctx->eq_classes(), eq_ax->rhs());
            if (not structural_equivalence(lhs, rhs.code()))
               evidence.set_bad();
         } else if (e.type() == ctx->get_bool().code()) {
            ensure_proposition_reduces_to_true(ctx, e);
         } else {
            semantics_error(ctx, "unsupported constraint: "
                                 + show_expr(c->formula(i).code()));
         }
         // FIXME: check other types of formula as well.
      }
      return *ctx->register_evidence(req, evidence);
   }

   // Return true if the argument `value' sastifies the requirements
   // of `type'.
   static bool
   satisfies(BasicContext& ctx, const Elaboration& value, const Type* type) {
      // 1.  If exact match, then we're done.
      if (are_equivalent(type, value.type(), ctx))
         return true;
      // 2.  If it smells like a type then it is a type.
      else if (type == ctx->get_typename())
         return has_type_denotation(value, ctx);
      // 3.  FIXME If we have a concept, we must check its predicates.
      return false;
   }

   // Return true if the type `target' is a read-only version of `source'
   static bool
   is_readonly_variant(const Type* target, const Type* source,
                       BasicContext& ctx) {
      if (auto t = is<ReadonlyType>(target))
         return are_equivalent(t->type(), source, ctx);
      auto t = is<ReferenceType>(target);
      auto s = is<ReferenceType>(source);
      if (t != nullptr and s != nullptr)
         return is_readonly_variant(t->referee(), s->referee(), ctx);
      return false;
   }

   // Return an elaboration for a function.
   static SimpleFuncElaboration
   is_functoid(BasicContext& ctx, Elaboration e) {
      if (auto t = is<ArrowType>(e.type()))
         return { t, e.code() };
      else if (is<ReferenceType>(e.type()))
         return is_functoid(ctx, rvalue(ctx, e));
      return { };
   }

   static DependentElaboration
   is_dependent_functoid(BasicContext& ctx, Elaboration e) {
      if (auto t = is<ProductType>(e.type()))
         return { t, e.code() };
      else if (is<ReferenceType>(e.type()))
         return is_dependent_functoid(ctx, rvalue(ctx, e));
      return { };
   }

   // Return the widened elaboration if `e' denotes the restriction
   // of a value of type `t'.
   static Elaboration
   can_widen_to(Elaboration e, const Type* t, BasicContext& ctx) {
      if (auto s = is<RestrictedType>(e.type()))
         if (are_equivalent(s->type(), t, ctx))
            return { t, e.code() };
      return { };
   }

   static Elaboration
   symbolic_expression(BasicContext& ctx, Elaboration e, const Type* t) {
      if (t == ctx->get_prop()) {
         if (is<Value>(e))
            return { t, e.code() };
         return { t, ctx->build_quote(e) };
      }
      return { };
   }

   static bool
   has_logical_values(const Type* t, BasicContext& ctx) {
      if (t == ctx->get_bool())
         return true;
      else if (auto qt = is<QuantifiedType>(t))
         return has_logical_values(qt->abstract_instance(), ctx);
      return false;
   }

   static bool
   is_depth1_reference(const Type* t, const Type* u) {
      if (auto rt = is<ReferenceType>(t))
         if (auto ru = is<ReferenceType>(u))
            return is<ReferenceType>(rt->referee().code())
               and not is<ReferenceType>(ru->referee().code());
      return false;
   }

   Elaboration
   Elaborator::coerce(Elaboration value, const Type* target) {
      BasicContext ctx { this };
      if (has_type_denotation(value, ctx))
         value.code(coerce_to_type(value).code());

      // Just return the value if we are in inference mode, or
      // when we have an exact match.
      const Type* source = value.type();
      if (target == nullptr or are_equivalent(source, target, ctx))
         return value;
      else if (auto x = symbolic_expression(ctx, value, target))
         return x;
      // If the expression is wanted for its side effect, let it be so.
      if (are_equivalent(target, get_void(), ctx))
         return { target, value.code() };

      // Convert to rvalue conversion if the context requires it.
      if ((is<ReferenceType>(source) and not is<ReferenceType>(target))
          or is_depth1_reference(source, target))
         return coerce(rvalue(ctx, value), target);

      // Implicit const-qualification is free of charge.
      if (is_readonly_variant(target, source, ctx))
         return { target, value.code() };

      // Losing a rvalue top-level const-qualification is OK.
      if (auto s = is<ReadonlyType>(source)) {
         if (are_equivalent(s->type(), target, ctx))
            return { target, value.code() };
      }

      // Widening is free of charge.
      if (auto expr = can_widen_to(value, target, ctx))
         return expr;

      // Binding an rvalue to a const-reference requires a temporary.
      if (auto t = is<ReferenceType>(target)) {
         if (not is<ReferenceType>(source)
             and is_readonly_variant(t->referee(), source, ctx)) {
            // FIXME: run destructor for the temporary.
            return { target, build_bind(fresh_name(),t->referee(),value,{}) };
         }
      }

      if (target == get_axiom() and has_logical_values(value.type(), ctx))
         return value;
      else if (not satisfies(ctx, value, target))
         coercion_error(ctx, value, target);
      return { target, value.code() };
   }


   const Function*
   select_equality_operator(BasicContext& ctx, const Type* t) {
      auto argtype = type_elaboration(ctx, t);
      auto decl = ctx->select
         (make_operator(ctx.elaborator(), "=="),
          get_binary_predicate_type(ctx.elaborator(), argtype));
      return to_function(decl->value().code());
   }

   static bool
   is_builtin_requirement(BasicContext& ctx, const Constraint* req) {
      auto c = req->constructor();
      return c == ctx->get_Function()
          or c == ctx->get_Record()
          // or c == ctx->get_Regular()
          or c == ctx->get_SubKey()
          or c == ctx->get_ValidKey()
          or c == ctx->get_ProjectionOf()
          or is_instance_of_constraint(req);
   }

   const Signature*
   subst_signature(Elaborator* ctx, const Signature* sig, Substitution subst) {
      Elaboration e { ctx->get_typename(), sig->link_name().type() };
      auto t = ctx->coerce_to_type(subst_expr(ctx, e, subst)).code();
      return ctx->build_signature(sig->link_name().name(), t);
   }

   static void print_substitution(BasicContext&, const Substitution&);

   // -- Requirements assumptions
   // Make the requirements in `req' available as assumptions in `context'
   static void
   load_requirements(BasicContext& ctx, const Constraint* req) {
      // 1. Add the constraint itself first.
      load_assumption(ctx, { ctx->get_bool(), req });
      if (is_builtin_requirement(ctx, req))
         return;
      // 2. Build a substitution to instantiate the body.
      const Concept* c = req->original_concept();
      Substitution subst = substitution(req);
      // 3. Traverse the body and insert assumptions.
      for (auto& f : c->formulae())
         assume_property(ctx, subst_expr(ctx.elaborator(), f, subst));
   }

   static AstSequence
   get_sequence(const Ast* x) {
      if (auto y = is<SequenceAst>(x))
         return y->sequence();
      return { x };
   }
   
   // Determine whether the syntactic object `x' is enclosed
   // in parenthesis.
   static bool
   is_parenthesized(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_paren_tok;
   }

   static bool
   is_braced(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_brace_tok;
   }

   static bool
   is_mbracketed(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_mbracket_tok;
   }

   // Determine whether the syntactic object `x' is enclosed
   // in brackets.
   static bool
   is_bracketed(const EnclosureAst& x) {
      return x.bracket()->first().kind == token::open_bracket_tok;
   }

   static AstSequence
   get_arguments(const ApplyAst& x) {
      auto y = x.arguments();
      if (y->expr() == nullptr)
         return { };
      else if (is_parenthesized(*y) or is_mbracketed(*y))
         return get_sequence(y->expr());
      return { y };
   }

   // Attempt to elaborate an expression in a given type context.
   // Return the elaboration in case of success.
   static Elaboration
   try_initializer(BasicContext& ctx, const Ast* x, const Type* t) {
      try {
         return ctx->elaborate(x, t);
      }
      catch (const SemanticsErrorAt&) { }
      catch (const UndeclaredError&) { }
      return { };
   }

   static Substitution
   make_subst(BasicContext& ctx, const Formals& formals,
              const std::vector<Elaboration>& elabs)
   {
      Substitution subst;
      const std::size_t n = formals.size();
      if (n == elabs.size()) {
         for (std::size_t i = 0; i != n; ++i)
            subst[formals[i]] = elabs[i];
         return subst;
      } else
         semantics_error(ctx, "cannot form substitution from formals and "
                              "elaborations");
      return  { };
   }

   static Substitution
   substitution(const Formals& fs, const Arguments& args) {
      Substitution subst;
      const std::size_t n = fs.size();
      for(std::size_t i = 0; i != n; ++i)
         subst[fs[i]] = args[i];
      return subst;
   }

   static void
   arguments_satisfy_restriction(BasicContext& ctx, const ProductType& pt,
                                 const Arguments& args)
   {
      // Axiom args.size() == pt.source().size()
      if (not pt.restriction())
         return;
      Substitution subst = substitution(pt.source(), args);
      auto res = subst_expr(ctx.elaborator(), pt.restriction(), subst);
      if (res.type() == ctx->get_concept()) {
         if (not discharge_constraint(ctx, res.code()))
            semantics_error(ctx, "could not satisfy constraint "
                                 + quote(show_expr(res.code())));
      } else if (res.type() == ctx->get_bool()) {
         if (not can_prove(ctx, res, ctx->assumptions())) {
            std::stringstream sstr;
            sstr
               << "could not satisfy constraint `"
               << show_expr(res.code()) << '`';
            semantics_error(ctx, sstr.str());
         }
      } else
         semantics_error(ctx, "restriction has type "
                              + quote(show_type(res.type()))
                              + ", but should have type "
                              + quote(show_type(ctx->get_concept()))
                              + " or " + quote(show_type(ctx->get_bool())));
   }

   static FullApplication
   debug_acceptable_arguments(BasicContext& ctx, SimpleFuncElaboration fun,
                              const AstSequence& arglist) {
      const ArrowType* ftype = fun.type();
      const std::size_t nargs = arglist.size();
      if (nargs != ftype->arity()) {
         std::ostringstream os;
         os << "have " << arglist.size() << " arguments, but `"
            << pretty(fun.code()) << ":" << pretty(fun.type())
            << "` has arity of " << ftype->arity();
         semantics_error(ctx, os.str());
      }
      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         if (auto arg = ctx->elaborate(arglist[i], ftype->argument(i)))
            args[i] = arg;
         else {
            std::ostringstream os;
            os << "cannot elaborate `";
            prefix_form(os, arglist[i]) << "` to type `";
            os << pretty(ftype->argument(i).code()) << '`';
            semantics_error(ctx, os.str());
            return { };
         }
      }
      return { fun, args };
   }

   static inline TypeElaboration
   source_type_at(const ProductType& pt, std::size_t n) {
      return pt.source()[n]->type();
   }

   static FullApplication
   debug_acceptable_arguments(BasicContext& ctx, DependentElaboration fun,
                              const AstSequence& arglist) {
      const ProductType* pt = fun.type();
      const std::size_t nargs = arglist.size();
      if (nargs != pt->arity()) {
         std::ostringstream os;
         os << "have " << arglist.size() << " arguments, but `"
            << pretty(fun.code()) << ":" << pretty(fun.type())
            << "` has arity of " << pt->arity();
         semantics_error(ctx, os.str());
      }
      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         if (auto arg = ctx->elaborate(arglist[i], source_type_at(*pt,i)))
            args[i] = arg;
         else {
            std::ostringstream os;
            os << "cannot elaborate `";
            prefix_form(os, arglist[i]) << "` to type `";
            os << pretty(source_type_at(*pt,i)) << '`';
            semantics_error(ctx, os.str());
            return { };
         }
      }
      arguments_satisfy_restriction(ctx, *fun.type(), args);
      return { fun, args };
   }

   // Subroutine of is_viable_call.
   // Return true if 'fun' is a viable candidate function of the
   // list of arguments  `args'.
   static FullApplication
   acceptable_arguments(BasicContext& ctx, SimpleFuncElaboration fun,
                        const AstSequence& arglist) {
      const ArrowType* ftype = fun.type();
      const std::size_t nargs = arglist.size();
      if (nargs != ftype->arity())
         return { };
      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         if (auto arg = try_initializer(ctx, arglist[i], ftype->argument(i)))
            args[i] = arg;
         else
            return { };
      }
      return { fun, args };
   }

   // Subroutine of is_viable_call.
   // Return true if 'fun' is a viable candidate function of the
   // list of arguments  `args'.
   static FullApplication
   acceptable_arguments(BasicContext& ctx, DependentElaboration fun,
                        const AstSequence& arglist) {
      const ProductType* pt = fun.type();
      const std::size_t nargs = arglist.size();
      if (nargs != pt->arity())
         return { };
      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         if (auto arg = try_initializer(ctx, arglist[i], source_type_at(*pt,i)))
            args[i] = arg;
         else
            return { };
      }
      arguments_satisfy_restriction(ctx, *fun.type(), args);
      return { fun, args };
   }

   static void
   print_matching_problem(BasicContext& ctx, const Expression* expr,
                          const Expression* pat) {
      if (not ctx->enabled(debug::matching))
         return;
      ctx->debug() << "============= Matching Problem =============" << std::endl;
      prefix_form(ctx->debug() << "given: ", expr);
      ctx->debug() << std::endl;
      prefix_form(ctx->debug() << "pattern: ", pat);
      ctx->debug() << std::endl;
      ctx->debug() << "============================================" << std::endl;
   }

   static void
   print_no_matching_solution(BasicContext& ctx) {
      if (ctx->enabled(debug::matching))
         ctx->debug() << "============= No Solution =============" << std::endl;
   }

   static void
   print_substitution(BasicContext& ctx, const Substitution& subst) {
      if (not ctx->enabled(debug::instantiation))
         return;
      ctx->debug() << "============= Substitution =============" << std::endl;
      for (auto& e : subst) {
         ctx->debug() << '\t' << *e.first
                      << " ~> "
                      << show_expr(e.second.code())
                      << '\n';
      }
      ctx->debug() << "========================================" << std::endl;
   }

   namespace {
      struct InstantiationData {
         Substitution subst;
         Elaboration pattern;

         explicit operator bool() const { return not subst.failed(); }
      };
   }

   namespace {
      struct UniversalArrow {
         const Formals formals;
         Elaboration constraint;
         const ArrowType* const arrow;
         const Expression* qexpr;
         explicit operator bool() const { return arrow != nullptr; }
      };

      struct UniversalProduct {
         const Formals formals;
         Elaboration constraint;
         const ProductType* const product;
         const Expression* qexpr;
         explicit operator bool() const { return product != nullptr; }
      };
   }

   static void
   print_instantiation_request(BasicContext& ctx, const UniversalArrow& ua) {
      ctx->debug() << "========== instantiate_arrow ==========\n";
      ctx->debug() << "\ttype: " << show_expr(ua.arrow) << '\n';
      ctx->debug() << "\tqexpr: " << show_expr(ua.qexpr) << '\n';
      ctx->debug() << "=======================================\n";
   }

   static void
   print_instantiation_request(BasicContext& ctx, const UniversalProduct& ua) {
      ctx->debug() << "========== instantiate_arrow ==========\n";
      ctx->debug() << "\ttype: " << show_expr(ua.product) << '\n';
      ctx->debug() << "\tqexpr: " << show_expr(ua.qexpr) << '\n';
      ctx->debug() << "=======================================\n";
   }

   static void
   check_all_formals_have_values(BasicContext& ctx, const Formals& formals,
                                 const UnificationContext& uc) {
      for (auto p : formals)
         if (not uc.subst.has(p))
            semantics_error(ctx, "no substitution value for "
                            + quote(show(p->name()->symbol())));
   }

   static bool
   check_subst_is_closed(const Substitution& subst) {
      for (auto& s: subst)
         if (not is_closed(s.second))
            return false;
      return true;
   }

   static bool
   is_a_coercion(const Expression* e) {
      if (auto read = is<Read>(e))
            if (auto cmpt = is<Component>(read->address().code()))
               if (auto name = cmpt->name())
                  return name->symbol().string() == "per"
                      or name->symbol().string() == "rep";
      if (auto l = is<Lambda>(e))
         if (auto name = l->name())
            return name->symbol().string() == "per"
                or name->symbol().string() == "rep";
      return false;
   }

   static void
   remember_closed_specialization(BasicContext& ctx, const Substitution& subst,
                                  Elaboration e)
   {
      if (check_subst_is_closed(subst))
         if (not is_a_coercion(e.code()))
            ctx->remember_specialization(e);
   }

   static SimpleFuncElaboration
   instantiate_arrow(BasicContext& ctx, const UniversalArrow& ua,
                     const Substitution& subst, const Evidence* evidence)
   {
      if (ctx->enabled(debug::instantiation))
         print_instantiation_request(ctx, ua);
      auto e = evaluate(ctx.elaborator(), { ua.arrow, ua.qexpr });
      if (auto lam = is<Lambda>(e)) {
         SimpleFuncElaboration fun { ua.arrow, lam->body().code() };
         // FIXME: `e` is not being used. Why not?
         if (evidence != nullptr)
            fun = substitute(ctx.elaborator(), fun, *evidence);
         fun = substitute(ctx.elaborator(), fun, subst);
         remember_closed_specialization(ctx, subst, fun);
         return fun;
      }
      SimpleFuncElaboration ftype { ua.arrow, ua.qexpr };
      if (evidence != nullptr)
         ftype = substitute(ctx.elaborator(), ftype, *evidence);
      ftype = substitute(ctx.elaborator(), ftype, subst);
      remember_closed_specialization(ctx, subst, ftype);
      return ftype;
   }

   static DependentElaboration
   instantiate_product(BasicContext& ctx, const UniversalProduct& ua,
                       const Substitution& subst, const Evidence* evidence)
   {
      if (ctx->enabled(debug::instantiation))
         print_instantiation_request(ctx, ua);
      auto e = evaluate(ctx.elaborator(), { ua.product, ua.qexpr });
      if (auto lam = is<Lambda>(e)) {
         DependentElaboration fun { ua.product, lam->body().code() };
         // FIXME: `e` is not being used. Why not?
         if (evidence != nullptr)
            fun = substitute(ctx.elaborator(), fun, *evidence);
         fun = substitute(ctx.elaborator(), fun, subst);
         remember_closed_specialization(ctx, subst, fun);
         return fun;
      }
      DependentElaboration ftype { ua.product, ua.qexpr };
      if (evidence != nullptr)
         ftype = substitute(ctx.elaborator(), ftype, *evidence);
      ftype = substitute(ctx.elaborator(), ftype, subst);
      remember_closed_specialization(ctx, subst, ftype);
      return ftype;
   }

   static const Evidence*
   constraint_satisfied(BasicContext& ctx, Elaboration constraint,
                        const Substitution& subst)
   {
      if (constraint) {
         auto c_inst = subst_expr(ctx.elaborator(), constraint, subst);
         const Evidence& ev = discharge_constraint(ctx, c_inst.code());
         if (not ev.good())
            semantics_error(ctx, "could not satisfy constraint "
                                 + quote(show_expr(c_inst.code())));
         return &ev;
      }
      return nullptr;
   }

   static bool
   ref_and_ref_const_case(const Type* source, const Type* target) {
      if (is<ReferenceType>(source))
         if (auto ref_target = is<ReferenceType>(target))
            return is<ReadonlyType>(ref_target->referee().code());
      return false;
   }

   static void
   check_arity(BasicContext& ctx, std::size_t nargs, std::size_t arity) {
      if (nargs != arity) {
         std::ostringstream os;
         os << "function has arity of " << arity << ", but have " << nargs
            << " arguments";
         semantics_error(ctx, os.str());
      }
   }

   static bool
   ref_ref_and_val_case(const Type* t, const Type* u) {
      if (auto ref_t = is<ReferenceType>(t))
         if (is<ReferenceType>(ref_t->referee().code()))
            return not is<ReferenceType>(u);
      return false;
   }

   static bool
   ref_ref_and_ref_case(const Type* t, const Type* u) {
      if (auto ref_t = is<ReferenceType>(t))
         if (is<ReferenceType>(ref_t->referee().code()))
            if (auto ref_u = is<ReferenceType>(u))
               return not is<ReferenceType>(ref_u->referee().code());
      return false;
   }

   // coerce(t,u) is true when t and u may be coerced to structurally equal
   // types. In a unification problem, t and u must be coerced and then a
   // pattern match.
   static void
   apply_top_level_coerce(BasicContext& ctx, Elaboration& arg,
                          TypeElaboration& t)
   {
      if (ref_ref_and_val_case(arg.type(), t.code())) {
         arg = rvalue(ctx, rvalue(ctx, arg));
      } if (ref_ref_and_ref_case(arg.type(), t.code())) {
         arg = rvalue(ctx, arg);
      } else if (ref_and_ref_const_case(arg.type(), t)) {
         t = is<ReadonlyType>(is<ReferenceType>(t)->referee().code())->type();
         arg = rvalue(ctx, arg);
      } else if (not is<ReferenceType>(t))
         arg = rvalue(ctx, arg);
      else if (auto rt = is<ReadonlyType>(t))
         t = rt->type();
   }

   static void
   unification_error(BasicContext& ctx, Elaboration arg, TypeElaboration t) {
      std::ostringstream os;
      os << "failed to unify " << quote(show_type(arg.type())) <<  " with "
         << quote(show_type(t));
      semantics_error(ctx, os.str());
   }

   static Arguments
   unify_arguments(BasicContext& ctx, UnificationContext& uctx,
                   const UniversalArrow& ua, const AstSequence& argsyn)
   {
      const auto nargs = argsyn.size();
      check_arity(ctx, nargs, ua.arrow->arity());

      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         args[i] = ctx->elaborate(argsyn[i]);
         auto t = ua.arrow->argument(i);
         apply_top_level_coerce(ctx, args[i], t);
         print_matching_problem(ctx, args[i].type(), t);
         if (not uctx.match_type({ ctx->get_typename(), args[i].type() }, t)) {
            print_no_matching_solution(ctx);
            unification_error(ctx, args[i], t);
            return { };
         }
         print_substitution(ctx, uctx.subst);
      }
      return args;
   }

   static Arguments
   unify_arguments(BasicContext& ctx, UnificationContext& uctx,
                   const UniversalProduct& up, const AstSequence& argsyn)
   {
      const auto nargs = argsyn.size();
      check_arity(ctx, nargs, up.product->arity());

      Arguments args(nargs);
      for (std::size_t i = 0; i < nargs; ++i) {
         args[i] = ctx->elaborate(argsyn[i]);
         auto t = source_type_at(*up.product, i);;
         apply_top_level_coerce(ctx, args[i], t);
         print_matching_problem(ctx, args[i].type(), t);
         if (not uctx.match_type({ ctx->get_typename(), args[i].type() }, t)) {
            print_no_matching_solution(ctx);
            unification_error(ctx, args[i], t);
            return { };
         }
         print_substitution(ctx, uctx.subst);
      }
      return args;
   }

   static void
   unify_result_type(BasicContext& ctx, UnificationContext& uctx,
                     const UniversalArrow& ua, const Type* target)
   {
      if (target != nullptr)
         uctx.match_type(type_elaboration(ctx, target),
                         ua.arrow->target().code());
   }

   static void
   unify_result_type(BasicContext& ctx, UnificationContext& uctx,
                     const UniversalProduct& up, const Type* target)
   {
      if (target != nullptr)
         uctx.match_type(type_elaboration(ctx, target),
                         up.product->target().code());
   }

   static FullApplication
   instantiate_if_viable_call(BasicContext& ctx, const UniversalArrow& ua,
                              const AstSequence& argsyn, const Type* target)
   {
      UnificationContext uctx { ctx.elaborator(), ua.formals, { } };
      Arguments args = unify_arguments(ctx, uctx, ua, argsyn);
      unify_result_type(ctx, uctx, ua, target);
      check_all_formals_have_values(ctx, ua.formals, uctx);

      auto ev = constraint_satisfied(ctx, ua.constraint, uctx.subst);
      auto fun = instantiate_arrow(ctx, ua, uctx.subst, ev);
      auto arrow_t = fun.type();
      for (std::size_t i = 0; i < argsyn.size(); ++i)
         args[i] = ctx->coerce(args[i], arrow_t->argument(i));
      return { fun, args };
   }

   static FullApplication
   instantiate_if_viable_call(BasicContext& ctx, const UniversalProduct& up,
                              const AstSequence& argsyn, const Type* target)
   {
      UnificationContext uctx { ctx.elaborator(), up.formals, { } };
      Arguments args = unify_arguments(ctx, uctx, up, argsyn);
      unify_result_type(ctx, uctx, up, target);
      check_all_formals_have_values(ctx, up.formals, uctx);

      auto ev = constraint_satisfied(ctx, up.constraint, uctx.subst);
      auto fun = instantiate_product(ctx, up, uctx.subst, ev);
      arguments_satisfy_restriction(ctx, *fun.type(), args);
      auto product_t = fun.type();
      for (std::size_t i = 0; i < argsyn.size(); ++i)
         args[i] = ctx->coerce(args[i], source_type_at(*product_t, i));
      return { fun, args };
   }

   static FullApplication
   try_instantiate_if_viable_call(BasicContext& ctx, const UniversalArrow& ua,
                                  const AstSequence& argsyn, const Type* target)
   {
      try {
         return instantiate_if_viable_call(ctx, ua, argsyn, target);
      }
      catch (...) {
         return { };
      }
   }

   static FullApplication
   try_instantiate_if_viable_call(BasicContext& ctx, const UniversalProduct& up,
                                  const AstSequence& argsyn, const Type* target)
   {
      try {
         return instantiate_if_viable_call(ctx, up, argsyn, target);
      }
      catch (...) {
         return { };
      }
   }

   static UniversalArrow
   universal_arrow(Elaboration e) {
      if (auto qt = is<QuantifiedType>(e.type())) {
         if (qt->quantifier() == Quantifier::forall)
            if (auto ftype = is<ArrowType>(qt->abstract_instance()))
               return { qt->formals(), qt->constraint(), ftype, e.code() };
      }
      return { { },{ }, nullptr, nullptr };  // FIXME: should be { }, silly GCC.
   }

   static UniversalProduct
   universal_product(Elaboration e) {
      if (auto qt = is<QuantifiedType>(e.type())) {
         if (qt->quantifier() == Quantifier::forall)
            if (auto ftype = is<ProductType>(qt->abstract_instance()))
               return { qt->formals(), qt->constraint(), ftype, e.code() };
      }
      return { { },{ }, nullptr, nullptr };  // FIXME: should be { }, silly GCC.
   }

   static FullApplication
   is_viable_call(BasicContext& ctx, Elaboration e, const AstSequence& args,
                  const Type* target)
   {
      if (auto f = is_functoid(ctx, e))
         return acceptable_arguments(ctx, f, args);
      else if (auto f = is_dependent_functoid(ctx, e))
         return acceptable_arguments(ctx, f, args);
      else if (auto ua = universal_arrow(e))
         return try_instantiate_if_viable_call(ctx, ua, args, target);
      else if (auto up = universal_product(e))
         return try_instantiate_if_viable_call(ctx, up, args, target);
      return { };
   }

   // Used to to report overload resolution diagnostics.
   static void
   no_match(const Fiber& candidates, const Type* target, BasicContext& ctx) {
      std::ostringstream os;
      if (candidates.size() > 1) {
         os << "no suitable match amoung the following candidates";
         if (target != nullptr)
            os << " with target type " << quote(show(*target));
         os << ':';
      }
      else
         os << "invalid call to:";
      os << '\n';
      for (auto& x : candidates)
         format_as_decl(os << "   ", x) << std::endl;
      
      semantics_error(ctx, os.str());
   }

   static void
   print_candidates(BasicContext& ctx, const Fiber& funs) {
      if (not ctx->enabled(debug::overload))
         return;
      ctx->debug() << "========= Overload candidates ==========" << std::endl;
      for (auto& f : funs)
         format_as_decl(ctx->debug(), f) << std::endl;
      ctx->debug() << "========================================" << std::endl;
   }

   // Attempt to resolve a candidate in an overload set.
   template<typename Builder>
   static std::pair<FunctionElaboration, Elaboration>
   resolve_candidate(BasicContext& ctx, Builder builder, Elaboration f,
                     const AstSequence& args, const Type* target)
   {
      if (auto x = is_viable_call(ctx, rvalue(ctx, f), args, target)) {
         auto e = call_with(ctx, builder, x);
         if (auto y = successful_coercion(ctx, e, target))
            return { x.function(), y };
      }
      return std::pair<FunctionElaboration, Elaboration>({ }, { });
   }

   template<typename Builder>
   void
   debug_overload_failure_x(BasicContext& ctx, Builder builder, Elaboration cand,
                          const AstSequence& args, const Type* target)
   {
      cand = rvalue(ctx, cand);
      if (auto f = is_functoid(ctx, cand)) {
         auto fapp = debug_acceptable_arguments(ctx, f, args);
         auto e = call_with(ctx, builder, fapp);
         ctx->coerce(e, target);
      } else if (auto ua = universal_arrow(cand)) {
         auto fapp = instantiate_if_viable_call(ctx, ua, args, target);
         auto e = call_with(ctx, builder, fapp);
         ctx->coerce(e, target);
      } else if(auto f = is_dependent_functoid(ctx, cand)) {
         auto fapp = debug_acceptable_arguments(ctx, f, args);
         auto e = call_with(ctx, builder, fapp);
         ctx->coerce(e, target);
      } else if (auto up = universal_product(cand)) {
         auto fapp = instantiate_if_viable_call(ctx, up, args, target);
         auto e = call_with(ctx, builder, fapp);
         ctx->coerce(e, target);
      } else {
         std::ostringstream os;
         os << "is not callable\n";
         semantics_error(ctx, os.str());
      }
   }

   template <typename Exception_t, typename Ostream_t>
   void
   print_candidate_error(const Exception_t& err, Elaboration f, Ostream_t& os) {
      os << std::endl << "candidate `" << pretty(f.code()) << " : "
               << pretty(f.type()) << "` ";
      err.issue_on(os);
      os << std::endl;
   }

   template<typename Builder>
   void
   debug_overload_failure(BasicContext& ctx, Builder builder, const Fiber& funs,
                          const AstSequence& args, const Type* target)
   {
      std::ostringstream os;
      os << "overload failure: candidates are" << std::endl;
      for(auto& f: funs) {
         try {
            debug_overload_failure_x(ctx, builder, f, args, target);
            internal_error("trying to debug a perfectly fine candidate");
         } catch (const CoercionError& err) {
            print_candidate_error(err, f, os);
         } catch (const SemanticsErrorAt& err) {
            print_candidate_error(err, f, os);
         } catch (const UndeclaredError& err) {
            print_candidate_error(err, f, os);
         }
      }
      semantics_error(ctx, os.str());
   }

   void
   debug_ambiguous_overload(BasicContext& ctx,
            const vector<std::pair<FunctionElaboration, Elaboration>>& ovlds)
   {
      std::ostringstream os;
      os << "ambiguous overload: " << ovlds.size() << " candidates\n";
      for (auto& ovld: ovlds) {
         os << "candidate `" << pretty(ovld.first.code()) << " : "
            << pretty(ovld.first.type()) << '`' << std::endl;
      }
      overload_error(ctx, os.str());
   }

   // Given a sequence of `arguments' and an overload `funs', select
   // the first function whose source type is satisfied by the arguments.
   template<typename Builder>
   static Elaboration
   resolve_overload(BasicContext& ctx, Builder builder, const Ast* op,
                    const AstSequence& args, const Type* target) {
      Fiber funs = semantics_fiber(ctx, op);
      // FIXME: do ADL before giving up.
      if (funs.empty())
         semantics_error(ctx, "no such operation in scope", op);
      print_candidates(ctx, funs);
      using CallInfo = std::pair<FunctionElaboration, Elaboration>;
      vector<CallInfo> data;
      for (auto& f : funs) {
         auto res_f = resolve_candidate(ctx, builder, f, args, target);
         if (res_f.second)
            data.push_back(res_f);
      }
      if (data.size() == 1)
         return data.front().second;
      if (data.size() > 1)
         debug_ambiguous_overload(ctx, data);
      else {
         debug_overload_failure(ctx, builder, funs, args, target);
      }
      return { };
   }

   // True iff the ApplyAst represents an explicit instantiation.
   static bool
   is_explicit_instantiation(const ApplyAst& x) {
      if (auto args = x.arguments())
         if (auto bracket = args->bracket())
            return bracket->first().kind ==  token::open_mbracket_tok
               and bracket->second().kind == token::close_mbracket_tok;
      return false;
   }

   // True iff the elaboration is a quantified expression.
   static QuantifiedElaboration
   is_quantifiedoid(BasicContext& ctx, Elaboration e) {
      if (auto qt = is<QuantifiedType>(e.type()))
         return { qt, e.code() };
      else if (is<ReferenceType>(e.type()))
         return is_quantifiedoid(ctx, rvalue(ctx, e));
      else
         return { };
   }

   static QuantifiedElaboration
   is_universally_quantifiedoid(BasicContext& ctx, Elaboration e) {
      if (auto qe = is_quantifiedoid(ctx,e))
         if (qe.type()->quantifier() == Quantifier::forall)
            return qe;
      return { };
   }

   static Elaboration
   resolve_by_result_type(BasicContext& ctx, QuantifiedElaboration qe,
                          const Type* t)
   {
      UnificationContext uctx { ctx.elaborator(), qe.type()->formals(), { } };
      uctx.match_type({ ctx->get_typename(), t },
                      qe.type()->abstract_instance().code());
      if (not uctx.subst)
         semantics_error(ctx, "could not deduce formals by result type");
      check_all_formals_have_values(ctx, qe.type()->formals(), uctx);
      // Ensure the constraints are satisfied.
      auto ev = constraint_satisfied(ctx, qe.type()->constraint(), uctx.subst);
      // Get lambda form.
      auto qf = evaluate(ctx.elaborator(), qe);
      // FIXME: Could this be a constructor?
      Elaboration e { qe.type()->abstract_instance(),
                      is<Lambda>(qf.code())->body().code() };
      if (ev != nullptr)
         e = subst_expr(ctx.elaborator(), e, *ev);
      e = subst_expr(ctx.elaborator(), e, uctx.subst);
      remember_closed_specialization(ctx, uctx.subst, e);
      return e;
   }

   static Elaboration
   try_resolve_by_result_type(BasicContext& ctx, QuantifiedElaboration qe,
                              const Type* t)
   {
      try {
         return resolve_by_result_type(ctx,qe,t);
      }
      catch (const SemanticsErrorAt&) { }
      catch (const UndeclaredError&) { }
      return { };
   }

   static Fiber
   fiber_section(BasicContext& ctx, const Fiber& f, const Type* t) {
      if (t == nullptr)
         return f;
      Fiber s;
      for (auto& e : f) {
         // Attempt to resolve by overloading
         if (auto qf = is_universally_quantifiedoid(ctx, e)) {
            if (auto e = try_resolve_by_result_type(ctx, qf, t))
               s.push_back(e);
         // Otherwise, just attempt to coerce
         } else if (auto x = successful_coercion(ctx, e, t))
            s.push_back(x);
      }
      return s;
   }

   static void
   not_a_value_error(BasicContext& ctx, const Expression* e) {
      std::ostringstream os;
      os << "attempting to instantiate over " << quote(show_expr(e))
         << " which is not a value\n";
      semantics_error(ctx, os.str());
   }

   // FIXME: refactor into a class of expressions like `is_closed`.
   // True iff the expression is a `Value` or a symbolic `Value`.
   static bool
   is_value(const Expression* e) {
      if (is<Value>(e))
         return true;
      if (auto te = is<TypeExpression>(e))
         e = te->expr().code();
      if (auto r = is<Read>(e))
         return is<Formal>(r->address().code());
      return false;
   }

   static void
   check_value_substitution(BasicContext& ctx, Substitution subst) {
      for (auto& s: subst)
         if (not is_value(s.second.code()))
            not_a_value_error(ctx, s.second.code());
   }

   // If the arguments are coercible to the quantified type's parameter types,
   // then construct an instantiation.
   static Elaboration
   can_instantiate_on(BasicContext& ctx, QuantifiedElaboration e,
                         const AstSequence& args)
   {
      auto qt = e.type();
      const std::size_t nargs = args.size();
      if (nargs != qt->formals().size())
         return { };
      Substitution subst;
      for (std::size_t i = 0; i != nargs; i++)
         if (auto arg = try_initializer(ctx, args[i], qt->formals()[i]->type()))
            subst[qt->formals()[i]] = evaluate(ctx.elaborator(), arg);
         else
            return { };
      check_value_substitution(ctx, subst);
      auto f = evaluate(ctx.elaborator(), e);
      if (auto lam = is<Lambda>(f)) {
         f = lam->body();
         if (auto ev = constraint_satisfied(ctx, e.type()->constraint(),subst))
            f = subst_expr(ctx.elaborator(), lam->body(), *ev);
         auto g = subst_expr(ctx.elaborator(), f, subst);
         remember_closed_specialization(ctx, subst, g);
         return g;
      }
      // Are there any builtin lambdas?
      semantics_error(ctx, "expression " + quote(show(*e.code()))
                         + " is not reducible to a lambda; instead it is "
                         + quote(show(*f.code())));
      return { };
   }

   // Returns an instantiation iff the alaboration is a quantified type and the
   // arguments are coercible to the quantifier types.
   static Elaboration
   is_viable_specialization(BasicContext& ctx, Elaboration e,
                            const AstSequence& args)
   {
      if (auto q = is_quantifiedoid(ctx, e))
         return can_instantiate_on(ctx, q, args);
      return { };
   }

   // Given a sequence of `arguments' and an overload `funs', select
   // the first function whose source type is satisfied by the arguments.
   static Elaboration
   resolve_as_specialization(BasicContext& ctx, const Ast* op,
                             const AstSequence& args, const Type* target)
   {
      Fiber funs = semantics_fiber(ctx, op);
      // FIXME: do ADL before giving up.
      if (funs.empty())
         semantics_error(ctx, "no such operation in scope", op);
      print_candidates(ctx, funs);
      vector<Elaboration> candidates;
      for (auto& f : funs) {
         if (auto e = is_viable_specialization(ctx, rvalue(ctx, f), args))
            if (auto f = successful_coercion(ctx, e, target))
               candidates.push_back(f);
      }
      if (candidates.empty())
         no_match(funs, target, ctx);
      else if (candidates.size() > 1)
         semantics_error(ctx, "ambiguous specialization");
      return candidates.front();
   }

   // An overloaded function is called with argument expressions `args'.
   // Elaborate the arguments, perform overload resolution, then return
   // an elaboration for the whole call.
   static Elaboration
   elaborate_call(BasicContext& ctx, const Ast* op, const AstSequence& args,
                  const Type* t) {
      return resolve_overload(ctx, &Elaborator::build_call, op, args, t);
   }

   static Elaboration
   elaborate_call(BasicContext& ctx, const ApplyAst& x, const Type* t) {
      if (is_explicit_instantiation(x))
         return resolve_as_specialization(ctx, x.operation(), get_arguments(x),
                                          t);
      return elaborate_call(ctx, x.operation(), get_arguments(x), t);
   }

   static Elaboration
   elaborate_juxtapose(BasicContext& ctx, const JuxtaposeAst& x, const Type* t) {
      return elaborate_call(ctx, x.operation(), { x.argument() }, t);
   }

   static std::pair<intmax_t,intmax_t>
   elaborate_key_element(BasicContext& ctx, const SequenceAst* x) {
      auto& seq = x->sequence();
      if (seq.size() != 2)
         semantics_error(ctx, "key element must be a protocol-field pair");
      auto fst = ctx->elaborate(seq[0], ctx->get_int());
      fst = evaluate(ctx.elaborator(), fst);
      if (not is<Int>(fst.code()))
         semantics_error(ctx, "key element protocol is not a constant");
      auto snd = ctx->elaborate(seq[1], ctx->get_int());
      snd = evaluate(ctx.elaborator(), snd);
      if (not is<Int>(snd.code()))
         semantics_error(ctx, "key element field is not a constant");
      intmax_t fst_int = is<Int>(fst.code())->rep();
      intmax_t snd_int = is<Int>(snd.code())->rep();
      return std::make_pair(fst_int, snd_int);
   }

   static std::pair<intmax_t,intmax_t>
   elaborate_key_element(BasicContext& ctx, const Ast* x) {
      if (auto y = is<EnclosureAst>(x))
         if (is_bracketed(*y))
            if (auto z = is<SequenceAst>(y->expr()))
               return elaborate_key_element(ctx, z);
      semantics_error(ctx, "key element must be a protocol-field pair");
      return { };
   }

   static SubsetKeyValue
   elaborate_key_sequence(BasicContext& ctx, const Ast* x) {
      if (x == nullptr)
         return { };
      if (auto y = is<SequenceAst>(x)) {
         auto& seq = y->sequence();
         SubsetKeyValue keys(seq.size());
         for (std::size_t i = 0; i != seq.size(); ++i)
            keys[i] = elaborate_key_element(ctx, seq[i]);
         return keys;
      } else
         return { elaborate_key_element(ctx, x) };
   }

   static Elaboration
   elaborate_key_value(BasicContext& ctx, const Ast& x) {
      if (auto y = is<EnclosureAst>(&x))
         if (is_bracketed(*y)) {
            auto key = elaborate_key_sequence(ctx, y->expr());
            return { ctx->get_key(), ctx->build_key(key) };
         }
      semantics_error(ctx, "argument to key must be a bracketed sequence of "
                           "pairs");
      return { };
   }

   // Elaborate a prefix expression.
   template<typename T>
   static Elaboration
   elaborate_prefix(BasicContext& ctx, const UnaryAst& x,
                    unary_builder<T> builder, const Type* t) {
      if (x.operation()->token()->kind == token::key_tok)
         return elaborate_key_value(ctx, *x.argument());
      return resolve_overload
         (ctx, builder, x.operation(), { x.argument() }, t);
   }

   static Elaboration
   elaborate_reference(BasicContext& ctx, const Ast* x) {
      auto type = elaborate_type(ctx, x);
      return { ctx->get_typename(), ctx->make_reference_type(type) };
   }

   static Elaboration
   elaborate_readonly(BasicContext& ctx, const Ast* x) {
      auto type = elaborate_type(ctx, x);
      return { ctx->get_typename(), ctx->make_readonly_type(type) };
   }

   static Elaboration
   elaborate_assumption(BasicContext& ctx, const Ast* property) {
      // FIXME: check that we are actually elaborating a previously
      //        defined axiom; not just any arbitrary expression.
      auto prop = ctx->elaborate(property, ctx->get_bool());
      load_assumption(ctx, prop);
      return { ctx->get_bool(), ctx->build_bool(true, ctx->get_bool()) };
   }

   static Elaboration
   elaborate_unary(BasicContext& ctx, const UnaryAst& x, const Type* t) {
      Elaborator::LocationManager push_loc(ctx.elaborator(), *anchor(x.operation()));
      switch (x.operation()->token()->kind) {
      default:
         return elaborate_call(ctx, x.operation(), { x.argument() }, t);
         
      case token::minus_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_negate, t);
         
      case token::not_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_not, t);
         
      case token::tilde_tok:
         return elaborate_prefix(ctx, x, &Elaborator::build_complement, t);
         
      case token::ampersand_tok:
         return ctx->coerce(elaborate_reference(ctx, x.argument()), t);
         
      case token::const_tok:
         return ctx->coerce(elaborate_readonly(ctx, x.argument()), t);
         
      case token::assume_tok:
         return ctx->coerce(elaborate_assumption(ctx, x.argument()), t);

      case token::key_tok:
         return elaborate_key_value(ctx, *x.argument());
      }
   }

   // Evaluate an infix expression.
   template<typename T>
   static Elaboration
   elaborate_infix(BasicContext& ctx, const BinaryAst& x,
                   binary_builder<T> builder, const Type* t) {
      return resolve_overload
         (ctx, builder, x.operation(), { x.lhs(), x.rhs() }, t);
   }

   // Subroutine of Elaborator::coerce_to_type.
   // A constructor is being used as a type, check that can be so.
   static TypeElaboration
   check_ctor_as_type(BasicContext& ctx, const Constructor* ctor) {
      auto ctype = is<ArrowType>(ctor->type());
      if (ctype == nullptr or ctype->arity() != 1
          or ctype->target() != ctx->get_concept())
         semantics_error
            (ctx, "only a unary concept constructor can be used as type");
      auto type = ctype->argument(0);
      return { type.type(), ctx->make_restricted_type(type, { ctype, ctor }) };
   }

   // -- We have an expression that semantically designates a type.
   // -- Return the typeful version representation reflecting that
   // -- knowledge.
   TypeElaboration
   Elaborator::coerce_to_type(Elaboration expr) {
      BasicContext ctx { this };
      expr = evaluate(this, rvalue(ctx, expr));
      if (auto type = is<Type>(expr.code()))
         return { expr.type(), type };
      else if (auto ctor = is<Constructor>(expr))
         return check_ctor_as_type(ctx, ctor);
      else if (not has_type_values(expr.type(), ctx))
         semantics_error(ctx, quote(show_expr(expr.code()))
                         + " does not designate a type");
      return { expr.type(), ctx->make_type_expression(expr) };
   }

   // Ensure `expr' is an lvalue expression.
   static Elaboration
   lvalue(BasicContext& ctx, Elaboration expr) {
      if (not is<ReferenceType>(expr.type()))
         semantics_error(ctx, quote(show(expr.code()))
                         + " is not an lvalue");
      return expr;
   }
   
   // Elaborate an assignment statement.
   static Elaboration
   elaborate_assignment(BasicContext& ctx, const Ast* x, const Ast* y) {
      Elaboration rhs = ctx->elaborate(y);
      //  1. Elaborate the left hand side to a memory location.
      Elaboration place = lvalue(ctx, ctx->elaborate(x));
      auto type = is<ReferenceType>(place.type());
      // 2. Convert the right hand side to the appropriate type.
      Elaboration value = ctx->coerce(rhs, type->referee());
      // FIXME: check that the type is regular. and for classes
      // FIXME: look up assignment operator.
      return { type, ctx->build_write(place, value) };
   }

   static Substitution
   deduce_pattern_formals(TypeElaboration x, const Type* pattern) {
      auto t = is<ReferenceType>(x.code());
      auto p = is<ReferenceType>(pattern);
      if (t != nullptr and p == nullptr)
         return deduce_pattern_formals(t->referee(), pattern);
      else if (t == nullptr and p != nullptr)
         return deduce_pattern_formals(x, p->referee());
      else if (auto tt = is<ReadonlyType>(x))
         return deduce_pattern_formals(tt->type(), pattern);
      else if (auto pp = is<ReadonlyType>(pattern))
         return deduce_pattern_formals(x, pp->type());
      return pattern_match(x, pattern);
   }

   static const Literal*
   get_name(BasicContext& ctx, const LiteralForm& x) {
      return literal_name(ctx, x.token());
   }

   static const Identifier*
   get_name(BasicContext& ctx, const AlphabethicForm& x) {
      return ctx->build_identifier(x.token());
   }

   static const Operator*
   get_name(BasicContext& ctx, const OperatorForm& x) {
      return ctx->build_operator(x.token());
   }

   static const Identifier*
   get_name(BasicContext& ctx, const CallForm& head) {
      return ctx->build_identifier(&head.function());
   }
   
   static const Operator*
   get_name(BasicContext& ctx, const InfixForm& head) {
      return ctx->build_operator(head.operation()->token());
   }

   static const Operator*
   get_name(BasicContext& ctx, const PrefixForm& head) {
      return ctx->build_operator(ctx->intern(head.operation()->token()));
   }

   static const Operator*
   get_name(BasicContext& ctx, const SuffixForm& head) {
      return ctx->build_operator(ctx->intern(head.operation()->token()));
   }

   static const Operator*
   get_name(BasicContext& ctx, const ClosedForm& head) {
      return make_name(ctx, *head.bracket());
   }

   static const Name*
   get_name(BasicContext& ctx, const FixityForm* f) {
      struct V : FixityForm::Visitor {
         BasicContext& ctx;
         const Name* result;
         V(BasicContext& c) : ctx(c), result() { }
         void visit(const LiteralForm& x) { result = get_name(ctx, x); }
         void visit(const AlphabethicForm& x) { result = get_name(ctx, x); }
         void visit(const OperatorForm& x) { result = get_name(ctx, x); }
         void visit(const PrefixForm& x) { result = get_name(ctx, x); }
         void visit(const SuffixForm& x) { result = get_name(ctx, x); }
         void visit(const InfixForm& x) { result = get_name(ctx, x); }
         void visit(const ClosedForm& x) { result = get_name(ctx, x); }
         void visit(const CallForm& x) { result = get_name(ctx, x); }
      };
      V v { ctx };
      f->accept(v);
      return v.result;
   }

   // Return true if `x' is appropriate at non-local scopes.
   static bool
   valid_statement_at_nonlocal_scope(const Ast* x) {
      struct V {
         bool result;
         void operator()(const Ast&) { }
         void operator()(const DefinitionAst&) { result = true; }
         void operator()(const SignatureAst&) { result = true; }
         void operator()(const RuleAst&) { result = true; }
         void operator()(const PostulateAst&) { result = true; }
         void operator()(const ProlongAst&) { result = true; }
         void operator()(const ExprStmtAst& x) {
            if (x.expression() == nullptr)
               error_at(x.semicolon(), "extraneous semicolon");
         }
      };
      ast_visitor<V> v{ false };
      x->accept(v);
      return v.result;
   }

   static void
   filter_nonlocal_statement(BasicContext& ctx, const Ast* x) {
      if (x != nullptr and not valid_statement_at_nonlocal_scope(x))
         semantics_error(ctx, "invalid statement at non-local scope", x);
   }

   static const Constructor*
   finish_user_ctor(BasicContext& ctx, const Name* n, TypeElaboration type,
                    const Ast* x) {
      auto t = is<ArrowType>(type);
      if (t == nullptr)
         semantics_error(ctx, "user-supplied constructors must have "
                         "function type", x);
      auto impl_type = make_predicate_type(ctx, t->source());
      Elaboration impl = ctx->elaborate(x, impl_type);
      return ctx->build_constructor({ n, t } , impl);
   }

   // We are about to elaborate an entity (a structure, or a function)
   // the initialzier of which must be a compound statement.  There is
   // a singularity for "{ }" which fits both an empty block and
   // an empty brace-enclosed expression list.
   static const CompoundAst*
   get_block_or_else(BasicContext& ctx, const Ast* x) {
      if (x == nullptr)
         return nullptr;
      else if (auto y = is<EnclosureAst>(x)) {
         if (y->expr() == nullptr)
            return nullptr;
      }
      auto y = is<CompoundAst>(x);
      if (y == nullptr)
         semantics_error(ctx, "invalid initializer for scope object", x);
      return y;
   }

   static const TagType*
   elaborate_tag_type(BasicContext& ctx, const SignatureAst& x) {
      const Name* n = get_name(ctx, x.form());
      if (not ctx->current_env()->lookup(n).empty())
         semantics_error(ctx, "duplicate declaration of this tag", x.form());
      const Type* t = elaborate_type(ctx, x.type());
      return ctx->make_tag_type(n, { ctx->get_typename(), t});
   }

   static Elaboration
   elaborate_record(BasicContext& ctx, const DatatypeAst& x) {
      Sequence<TagType> fields;
      LocalScopeManager new_scope { ctx.elaborator() };
      for (auto y : x.members()) {
         auto f = elaborate_tag_type(ctx, *y);
         auto rt = make_reference_type(ctx, f->type());
         new_scope.bind(f->tag(), make_elaboration(rt, f));
         fields.push_back(f);
      }
      return { ctx->get_typename(), ctx->make_record_type(fields) };
   }

   // Create a fresh namespace scope object to hold the definition
   // of the namespace being defined.
   static Namespace*
   new_namespace(DeclContext& ctx) {
      return ctx->build_namespace(ctx.name(), ctx->current_env().base());
   }

   static void
   elaborate_members(BasicContext& ctx, const Ast* x, ToplevelScope* scope) {
      ScopeManager new_scope { ctx.elaborator(), *scope };
      const CompoundAst* block = get_block_or_else(ctx, x);
      const std::size_t nmembers = length(block);
      for (std::size_t i = 0; i < nmembers; ++i) {
         const Ast* y = block->at(i);
         filter_nonlocal_statement(ctx, y);
         scope->add_stmt(ctx->elaborate(y));
      }
   }
   
   static Elaboration
   elaborate_namespace(DeclContext& ctx, const Ast* x) {
      Namespace* value = new_namespace(ctx);
      elaborate_members(ctx, x, value);
      return { ctx->get_namespace(), value };
   }

   static const Signature*
   elaborate_signature(DeclContext& ctx, const PostulateAst* x) {
      auto n = get_name(ctx, x->form());
      auto t = elaborate_type(ctx, x->type());
      auto sig = ctx->build_signature(n, t);
      declare(ctx->current_env(), { n, t }, sig);
      return sig;
   }

   // Synthesize the type of a type alias
   static const ArrowType*
   make_alias_type(BasicContext& ctx, const Type* source_t,
                   const Type* t)
   {
      TypeElaboration te { ctx->get_typename(), t };
      if (auto arrow_t = is<ArrowType>(source_t))
         return ctx->make_arrow_type(te, arrow_t->source());
      else
         return ctx->make_arrow_type(te, { });
   }

   static void
   assume_builtins(BasicContext& ctx, const Constraint* req) {
      if (is_instance_of_constraint(req)) {
         if (auto ctor_t = is<Constructor>(req->argument(1).code()))
            emit_instance_of_signatures(ctx, *ctor_t);
      }
   }

   // FIXME: Should this be handled by overloading?
   // Hmm. This shouldn't require different syntax. Keep as a call.
   static Elaboration
   call_to_constraint(BasicContext& ctx, const CallExpression& call_e) {
      // axiom: call_e.function().type()->target().code() == ctx->get_concept()
      auto e = evaluate(ctx.elaborator(), call_e.function());
      if (auto ctor = is<Constructor>(e)) {
         auto req = ctx->build_constraint(ctor, call_e.arguments());
         assume_builtins(ctx, req);
         return { ctx->get_concept(), req };
      }
      return  { };
   }

   static Elaboration to_requirement(BasicContext&, Elaboration);

   static Elaboration
   logical_to_requirement(BasicContext& ctx, const BinaryLogical& e) {
      if (e.operation() != logical::Operation::conjunction)
         semantics_error(ctx, "concept connective unsupported ");
      Elaboration lhs = to_requirement(ctx, e.lhs());
      if (not lhs) return { };
      Elaboration rhs = to_requirement(ctx, e.rhs());
      if (not rhs) return { };
      auto cst = ctx->build_logical(e.operation(), lhs, rhs);
      return { ctx->get_concept(), cst };
   }

   static Elaboration
   to_requirement(BasicContext& ctx, Elaboration e) {
      e = evaluate(ctx.elaborator(), e);
      if (e.type() == ctx->get_concept()) {
         if (auto call_e = is<CallExpression>(e.code()))
            return call_to_constraint(ctx, *call_e);
         else if (auto logical_e = is<BinaryLogical>(e.code()))
            return logical_to_requirement(ctx, *logical_e);
      }
      return { };
   }

   static Elaboration
   elaborate_requirement(DeclContext&, ScopeRef, Concept*, const Ast*);

   static Elaboration
   elaborate_requirement_block(DeclContext& ctx, const Ast* x, ScopeRef scope,
                          Concept* c)
   {
      if (auto stmts = is<CompoundAst>(x)) {
         const std::size_t n = stmts->sequence().size();
         Arguments args(n);
         for (std::size_t i = 0; i != n; ++i)
            args[i] = elaborate_requirement(ctx,scope,c,stmts->sequence()[i]);
         return { ctx->get_concept(), ctx->build_block(args) };
      }
      else
         return elaborate_requirement(ctx, scope, c, x);
   }

   static Quantifier
   elaborate_quantifier(const QuantifiedAst& x) {
      return x.quantifier()->token()->kind == token::exists_tok
         ? Quantifier::exists
         : Quantifier::forall;
   }

   static Formals elaborate_parameters(BasicContext&, const Parameters&);

   static const Expression*
   elaborate_quantified_requirement(DeclContext& ctx, Concept& c,
                                    ScopeRef scope, const QuantifiedAst& x)
   {
      ParameterScopeManager parms_scope(ctx.elaborator());
      auto quantifier = elaborate_quantifier(x);
      auto formals = elaborate_parameters(ctx, x.parameters());
      auto body = elaborate_requirement_block(ctx, x.body(), scope, &c);
      return ctx->build_formula(quantifier, formals, body);
   }

   static bool
   is_literal(const Name* n) {
      return dynamic_cast<const Literal*>(n);
   }

   // Return true if a given declaration cannot be overloaded.
   static bool
   cannot_overload(BasicContext& ctx, const Declaration& d) {
      return has_type_denotation(d.value(), ctx)
         or not is_literal(d.name());
   }

   static void
   ensure_unique_name(BasicContext& ctx, const Name* name) {
      for (auto& d : ctx->current_env()->lookup(name))
         if (cannot_overload(ctx, d))
            semantics_error(ctx, "invalid redefinition of " + quote(name));
   }

   static Elaboration
   read(BasicContext& ctx, Elaboration e) {
      if (auto rt = is<ReferenceType>(e.type()))
         return { rt->referee(), ctx->build_read(e) };
      return e;
   }

   static Arguments
   to_args(const Formals& fs) {
      const std::size_t n = fs.size();
      Arguments args(n);
      for (std::size_t i = 0; i != n; ++i)
         args[i] = { fs[i]->type(), fs[i] };
      return args;
   }

   // An alias is parameterized by the parameters of the concept in which it
   // resides.
   static Elaboration
   skolemize_alias(DeclContext& ctx, const Signature& sig) {
      Elaboration alias;
      const Type* alias_t = nullptr;
      if (auto fctx = dynamic_cast<const FormContext*>(&ctx)) {
         auto t = is<ArrowType>(sig.link_name().type());
         FunctionElaboration f { t, &sig };
         alias_t = t->target().code();
         alias = { alias_t, ctx->build_call(f, to_args(fctx->formals())) };
      } else {
         alias_t = sig.link_name().type();
         alias = { sig.link_name().type(), &sig };
      }
      return alias;
   }

   static Elaboration
   try_coerce_to_type(BasicContext& ctx, Elaboration e) {
      try {
         return ctx->coerce_to_type(e);
      }
      catch(const SemanticsErrorAt&) {
         return e;
      }
   }

   static Elaboration
   make_alias_eq(DeclContext& ctx, const Signature& sig, Elaboration e) {
      auto alias = skolemize_alias(ctx, sig);
      alias = try_coerce_to_type(ctx, alias);
      e = try_coerce_to_type(ctx, e);
      return equality_expr(alias, e, ctx);
   }

   static const Alias*
   elaborate_alias(DeclContext& ctx, const AliasAst& x)
   {
      // FIXME: Only support aliasing variables for now.
      if (not is<IdentifierAst>(x.value()))
         semantics_error(ctx, "only support alias postulates to names");
      auto value = read(ctx, ctx->elaborate(x.value()));
      auto name = make_name(ctx, *x.alias());
      ensure_unique_name(ctx, name);
      auto t = make_alias_type(ctx, ctx.type(), value.type());
      auto alias = ctx->build_signature(name, t);
      auto eq = make_alias_eq(ctx, *alias, value);
      return ctx->build_alias(*alias, eq);
   }

   // FIXME: Create a ConceptContext
   static Elaboration
   elaborate_requirement(DeclContext& ctx, ScopeRef scope, Concept* c,
                         const Ast* x)
   {
      if (auto y = is<PostulateAst>(x)) {
         auto sig = elaborate_signature(ctx, y);
         return { ctx->get_concept(), sig };
      } else if (auto y = is<QuantifiedAst>(x)) {
         auto qreq = elaborate_quantified_requirement(ctx, *c, scope, *y);
         return { ctx->get_concept(), qreq };
      } else if(auto y = is<AliasAst>(x)) {
         auto alias = elaborate_alias(ctx, *y);
         return { ctx->get_concept(), alias };
      } else {
         auto e = ctx->elaborate(x);
         if (auto ref = to_requirement(ctx, e))
            return ref;
         else if (is_arithmetic_constraint(e, ctx->get_int()))
            return { ctx->get_arithmetic(), e.code() };
         else
            return e;
      }
   }

   static Elaboration
   elaborate_guard(BasicContext& ctx, const Ast* x) {
      if (x == nullptr)
         return { };
      auto e = ctx->elaborate(x);
      if (e.type() == ctx->get_bool())
         return e;
      if (auto ref = to_requirement(ctx, e))
         return ref;
      semantics_error(ctx, "expected a concept or boolean expression; got an "
                           "expression of type" + quote(show_type(e.type())));
      return { };
   }

   static void
   check_concept_ctor_definition(DeclContext& ctx) {
      auto type = is<ArrowType>(ctx.type());
      if (type == nullptr or type->target() != ctx->get_concept())
         semantics_error(ctx, "invalid concept constructor");
   }

   static Elaboration
   elaborate_concept(DeclContext& ctx, const Ast* x, ScopeRef scope) {
      // FIXME: We should be checking this upstream.
      check_concept_ctor_definition(ctx);
      
      LocalScopeManager new_scope { ctx.elaborator() };
      auto c = ctx->build_concept();
      if (auto stmts = is<CompoundAst>(x))
         for (auto z : *stmts){
            auto req = elaborate_requirement(ctx, scope, c, z);
            c->require(req);
            ctx->assume(req);
         }
      else
         c->require(elaborate_requirement(ctx, scope, c, x));
      return { ctx->get_concept(), c };
   }

   static Elaboration
   make_ctor_tag(BasicContext& ctx, int k) {
      return { ctx->get_int(), ctx->build_int(k, ctx->get_int()) };
   }

   static const Constructor*
   elaborate_associated_ctor(BasicContext& ctx, const SignatureAst& x, int k) {
      auto n = get_name(ctx, x.form());
      auto t = elaborate_type(ctx, x.type());
      if (const Ast* i = x.implementation())
         return finish_user_ctor(ctx, n, t, i);
      return ctx->build_constructor({ n, t }, make_ctor_tag(ctx, k));
   }

   static const DatatypeAst*
   is_inductive_definition(const Ast* x) {
      auto y = is<DatatypeAst>(x);
      if (y != nullptr and y->sort().kind == token::inductive_tok)
         return y;
      return nullptr;
   }

   static Elaboration
   elaborate_inductive_body(DeclContext& ctx, const DatatypeAst* x) {
      const std::size_t n = x->members().size();
      Sequence<Constructor> ctors(n);
      for (std::size_t i = 0; i < n; ++i) {
         auto y = x->members()[i];
         auto ctor = elaborate_associated_ctor(ctx, *y, i);
         // FIXME: Check that we have a positive occurance.
         ctors[i] = ctor;
      }
      return { ctx->get_typename(), ctx->make_variant_type(ctors) };
   }

   static Elaboration
   elaborate_initializer(DeclContext& ctx, const Type* type, const Ast* x) {
      if (type == ctx->get_typename()) {
         if (auto y = is_inductive_definition(x))
            return elaborate_inductive_body(ctx, y);
         return elaborate_type(ctx, x);
      }
      else if (type == ctx->get_namespace())
         return elaborate_namespace(ctx, x);
      else if (type == ctx->get_concept())
         return elaborate_concept(ctx, x, ctx.scope());
      return ctx->elaborate(x, type);
   }

   static void
   quantify_and_emit_constructor(BasicContext& ctx, const Constructor& ctor,
                                 const Formals& formals, Elaboration constraint)
   {
      if (not formals.empty()) {
         auto t = simplify_type(ctx.elaborator(), ctx->make_quantified_type(
                     Quantifier::forall, formals,
                     type_elaboration(ctx,ctor.type()), constraint));
         LinkName lnk { ctor.name(), t };
         auto e = ctx->build_lambda(lnk, formals, { ctor.type(), &ctor });
         ctx.scope()->define(ctor.name(), t, e);
      }
      else
         ctx.scope()->define(ctor.name(), ctor.type(), &ctor);
   }

   // Return the elaboration for a read-access of a parameter
   static Elaboration
   read(BasicContext& ctx, const Formal* f) {
      auto rt = make_reference_type(ctx, f->type());
      return { f->type(), ctx->build_read({ rt, f }) };
   }

   static Arguments
   to_type_args(BasicContext& ctx, const Formals& fs) {
      const std::size_t n = fs.size();
      Arguments args(n);
      for (std::size_t i = 0; i != n; ++i) {
         if (fs[i]->type().code() == ctx->get_typename())
            args[i] = { ctx->get_typename(),
                        ctx->make_type_expression(read(ctx, fs[i])) };
         else
            args[i] = { fs[i]->type(), fs[i] };
      }
      return args;
   }

   FunctionElaboration
   try_make_function_elaboration(BasicContext& ctx, const Type* t,
                                 const Expression* e)
   {
      if (auto arrow_t = is<ArrowType>(t))
         return { arrow_t, e };
      if (auto prod_t = is<ProductType>(t))
         return {prod_t, e };
      semantics_error(ctx, "trying to make a function elaboration with type "
                           + quote(show_type(t)) + ", but must be an arrow "
                           "type of a product type");
      return { };
   }

   // FIXME: Something is wrong. Too many parameters. What is the actual domain?
   static void
   define_quantified_coercion(BasicContext&, ScopeRef, const char*,
                              const Formals&, Elaboration, const Type*,
                              const Type*);

   static void
   emit_parametric_consorts(FormContext& ctx, const Constructor& ctor,
                            const Formals& fs, Elaboration boxed,
                            Elaboration c) {
      auto args = to_type_args(ctx, fs);
      auto subst = make_subst(ctx, fs, args);
      auto t = ctx->coerce_to_type(
                  subst_expr(ctx.elaborator(), boxed, subst)).code();
      auto func = try_make_function_elaboration(ctx, ctor.type(), &ctor);
      auto gen_t =
         ctx->coerce_to_type(evaluate(ctx.elaborator(), call(ctx, func, args)));
      define_quantified_coercion(ctx, ctx.scope(), "per", fs, c, t, gen_t);
      define_quantified_coercion(ctx, ctx.scope(), "rep", fs, c, gen_t, t);
   }

   static void
   emit_associated_ops(FormContext& ctx, const Constructor& ctor) {
      if (ctx.target_type().code() != ctx->get_typename())
         return;
      auto lam = is<Lambda>(ctor.implementation().code());
      if (lam == nullptr)
         return;
      auto formals = lam->formals();
      auto res = ctx.restriction();
      if (auto var_t = is<VariantType>(lam->body()))
         for (auto c: var_t->constructors())
            quantify_and_emit_constructor(ctx, *c, formals, res);
      else
         emit_parametric_consorts(ctx, ctor, formals, lam->body(), res);
   }

   static void
   emit_associated_ops(FormContext& ctx, const Declaration& decl) {
      if (has_type_denotation({ctx->get_typename(), decl.value().type()},ctx))
         if (auto ctor = is<Constructor>(decl.value()))
            emit_associated_ops(ctx, *ctor);
   }

   // FIXME: Abstraction missing. Why isn't it obvious that only variants should
   //        be considered?
   static void
   emit_associated_ops(BasicContext& ctx, Elaboration init) {
      if (auto gen_t = is<GenerativeType>(init))
         if (auto var_t = is<VariantType>(gen_t->value()))
            for (auto c: var_t->constructors())
               quantify_and_emit_constructor(ctx, *c, { }, { });
   }

   // Return a fresh name for a sytnthetized parameter of a given
   // type at a given position.
   static const Formal*
   fresh_formal(BasicContext& ctx, int i, TypeElaboration t) {
      return ctx->build_formal(0, i, t, { ctx->fresh_name(), t });
   }

   static Elaboration
   finish_definition(FormContext& ctx, Declaration* decl, Elaboration init) {
      auto type = decl->value().type();
      auto name = decl->name();
      if (not satisfies(ctx, init, type))
         semantics_error(ctx, "invalid initializer in definition of "
                         + quote(name));
      decl->value().code(init.code());
      emit_associated_ops(ctx, *decl);
      // The result is an lvalue.
      const Type* t = make_reference_type(ctx, type);
      return { t, ctx->build_bind(name,type,init,ctx.get_specializations()) };
   }

   static Elaboration
   finish_definition(BasicContext& ctx, Declaration* decl, Elaboration init) {
      auto type = decl->value().type();
      auto name = decl->name();
      if (not satisfies(ctx, init, type))
         semantics_error(ctx, "invalid initializer in definition of "
                         + quote(name));
      decl->value().code(init.code());
      emit_associated_ops(ctx, init);
      // The result is an lvalue.
      const Type* t = make_reference_type(ctx, type);
      return { t, ctx->build_bind(name,type,init,ctx.get_specializations()) };
   }

   static const Formal*
   elaborate_parameter(BasicContext&, const ParameterAst*, int);

   static const ParameterAst*
   ensure_unique_parameter(BasicContext& ctx, const ParameterAst* parm,
                           const Parameters& parms, int pos) {
      if (auto name = parm->name())
         for (int i = 0; i < pos; ++i)
            if (lexeme(*parms[i]->name()->token()) == lexeme(*name->token()))
               semantics_error(ctx, "parameters at same level must have "
                               "distinct names", name);
      return parm;
   }

   static Formals
   elaborate_parameters(BasicContext& ctx, const Parameters& parms) {
      Formals formals(parms.size());
      for (std::size_t i = 0; i < parms.size(); ++i) {
         auto parm = ensure_unique_parameter(ctx, parms[i], parms, i);
         formals[i] = elaborate_parameter(ctx, parm, i);
      }
      return formals;
   }

   namespace {
      // When elaborating a function body, we want a return value
      // to satisfy the constant the function's return type.
      struct ReturnTypeManager {
         BasicContext& ctx;
         ReturnTypeManager(BasicContext& c, const Type* t)
               : ctx(c) {
            ctx->push_return_type(t);
         }
         ~ReturnTypeManager() {
            ctx->pop_return_type();
         }
      };
   }
   
   static Elaboration
   elaborate_block(BasicContext& ctx, const CompoundAst& stmt) {
      const std::size_t size = length(stmt.sequence());
      LocalScopeManager local_scope { ctx.elaborator() };
      std::vector<Elaboration> stmts(size);
       for (std::size_t i = 0; i < size; ++i)
          stmts[i] = ctx->elaborate(stmt.sequence()[i]);
      return { ctx->get_void(), ctx->build_block(stmts) };
   }

   static void
   load_assumptions_on_parameter(BasicContext&, const Formal*) {
      // FIXME: build concept constraints.
   }

   static const Formal*
   elaborate_parameter(BasicContext& ctx, const ParameterAst* parm, int pos) {
      auto type = elaborate_type(ctx, parm->type());
      const int level = ctx->get_parameter_depth();
      auto name = make_identifier(ctx, parm->name());
      LinkName lnk { name, type };
      const Formal* formal = ctx->build_formal(pos, level, type, lnk);
      load_assumptions_on_parameter(ctx, formal);
      if (name != nullptr)
         ctx->current_env()->bind(name, make_elaboration(formal));
      return formal;
   }

   static TypeElaboration
   elaborate_target_type(BasicContext& ctx, const Ast* x) {
      auto t = elaborate_type(ctx, x);
      if (auto tt = is<TagType>(t.code()))
         return { t.type(), tt->type() };
      return t;
   }

   static TypeElaboration
   do_elaborate_arrow_type(BasicContext& ctx, const LambdaAst& x) {
      InputTypes source = elaborate_parameters(ctx, x.parameters());
      auto target = elaborate_target_type(ctx, x.body());
      return type_elaboration(ctx, ctx->make_arrow_type(target, source));
   }

   static TypeElaboration
   do_elaborate_quantified_type(BasicContext& ctx, const QuantifiedAst& x) {
      const Quantifier quant = elaborate_quantifier(x);
      Formals formals = elaborate_parameters(ctx, x.parameters());
      auto target = elaborate_type(ctx, x.body());
      return type_elaboration
         (ctx, ctx->make_quantified_type(quant, formals, target, { }));
   }

   // Return true if the entity designated by `e' has generative instances.
   static bool
   has_generative_instances(Elaboration e, BasicContext& ctx) {
      if (has_type_denotation(e, ctx))
         return true;
      return are_equivalent(e.type(), ctx->get_concept(), ctx)
         or are_equivalent(e.type(), ctx->get_namespace(), ctx);
   }

   // Construct the mapping object of a function definition.
   static const Expression*
   build_mapping(FormContext& ctx, Elaboration body) {
      LinkName lnk { ctx.name(), ctx.type() };
      auto lam = ctx->build_lambda(lnk, ctx.formals(), body);
      if (has_generative_instances(body, ctx))
         return ctx->build_constructor(lnk, { lam->type(), lam });
      return lam;
   }

   static Elaboration
   elaborate_definiens(FormContext& ctx, const Ast* body) {
      auto type = ctx.type();
      const Type* target = ctx.target_type();
      ReturnTypeManager push_type { ctx, target };
      Elaboration expr = elaborate_initializer(ctx, target, body);
      auto map = build_mapping(ctx, expr);
      return { type, map };
   }

   static void
   check_if_redeclaration(DeclContext& ctx) {
      auto name = ctx.name();
      auto type = ctx.type();
      if (auto decl = ctx.scope()->select(name, type)) {
         // It makes little sense to redeclare a builtin function.
         auto user = is<Lambda>(decl->value());
         if (user == nullptr)
            semantics_error(ctx, "redeclaration of a builtin function");

         // We certainly don't want to divine what to do with
         // multitple definitions.
         else if (user->body().code() != nullptr)
            semantics_error(ctx, "multiple definition of " + quote(name));
      }
   }

   static Formals
   elaborate_parameters(BasicContext& ctx, const CallForm& head) {
      return elaborate_parameters(ctx, head.parameters());
   }
   
   static Formals
   elaborate_parameters(BasicContext& ctx, const ClosedForm& head) {
      return elaborate_parameters(ctx, head.parameters());
   }
   
   static Formals
   elaborate_parameters(BasicContext& ctx, const InfixForm& head) {
      if (head.lhs()->name() != nullptr
          and lexeme(*head.lhs()->name()->token())
          == lexeme(*head.rhs()->name()->token()))
         semantics_error(ctx, "repeated parameter name in definition",
                         head.rhs());
      Formals formals(2);
      formals[0] = elaborate_parameter(ctx, head.lhs(), 0);
      formals[1] = elaborate_parameter(ctx, head.rhs(), 0);
      return formals;
   }

   static Formals
   elaborate_parameters(BasicContext& ctx, const PrefixForm& head) {
      return Formals{ elaborate_parameter(ctx, head.parameter(), 0) };
   }

   static Formals
   elaborate_parameters(BasicContext& ctx, const SuffixForm& head) {
      return Formals{ elaborate_parameter(ctx, head.parameter(), 0) };
   }

   static const Lambda*
   make_coerce_function(BasicContext& ctx, const Name* name,
                        const Type* s, const Type* t) {
      TypeElaboration target = { ctx->get_typename(), t };
      InputTypes source = { { ctx->get_typename(), s} };
      auto ft = ctx->make_arrow_type(target, source);
      auto parm = fresh_formal(ctx, 0, source.front());
      return ctx->build_lambda({ name, ft }, Formals{ parm }, read(ctx, parm));
   }

   // Define a canonical coercion named `n' of type `s -> t' in
   // `scope'.
   static void
   define_coercion(BasicContext& ctx, ScopeRef scope,
                   const char* n, const Type* s, const Type* t) {
      auto name = make_identifier(ctx.elaborator(), n);
      auto fun = make_coerce_function(ctx, name, s, t);
      scope->define(name, fun->type(), fun);
   }

   // FIXME: Screaming for a FormContext
   static void
   define_quantified_coercion(BasicContext& ctx, ScopeRef scope, const char* n,
                              const Formals& fs, Elaboration c, const Type* s,
                              const Type* t)
   {
      auto name = make_identifier(ctx.elaborator(), n);
      auto f = make_coerce_function(ctx, name, s, t);
      auto qt = ctx->make_quantified_type(Quantifier::forall, fs,
                                    { ctx->get_typename(), f->type() }, c);
      auto qf = ctx->build_lambda({ name, qt }, fs, { f->type(), f });
      scope->define(name, qf->type(), qf);
   }

   // Ensure that the definition of the form `x' does not introduce
   // an overload for a type value.
   template<typename X>
   static const Name*
   get_name_and_ensure_uniqueness_if_necessary(BasicContext& ctx, const X& x) {
      auto name = get_name(ctx, x);
      ensure_unique_name(ctx, name);
      return name;
   }

   namespace {
      struct SimpleDeclContext : Elaborator::DeclContext {
         template<typename X>
         SimpleDeclContext(BasicContext& ctx, const X& x, const Ast* t)
               : DeclContext(ctx, get_name_and_ensure_uniqueness_if_necessary(ctx, x)),
                 ty(nullptr)
         { ty = elaborate_type(ctx, t, filter::nothing); }
         const Type* type() const override { return ty; }
      private:
         const Type* ty;
         SimpleDeclContext(SimpleDeclContext&&) = delete;
      };
   }

   // Build a generative type definition named `name' with `rhs' for
   // the right hand side of the definition.  In the process, make
   // available the canonical value isomorphism
   //    per : rhs -> name     and rep : name -> rhs
   static const GenerativeType*
   simple_generative_type_and_consort(SimpleDeclContext& ctx,
                               const Name* name, const Type* rhs) {
      auto rhs_t = type_elaboration(ctx, rhs);
      auto t = ctx->make_generative_type(name, rhs_t, ctx.scope().base());
      define_coercion(ctx, ctx.scope(), "per", rhs, t);
      define_coercion(ctx, ctx.scope(), "rep", t, rhs);
      return t;
   }

   // Elaborate the definiens of a simple definition.
   static Elaboration
   elaborate_definiens(SimpleDeclContext& ctx, const Ast* body) {
      auto init = elaborate_initializer(ctx, ctx.type(), body);
      // Type definitions give rise to generative types.
      if (auto var_t = is<VariantType>(init.code())) {
         auto t = ctx->make_generative_type
            (ctx.name(), type_elaboration(ctx, var_t), ctx.scope().base());
         init = type_elaboration(ctx, t);
      }
      else if (has_type_denotation(init, ctx)) {
         auto t = simple_generative_type_and_consort
            (ctx, ctx.name(), is<Type>(init.code()));
         init = { init.type(), t };
      }
      return init;
   }

   template<template<typename> class T, typename C>
   static Elaboration
   elaborate_definition(C& ctx, const DefinitionAst& def) {
      struct V : FixityForm::Visitor {
         C& ctx;
         const DefinitionAst& def;
         Elaboration result;
         V(C& c, const DefinitionAst& d) : ctx(c), def(d) { }

         void visit(const LiteralForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const AlphabethicForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const OperatorForm& x) {
            T<SimpleDeclContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const PrefixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const SuffixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const InfixForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const ClosedForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
         void visit(const CallForm& x) {
            T<FormContext> new_ctx { ctx, x, def.type() };
            result = do_elaborate_definition(new_ctx, def);
         }
      };

      V v { ctx, def };
      def.form()->accept(v);
      return v.result;
   }

   namespace {
      struct NameTypePair {
         std::pair<const Name*, const Type*> rep;
         const Name* name() const { return rep.first; }
         const Type* type() const { return rep.second; }
      };
   }

   static NameTypePair
   get_name_type(BasicContext& ctx, const FixityForm* f, const Ast* t) {
      struct V : FixityForm::Visitor {
         BasicContext& ctx;
         const Ast* tsyn;
         NameTypePair result;
         V(BasicContext& c, const Ast* t) : ctx(c), tsyn(t), result() { }
         void visit(const LiteralForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const AlphabethicForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const OperatorForm& x) {
            SimpleDeclContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const PrefixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const SuffixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const InfixForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const ClosedForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
         void visit(const CallForm& x) {
            FormContext new_ctx { ctx, x, tsyn };
            result.rep = { new_ctx.name(), new_ctx.type() };
         }
      };
      V v { ctx, t };
      f->accept(v);
      return v.result;
   }

   namespace {
      struct formal_name_eq {
         const Symbol name;
         formal_name_eq(Symbol n) : name(n) { }
         bool operator()(const Formal* f) const {
            return f->symbol() == name;
         }
      };
   }

   static void
   assume_existential(BasicContext& ctx, const Formals& fs, Elaboration e) {
      // Assume formals
      for(auto formal: fs)
         declare(ctx->current_env(), formal->link_name(), formal);
      if (auto b = is<Block>(e.code())) {
         for (auto formula: b->statements())
            assume_property(ctx, formula);
      } else
         assume_property(ctx, e);
   }

   static void
   assume_formula(BasicContext& ctx, const Formula& f) {
      // FIXME: Check the type for bool.
      if (f.quantifier() == Quantifier::exists)
         assume_existential(ctx, f.parameters(), f.body());
      // Universally quantified things?
   }

   static void
   assume_alias(BasicContext& ctx, const Alias& a) {
      declare(ctx->current_env(), a.alias().link_name(), &a.alias());
      load_assumption(ctx, a.equality());
   }

   static void assume_property(BasicContext& ctx, Elaboration expr) {
      const Expression* code = expr.code();
      if (auto req = is<Constraint>(code))
         load_requirements(ctx, req);
      else if (auto sig = is<Signature>(code)) {
         auto decl = declare(ctx->current_env(), sig->link_name());
         decl->value().code(sig);       // Give it an abstract value
      } else if (auto formulae = is<Formula>(code)) {
         assume_formula(ctx, *formulae);
      } else if (auto alias = is<Alias>(code)) {
         assume_alias(ctx, *alias);
      } else
         load_assumption(ctx, expr);
   }
   
   static Elaboration
   elaborate_logical(BasicContext& ctx, logical::Operation op,
                     const BinaryAst& ast) {
      try {
         Elaboration lhs = ctx->elaborate(ast.lhs(), ctx->get_bool());
         Elaboration rhs = ctx->elaborate(ast.rhs(), ctx->get_bool());
         return { ctx->get_bool(), ctx->build_logical(op, lhs, rhs) };
      } catch(...) { }
      Elaboration lhs = ctx->elaborate(ast.lhs(), ctx->get_concept());
      Elaboration rhs = ctx->elaborate(ast.rhs(), ctx->get_concept());
      return { ctx->get_concept(), ctx->build_logical(op, lhs, rhs) };
   }

   // -- QuantContext --
   namespace {
      struct QuantContext : BasicContext {
         QuantContext(BasicContext& ctx, const QuantifiedAst& x)
               : BasicContext(ctx),
                 quant(elaborate_quantifier(x)),
                 parms_scope(ctx.elaborator()),
                 parms(elaborate_parameters(ctx, x.parameters())),
                 guard(elaborate_guard(ctx, x.constraint()))
         { assume_property(ctx, guard); }
         Quantifier quantifier() const { return quant; }
         const Formals& formals() const { return parms; }
         Elaboration constraint() const { return guard; }
      private:
         const Quantifier quant;
         ParameterScopeManager parms_scope;
         Formals parms;
         Elaboration guard;
      };
   }

   static const QuantifiedType*
   make_quantified_type(QuantContext& ctx, TypeElaboration t) {
      return ctx->make_quantified_type(ctx.quantifier(), ctx.formals(), t,
                                       ctx.constraint());
   }

   static const QuantifiedType*
   make_quantified_type(QuantContext& ctx, const Type* t) {
      return make_quantified_type(ctx, type_elaboration(ctx, t));
   }

   namespace {
      template<typename C>
      struct QDefContext : Elaborator::DeclContext {
         template<typename F>
         QDefContext(QuantContext& qc, const F& f, const Ast* t)
               : DeclContext(qc, get_name_and_ensure_uniqueness_if_necessary(qc, f)),
                 qctx(qc),
                 ctx(qc, f, t),
                 ty(make_quantified_type(qc, ctx.type()))
         { }

         const QuantifiedType* type() const override { return ty; }
         QuantContext& quantifying_context() { return qctx; }
         C& quantified_context() { return ctx; }
                 
      private:
         QuantContext& qctx;
         C ctx;
         const QuantifiedType* ty;
      };
   }

   template<typename C>
   static Elaboration
   elaborate_definiens(QDefContext<C>& ctx, const Ast* body) {
      auto expr = elaborate_definiens(ctx.quantified_context(), body);
      auto& qctx = ctx.quantifying_context();
      const LinkName lnk { ctx.name(), ctx.type() };
      auto lam = ctx->build_lambda(lnk, qctx.formals(), expr);
      return { ctx.type(), lam };
   }

   static Elaboration
   elaborate_quantified(BasicContext& ctx, const QuantifiedAst& ast) {
      QuantContext new_ctx { ctx, ast };
      // 1. elaborate body
      Elaboration body;
      if (auto seq = is<SequenceAst>(ast.body())) {
         const int n = length(seq);
         std::vector<Elaboration> exprs(n);
         for (int i = 0; i < n; ++i)
            exprs[i] = new_ctx->elaborate(seq->at(i));
         body = { new_ctx->get_void(), new_ctx->build_block(exprs) };
      }
      else if (auto x = is<DefinitionAst>(ast.body()))
         return elaborate_definition<QDefContext>(new_ctx, *x);
      else {
         const Type* body_type = is<QuantifiedAst>(ast.body())
            ? new_ctx->get_bool().code()
            : nullptr;
         body = new_ctx->elaborate(ast.body(), body_type);
      }
      // 2. Now build the formula.
      if (auto x = is<Type>(body)) {
         TypeElaboration t = { body.type(), x };
         auto type = make_quantified_type(new_ctx, t);
         return type_elaboration(new_ctx, type);
      }
      const Formula* formula = new_ctx->build_formula(new_ctx.quantifier(), new_ctx.formals(), body);
      auto type = make_quantified_type(new_ctx, body.type());
      return { type, formula };
   }

   static Elaboration
   elaborate_conditional(BasicContext& ctx, const IfAst& s, const Type* t) {
      // 0. Set up a new scope for a possible declaration in the condition.
      LocalScopeManager local_scope { ctx.elaborator() };
      // 1. Elaborate the condition.
      Elaboration cond = ctx->elaborate(s.condition(), ctx->get_bool());
      // 2. Then the consequence.
      Elaboration conseq;
      {
         LocalScopeManager local_scope { ctx.elaborator() };
         assume_property(ctx, cond);
         conseq = ctx->elaborate(s.consequence(), t);
      }
      // 3. Finally, the alternative.
      Elaboration alt = ctx->elaborate(s.alternate(), t);
      return { ctx->get_void(), ctx->build_if(cond, conseq, alt) };
   }

   static Elaboration
   elaboration_error(BasicContext& ctx, const Ast* ast) {
      semantics_error(ctx, "cannot elaborate this expression", ast);
      return { };
   }

   // Interpret literal as an uint and coerce to target.
   static Elaboration
   as_literal_uint(BasicContext& ctx, const Token* x, const Type* target) {
      auto uint_t = is<UintType>(target);
      if (uint_t == nullptr)
         return { };
      uintmax_t v = 0;
      for (auto c : *x)
         v = 10 * v +  (c - '0');
      return { uint_t, ctx->build_uint(v, uint_t) };
   }

   static Elaboration
   literal_integer(BasicContext& ctx, const Token* x, const Type* target) {
      int v = 0;
      for (auto c : *x)
         v = 10 * v +  (c - '0');
      auto val = ctx->build_int(v, ctx->get_int());
      if (auto e = successful_coercion(ctx, { val->type(), val }, target))
         return e;
      else if (auto e = as_literal_uint(ctx, x, target))
         return e;
      semantics_error(ctx, "cannot interpret literal " + quote(x)
                      + " as a value of type " + quote(show(*target)), *x);
      return { };
   }

   static Elaboration
   elaborate_hex(BasicContext& ctx, const Token& token, const Type* t) {
      auto hex_str = lexeme(token);
      auto width = 4 * (hex_str.size() - 2);
      if (ctx->get_int().code() == t)
         return { t, ctx->build_int(std::strtol(hex_str.c_str(), 0, 16), t) };
      uintmax_t v = std::strtoul(hex_str.c_str(), 0, 16);
      t = ctx->make_uint_type({ ctx->get_int(),
                                ctx->build_int(width, ctx->get_int()) });
      return { t, ctx->build_uint(v, t) };
   }

   static Elaboration
   elaborate_string(BasicContext& ctx, const Token* token) {
      Symbol s = ctx->intern(token);
      const Type* t = ctx->get_string();
      return { t, ctx->build_string(s, t) };
   }

   // Elaborate a character constant.
   static Elaboration
   elaborate_character(BasicContext& ctx, const Token* token) {
      const std::size_t size = length(*token);
      if (size == 0)
         semantics_error(ctx, "missing character in character constant");
      else if (size > 1)
         semantics_error(ctx, "too many characters in character constant");
      const Type* t = ctx->get_char();
      return { t, ctx->build_char(Character(*begin(*token)), t) };
   }

   static Fiber
   literal_fiber(BasicContext& ctx, const Token* x, const Type* t) {
      Fiber f = lexical_fiber(ctx, literal_name(ctx, x));
      return fiber_section(ctx, f, t);
   }

   static Elaboration
   elaborate_integer(BasicContext& ctx, const LiteralAst& x, const Type* type) {
      Fiber f = literal_fiber(ctx, x.token(), type);
      if (f.empty())
         return literal_integer(ctx, x.token(), type);
      else if (f.size() > 1)
         semantics_error(ctx, "use of literal " + quote(&x)
                         + " is ambiguous in this context", &x);
      return f.front();
   }

   static bool is_not_digit(char c) { return not std::isdigit(c); }

   static std::pair<uintmax_t, std::string::const_iterator>
   get_next_ipv4_byte(std::string::const_iterator f,
                      std::string::const_iterator l)
   {
      auto start = f;
      f = std::find_if(f, l, is_not_digit);
      std::string digits(start, f);
      ++f;
      return { std::stoul(digits), f };
   }

   static Elaboration
   elaborate_ipv4(BasicContext& ctx, const Token& x) {
      auto str = lexeme(x);
      std::pair<uintmax_t, std::string::const_iterator> y;
      uintmax_t val = 0;
      y = get_next_ipv4_byte(str.begin() + 5, str.end());
      val += y.first << (3 * 8);
      y = get_next_ipv4_byte(y.second, str.end());
      val += y.first << (2 * 8);
      y = get_next_ipv4_byte(y.second, str.end());
      val += y.first << 8;
      y = get_next_ipv4_byte(y.second, str.end());
      val += y.first;
      auto t = ctx->make_uint_type({ ctx->get_int(),
                                     ctx->build_int(32, ctx->get_int()) });
      return { t, ctx->build_uint(val, t) };
   }

   static Elaboration
   elaborate_double(BasicContext& ctx, const Token* x, const Type* t) {
      std::istringstream is(lexeme(*x));
      double v;
      is >> v;
      if (!is)
         semantics_error(ctx, "invalid floating point literal", *x);
      auto cst = ctx->build_double(v, ctx->get_double());
      return ctx->coerce({ cst->type(), cst}, t);
   }

   static Elaboration
   elaborate_literal(BasicContext& ctx, const LiteralAst& x, const Type* t) {
      switch (x.token()->kind) {
      default:
         return elaboration_error(ctx, &x);

      case token::literal_real_tok:
         return elaborate_double(ctx, x.token(), t);

      case token::literal_boolean_tok:
         return ctx->coerce
            (ctx->get_binding
             (ctx->build_identifier(x.token())).select(ctx->get_bool())->value(), 
             t);

      case token::literal_character_tok:
         return ctx->coerce(elaborate_character(ctx, x.token()), t);
         
      case token::literal_string_tok:
         return ctx->coerce(elaborate_string(ctx, x.token()), t);
         
      case token::literal_integer_tok:
         return elaborate_integer(ctx, x, t);

      case token::literal_ipv4_tok:
         return elaborate_ipv4(ctx, *x.token());

      case token::literal_hex_tok:
         return elaborate_hex(ctx, *x.token(), t);
      }
   }

   // Elaborate brace-enclosed initializer lists.
   static Elaboration
   elaborate_brace_list(BasicContext& ctx, const SequenceAst& x, const Type* t) {
      if (length(x.sequence()) != 1)
         semantics_error(ctx, "non-singleton initializer lists are "
                         "not supported yet.");
      return ctx->elaborate(x.sequence().at(0), t);
   }

   struct iterator_space {
      std::vector<Elaboration> conditions;
   };

   static void
   union_iterator_space(iterator_space& i, const iterator_space& j) {
      for (auto condition: j.conditions)
         i.conditions.push_back(condition);
   }

   static Elaboration
   make_terminating_condition(BasicContext& ctx, const iterator_space& i) {
      // axiom i.conditions.size() > 0
      auto& conditions = i.conditions;
      Elaboration condition = conditions[0];
      for (std::size_t n = 1; n != conditions.size(); ++n)
         condition = {
            ctx->get_bool(), 
            ctx->build_logical(logical::conjunction, condition, conditions[n])
         };
      return condition;
   }

   static iterator_space
   elaborate_while(BasicContext& ctx, const WhileIterator& x) {
      auto condition = ctx->elaborate(x.condition(), ctx->get_bool());
      iterator_space i;
      i.conditions.push_back(condition);
      return i;
   }

   static iterator_space
   elaborate_iterator(BasicContext& ctx, const Iterator& iter) {
      struct V : Iterator::Visitor {
         BasicContext& ctx;
         iterator_space space;
         V (BasicContext ctx) : ctx(ctx), space() {}
         void visit(const ForIterator&) {
            semantics_error(ctx, "`for` iterator not yet supported"); }
         void visit(const WhileIterator& x) {
            space = elaborate_while(ctx, x); }
         void visit(const UntilIterator&) {
            semantics_error(ctx, "`until` iterator not yet supported"); }
         void visit(const ProvisoIterator&) {
            semantics_error(ctx, "`proviso` iterator not yet supported"); }
      };
      V v(ctx);
      iter.accept(v);
      return v.space;
   }

   static iterator_space
   elaborate_iterators(BasicContext& ctx, const Sequence<Iterator>& iters) {
      iterator_space iter_space;
      for (auto iter: iters)
         union_iterator_space(iter_space, elaborate_iterator(ctx, *iter));
      return iter_space;
   }

   static Elaboration
   no_op(BasicContext& ctx) { return { ctx->get_void(), nullptr }; }

   static Elaboration
   guard_loop_body(BasicContext& ctx, Elaboration body, const iterator_space& i) {
      if (i.conditions.empty())
         return body;
      auto terminal_condition = make_terminating_condition(ctx, i);
      Elaboration no_op_leave { ctx->get_void(), ctx->build_leave(no_op(ctx)) };
      auto guarded = ctx->build_if(terminal_condition, body, no_op_leave);
      return { ctx->get_void(), guarded };
   }

   static Elaboration
   synthesize_loop(BasicContext& ctx, Elaboration body,
                   const iterator_space& i)
   {
      auto guarded = guard_loop_body(ctx, body, i);
      return { ctx->get_void(), ctx->build_loop(guarded) };
   }

   static Elaboration
   elaborate_repeat(BasicContext& ctx, const RepeatAst& x) {
      LocalScopeManager local_scope(ctx.elaborator());
      auto iters = elaborate_iterators(ctx, x.iterators());
      auto body = ctx->elaborate(x.body());
      return synthesize_loop(ctx, body, iters);
   }

   template<typename X>
   static Elaboration
   elaborate_name(BasicContext& ctx, const X& x, const Type* t) {
      Fiber f = lexical_fiber_or_else(ctx, x);
      Fiber result = fiber_section(ctx, f, t);
      if (result.empty())
         semantics_error(ctx, "no valid interpretation of this name in"
                         " a context requiring expression of type "
                         + quote(show(*t)), &x);
      else if (not result.singleton())
         semantics_error(ctx, "use of " + quote(&x) + " is ambiguous");
      return result[0];
   }

   static Elaboration
   elaborate_dot(BasicContext& ctx, const DotAst& x, const Type* t) {
      Fiber f = fiber_section(ctx, semantics_fiber(ctx, x), t);
      if (f.empty())
         semantics_error(ctx, "selection expression is cannot be coerced to "
                         + quote(show(*t)));
      else if (not f.singleton())
         semantics_error(ctx, "ambiguous expression in context of "
                         + quote(show(*t)) + " expression");
      return f[0];
   }

   static Elaboration
   elaborate_return(BasicContext& ctx, const ReturnAst& x) {
      if (ctx->frame_depth() < 1)
         semantics_error(ctx, "return statement cannot appear "
                         "outside function definitions");
      Elaboration expr = ctx->elaborate
         (x.expression(), ctx->get_return_type());
      return { expr.type(), ctx->build_return(expr) };
   }

   static Elaboration
   elaborate_throw(BasicContext& ctx, const ThrowAst& x, const Type* t) {
      Elaboration expr = ctx->elaborate(x.expression());
      // FIXME: the elaboration type match anything.
      return { t, ctx->build_throw(expr) };
   }

   // Elaborate an assignment expression.
   // FIXME: restricted to variables only.
   static Elaboration
   elaborate_assignment(BasicContext& ctx, const AssignmentAst& x) {
      auto id = is<IdentifierAst>(x.lhs());
      if (id == nullptr)
         ctx->sorry("variable assignment are currently supported");
      auto lhs = elaborate_name(ctx, *id, nullptr);
      auto t = is<ReferenceType>(lhs.type());
      if (t == nullptr or is<ReadonlyType>(t->referee()))
         semantics_error(ctx, "modifiable reference required on "
                         "left hand side of assignment", x.lhs());
      while (t) {
         try {
            auto rhs = ctx->elaborate(x.rhs(), t->referee());
            return { t, ctx->build_write(lhs, rhs) };
         } catch(CoercionError&) {}
         lhs = rvalue(ctx, lhs);
         t = is<ReferenceType>(lhs.type());
      }
      semantics_error(ctx, "assignee is not a lvalue of the initializer");
      return { };
   }

   // -- Elaborate an input source file
   static LoadUnit*
   load_source_file(BasicContext& ctx, const SourceFileAst& x) {
      LoadUnit* u = ctx->build_load_unit(x.path);
      for (auto y : x.asts)
         u->add_stmt(ctx->elaborate(y));
      return u;
   }
   
   static Elaboration
   elaborate_source_file(BasicContext& ctx, const SourceFileAst& x) {
      return { ctx->get_LoadUnit(), load_source_file(ctx, x) };
   }

   static Elaboration
   elaborate_binary(BasicContext& ctx, const BinaryAst& x, const Type* t) {
      Elaborator::LocationManager push_loc(ctx.elaborator(), *anchor(x.operation()));
      switch (x.operation()->token()->kind) {
      default:
         return elaborate_call(ctx, x.operation(), { x.lhs(), x.rhs() }, t);
         
      case token::plus_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_plus, t);
         
      case token::minus_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_dash, t);

      case token::star_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_star, t);

      case token::slash_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_slash, t);
         
      case token::div_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_div, t);
         
      case token::mod_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_mod, t);
         
      case token::rem_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_rem, t);
         
      case token::greater_tok: 
         return elaborate_infix(ctx, x, &Elaborator::build_rangle, t);
         
      case token::greater_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_rangleq, t);
         
      case token::less_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_langle, t);

      case token::less_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_langleq, t);
         
      case token::double_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_eqeq, t);
         
      case token::not_equal_tok:
         return elaborate_infix(ctx, x, &Elaborator::build_excleq, t);

      case token::equal_tok: 
         return ctx->coerce(elaborate_assignment(ctx, x.lhs(), x.rhs()), t);
         
      case token::and_tok:
         return ctx->coerce(elaborate_logical(ctx, logical::conjunction, x), t);
         
      case token::or_tok:
         return ctx->coerce(elaborate_logical(ctx, logical::disjunction, x), t);
         
      case token::implies_tok:
         return ctx->coerce(elaborate_logical(ctx, logical::implication, x), t);
         
      case token::equiv_tok:
         return ctx->coerce(elaborate_logical(ctx, logical::equivalence, x), t);
      }
   }

   // -- Elaborate an expression statement.
   static Elaboration
   elaborate_expr_stmt(BasicContext& ctx, const ExprStmtAst& x) {
      Elaboration z = ctx->elaborate(x.expression());
      if (z.type() == nullptr)
         semantics_error(ctx, "expression cannot type check");
      return z;
   }

   // If `x' refers to a string literal AST object, returns a pointer
   // to that object; otherwise return null.
   static const LiteralAst*
   is_string_literal(const Ast* x) {
      if (auto s = is<LiteralAst>(x))
         if (s->token()->kind == token::literal_string_tok)
            return s;
      return nullptr;
   }

   // -- LoadUnit imports.
   static const SourceFileAst*
   read_file(BasicContext& ctx, const Path& p) {
      return ctx->reader()->read_file(p, ctx->current_flags());
   }
   
   static std::string
   resolve_load_path(BasicContext& ctx, const Ast* x, std::string d = "") {
      if (auto id = is<IdentifierAst>(x))
         return d + lexeme(*id->token());
      else if (auto s = is_string_literal(x))
         return lexeme(*s->token());
      else if (auto p = is<PathAst>(x))
         return resolve_load_path
            (ctx, p->subpath(), d + lexeme(*p->dirname()->token()) + "/");
      semantics_error(ctx, "invalid module name: " + quote(show(x)));
      return d;
   }

   static Elaboration
   elaborate_import(BasicContext& ctx, const ImportAst& x) {
      auto load_path = ctx->expand_library_path(
                          resolve_load_path(ctx, x.path()));
      // Don't look for load units that have been seen
      if (ctx->have_seen_path(load_path))
         return { ctx->get_void(), ctx->get_void() };
      ctx->mark_path_as_seen(load_path);
      auto src = read_file(ctx, load_path);
      auto unit = load_source_file(ctx, *src);
      return { ctx->get_LoadUnit(), ctx->build_import(unit) };
   }

   static Elaboration
   elaborate_restrict(BasicContext& ctx, const RestrictAst& x) {
      const Type* type = elaborate_type(ctx, x.type());
      return ctx->elaborate(x.expression(), type);
   }

   static Elaboration
   elaborate_rule(BasicContext& ctx, const RuleAst& x) {
      const Type* type = elaborate_type(ctx, x.type());
      auto id = get_name(ctx, x.form());
      LinkName lnk { id, type };
      auto decl = declare(ctx->current_env(), lnk);
      Elaboration e = ctx->elaborate(x.initializer(), type);
      return finish_definition
         (ctx, decl, { type, ctx->build_macro(lnk, e) });
   }

   // -- PatternMatchContext --
   namespace {
      struct PatternMatchContext : BasicContext {
         PatternMatchContext(BasicContext& ctx, const VariantType& var_t,
                             const Expression* subj, const Type* t)
            : BasicContext(ctx), var_t(var_t), subj(subj), seen_ctors(),
              result_type(t)
         {
            for (auto ctor: var_t.constructors())
               seen_ctors[ctor] = false;
         }
         const Constructor* register_ctor(const std::string& name) {
            auto ctor = get_constructor(name);
            if (seen_ctors[ctor])
               semantics_error(basic_ctx(), "overlapping case for " + name);
            seen_ctors[ctor] = true;
            return ctor;
         }
         void ensure_all_ctors_used() {
            if (not all_used())
              semantics_error(basic_ctx(), "missing cases:\n"
                                         + show_missing_ctors());
         }
         const VariantType& get_variant() const { return var_t; }
         Elaboration scrutinee() const { return { &var_t, subj }; }
      private:
         BasicContext& basic_ctx() { return *this; }
         const Constructor* get_constructor(const std::string& name) {
            for (auto ctor: var_t.constructors())
               if (name == ctor->name()->symbol().string())
                  return ctor;
            semantics_error(basic_ctx(), "identifier " + quote(name)
               + " is not a constructor of " + show_type(&var_t));
            return nullptr;
         }
         bool all_used() const {
            for (auto i: seen_ctors)
               if (i.second == false)
                  return false;
            return true;
         }
         std::string show_missing_ctors() const {
            std::stringstream ss;
            for (auto i: seen_ctors)
               if (i.second == false)
                  ss << "  " << pretty(i.first) << ": "
                     << pretty(i.first->type()) << '\n';
            return ss.str();
         }
         const VariantType& var_t;
         const Expression* subj;
         std::map<const Constructor*,bool> seen_ctors;
      public:
         const Type* result_type;
      };
   }

   static Elaboration
   elaborate_name_pattern(PatternMatchContext& ctx, const IdentifierAst& x) {
      auto name = lexeme(*x.token());
      if (auto ctor = ctx.register_ctor(name))
         return { ctor->type(), ctor };
      return { };
   }

   static const Formal*
   elaborate_pattern_param(PatternMatchContext& ctx, const IdentifierAst& id,
                           TypeElaboration t, int pos)
   {
      auto name = make_identifier(ctx, &id);
      const int level = ctx->get_parameter_depth();
      LinkName lnk { name, t.code() };
      declare(ctx->current_env(), lnk);
      return ctx->build_formal(pos, level, t, { name, t.code() });
   }

   static Formals
   elaborate_pattern_params(PatternMatchContext& ctx, const Constructor& ctor,
                            const SequenceAst& seq)
   {
      const Sequence<Ast>& s = seq.sequence();
      if (ctor.arity() != s.size())
         semantics_error(ctx, "number of arguments doesn't match constructor "
                              "arity");
      Formals formals(s.size());
      int pos = 0;
      const auto& arrow_t = is<ArrowType>(ctor.type())->source();
      for (std::size_t i = 0; i != s.size(); ++i, ++pos) {
         if (auto id = is<IdentifierAst>(s[i]))
            formals[i] = elaborate_pattern_param(ctx, *id, arrow_t[i], pos);
         else
            semantics_error(ctx, "expected a pattern variable, but got "
                               + quote(show(s[i])));
      }
      return formals;
   }

   static Elaboration
   elaborate_app_pattern(PatternMatchContext& ctx, const ApplyAst& x) {
      // FIXME Clearly there is an unimplemented AST that describes this data
      if (auto id = is<IdentifierAst>(x.operation())) {
         if (auto encl = is<EnclosureAst>(x.arguments())) {
            auto name = lexeme(*id->token());
            if (auto ctor = ctx.register_ctor(name)) {
               if (auto seq = is<SequenceAst>(encl->expr())) {
                  auto formals = elaborate_pattern_params(ctx, *ctor, *seq);
                  return { &ctx.get_variant(),
                           ctx->build_pattern_instance(ctor, formals) };
               } else if (is<IdentifierAst>(encl->expr())) {
                  auto seq = SequenceAst(encl->expr());
                  auto formals = elaborate_pattern_params(ctx, *ctor, seq);
                  return { &ctx.get_variant(),
                           ctx->build_pattern_instance(ctor, formals) };
               }
            }
         }
      } else
         semantics_error(ctx, "caller " + quote(show(x.operation()))
                            + " is not a constructor");
      return { };
   }

   static Elaboration
   elaborate_ctor_pattern(PatternMatchContext& ctx, const Ast& x) {
      if (auto id = is<IdentifierAst>(&x)) {
         return elaborate_name_pattern(ctx, *id);
      } else if (auto app = is<ApplyAst>(&x)) {
         return elaborate_app_pattern(ctx, *app);
      }
      semantics_error(ctx,"unsupported constructor pattern " + quote(show(&x)));
      return { };
   }

   static PatternClause
   elaborate_case(PatternMatchContext& ctx, const CaseAst& c) {
      LocalScopeManager new_scope { ctx.elaborator() };
      Elaboration cond = elaborate_ctor_pattern(ctx, *c.label());
      Elaboration expr = ctx->elaborate(c.statement(), ctx.result_type);
      return { cond, expr };
   }

   static Elaboration
   elaborate_variant_match(PatternMatchContext& ctx,
                           const Sequence<CaseAst>& branches)
   {
      const std::size_t n = branches.size();
      PatternClauses clauses(n);
      for (std::size_t i = 0; i != n; ++i)
         clauses[i] = elaborate_case(ctx, *branches[i]);
      ctx.ensure_all_ctors_used();
      return { ctx.result_type,
               ctx->build_pattern_match(ctx.scrutinee(), clauses) };
   }

   static Clause
   elaborate_case(BasicContext& ctx, const CaseAst& x, const Type* t) {
      Elaboration cond = ctx->elaborate(x.label(), t);
      Elaboration expr = ctx->elaborate(x.statement());
      return { cond, expr };
   }

   static Elaboration
   elaborate_match_cases(BasicContext& ctx, Elaboration subj,
                         const Sequence<CaseAst>& branches)
   {
      const std::size_t n = branches.size();
      Clauses cls(n);
      for (std::size_t i = 0; i < n; ++i)
         cls[i] = elaborate_case(ctx, *branches[i], subj.type());
      return { ctx->get_void(), ctx->build_match(subj, cls) };
   }

   static const VariantType*
   is_variant_type(const Type* t) {
      if (auto gen_t = is<GenerativeType>(t))
         t = gen_t->value().code();
      if (auto var_t = is<VariantType>(t))
         return var_t;
      return nullptr;
   }

   static Substitution
   make_subst(BasicContext& ctx, const Formals& formals,
              const Sequence<Value>& vals)
   {
      // axiom: vals are assumed to be in reified form...
      Substitution subst;
      const std::size_t n = formals.size();
      if (n == vals.size()) {
         for (std::size_t i = 0; i != n; ++i)
            subst[formals[i]] = { formals[i]->type().code(), vals[i] };
         return subst;
      } else
         semantics_error(ctx, "cannot form substitution from formals and "
                              "values");
      return  { };
   }

   // Will return the substituted variant.
   static const VariantType*
   is_parametric_variant_type(BasicContext& ctx, const Type* t) {
      // Sweet gravy. Does this need a context?
      if (auto te = is<TypeExpression>(t))
         if (auto inst = is<Instance>(te->expr().code()))
            if (auto ctor = is<Constructor>(inst->constructor()))
               if (auto lam = is<Lambda>(ctor->implementation().code()))
                  if (auto var_t = is<VariantType>(lam->body().code())) {
                     auto subst = make_subst(ctx, lam->formals(),
                                             inst->arguments());
                     Elaboration e = subst_expr(
                                        ctx.elaborator(),
                                        { ctx->get_typename(), var_t },
                                        subst);
                     return is<VariantType>(e.code());
                  }
      return nullptr;
   }

   static Elaboration
   elaborate_match(BasicContext& ctx, const MatchAst& x, const Type* t) {
      LocalScopeManager new_scope { ctx.elaborator() };
      Elaboration subj = rvalue(ctx, ctx->elaborate(x.scrutinee()));
      const Type* subj_t = simplify_type(ctx.elaborator(), subj.type());
      if (auto var_t = is_variant_type(subj_t)) {
         PatternMatchContext ptn_ctx { ctx, *var_t, subj.code(), t };
         return elaborate_variant_match(ptn_ctx, x.branches());
      } else if (auto inst = is_parametric_variant_type(ctx, subj_t)) {
         PatternMatchContext ptn_ctx { ctx, *inst, subj.code(), t };
         return elaborate_variant_match(ptn_ctx, x.branches());
      }
      return elaborate_match_cases(ctx, subj, x.branches());
   }

   // Check that the type context `t' is appropriate for a
   // left- or right- section.  On success return the function type.
   static const ArrowType*
   check_section_context(BasicContext& ctx, const Type* t) {
      if (t == nullptr)
         return nullptr;
      auto ft = is<ArrowType>(t);
      if (ft == nullptr or ft->arity() != 1)
         semantics_error(ctx, "use of section in non-unary function context");
      return ft;
   }

   // ------------------------
   // -- PartialApplication --
   // ------------------------
   // Representation of data generated by partial application.
   namespace {
      struct PartialApplication : std::pair<FunctionElaboration, Elaboration> {
         PartialApplication() { }
         PartialApplication(FunctionElaboration f, Elaboration e)
               : std::pair<FunctionElaboration, Elaboration>(f, e)
         { }
         // courtesy conversion for use in conditionals.
         explicit operator bool() const { return first ? true : false; }
      };
   }

   // Return true if `s' is a binary function type whose
   // right section matches `t'.  As a special case, the right
   // section of any function type is acceptable if no target is given.
   static bool
   right_section_equal(const ArrowType* s, const ArrowType* t) {
      if (t == nullptr)
         return true;
      return s->target() == t->target()
         and s->argument(0) == t->argument(0);
   }

   // Return partial application data for viable right section.
   static PartialApplication
   viable_for_right_section(BasicContext& ctx, Elaboration f, Elaboration y,
                            const ArrowType* t) {
      if (auto ft = is<ArrowType>(f.type())) {
         if (ft->arity() == 2 and right_section_equal(ft, t))
            if (auto arg = successful_coercion(ctx, y, ft->argument(1)))
               return { { ft, f.code() }, arg };
      }
      return { };
   }

   // Return elaboration data resulting from a partial application of
   // an operator and given second argument.
   static PartialApplication
   fix_second_argument(BasicContext& ctx, const OperatorAst& op,
                       Elaboration y, const ArrowType* t) {
      std::vector<PartialApplication> result;
      for (auto& e : lexical_fiber_or_else(ctx, op))
         if (auto data = viable_for_right_section(ctx, rvalue(ctx, e), y, t))
            result.push_back(data);
      if (result.empty())
         semantics_error(ctx, "no suitable operator " + quote(&op)
                             + " in right section", &op);
      else if (result.size() > 1)
         semantics_error(ctx, "ambiguous right section", &op);
      return result.front();
   }

   // Return the type of a right section of a binary operation.
   static const ArrowType*
   right_section_type(BasicContext& ctx, const ArrowType* t) {
      return ctx->make_arrow_type(t->target(), { t->argument(0) });
   }
   
   // Subroutine of elaborate_right_section.
   // Build the elaboration for the lambda generated from a right section.
   static Elaboration
   lambda_for_right_section(BasicContext& ctx, FunctionElaboration fun,
                            Elaboration arg) {
      if (auto fun_type = fun.arrow_type()) {
         auto parm = fresh_formal(ctx, 0, fun_type->argument(0));
         Elaboration body = call(ctx, fun, { read(ctx, parm), arg });
         auto ftype = right_section_type(ctx, fun_type);
         LinkName name = { ctx->fresh_name(), ftype };
         // FIXME: close over variables from enclosing scopes.
         return { ftype, ctx->build_lambda(name, Formals{ parm }, body) };
      } else
         semantics_error(ctx, "right selections do not yet support dependent "
                              "types");
      return { };
   }

   // Elaborate a right section.
   static Elaboration
   elaborate_right_section(BasicContext& ctx, const RightSectionAst& x,
                           const Type* t) {
      auto ft = check_section_context(ctx, t);
      Elaboration arg = ctx->elaborate(x.rhs());
      auto data = fix_second_argument(ctx, *x.operation(), arg, ft);
      return ctx->coerce
         (lambda_for_right_section(ctx, data.first, data.second), ft);
   }

   static Elaboration
   the_empty_record(BasicContext& ctx) {
      return { ctx->make_record_type({ }), ctx->build_record() };
   }

   // Elaborate use of an empty brackets.
   // FIXME: Ideally, the semantics of these should be controlled
   // or given by concepts.
   static Elaboration
   elaborate_empty_enclosure(BasicContext& ctx, const EnclosureAst& x,
                            const Type* t) {
      if ((is_braced(x) or is_parenthesized(x))
          and (t == nullptr or are_equivalent(t, ctx->get_void(), ctx)))
         return { ctx->get_void(), nullptr };
      else if (is_parenthesized(x)
               and are_equivalent(t, ctx->get_typename(), ctx))
         return { t, ctx->get_void() };
      else if (is_braced(x) and are_equivalent(t, ctx->get_concept(), ctx))
         return { ctx->get_concept(), nullptr };
      else if (is_bracketed(x) and x.expr() == nullptr)
         return the_empty_record(ctx);
      return elaborate_name(ctx, *x.bracket(), t);
   }

   static Elaboration
   elaborate_record(BasicContext& ctx, const Ast* x, const RecordType* t) {
      if (x == nullptr) {
         if (not t->empty())
            semantics_error(ctx, "empty initializer for object of "
                            " record type " + quote(show(*t)));
         return { t, nullptr };
      }
      else if (auto y = is<SequenceAst>(x)) {
         if (length(y) < t->size())
            semantics_error(ctx, "too few initializer for record object", x);
         else if (length(y) > t->size())
            semantics_error(ctx, "too many initializer for record object", x);
         const std::size_t n = length(y);
         AssocArguments inits(n);
         for (std::size_t i = 0; i < n; ++i) {
            auto val = ctx->elaborate(y->at(i), t->at(i)->type());
            inits[i] = { t->at(i)->tag()->symbol(), val };
         }
         return { t, ctx->build_initializer(inits) };
      }
      if (t->empty())
         semantics_error(ctx, "missing initializer for record field");
      else if (t->size() > 1)
         semantics_error(ctx, "too many initializer for record object", x);
      auto e = ctx->elaborate(x, t->front()->type());
      auto n = t->front()->tag()->symbol();
      return { t, ctx->build_initializer(AssocArguments{ 1, {n, e} }) };
   }

   static bool
   is_paren_expr(const EnclosureAst& x) {
      if (not is_parenthesized(x))
         return false;
      return is<SequenceAst>(x.expr()) == nullptr;
   }
   
   static Elaboration
   elaborate_enclosure(BasicContext& ctx, const EnclosureAst& x, const Type* t) {
      if (x.expr() == nullptr)
         return elaborate_empty_enclosure(ctx, x, t);
      else if (auto rt = is<RecordType>(t)) {
         if (is_bracketed(x))
            return elaborate_record(ctx, x.expr(), rt);
      }
      else if (is_paren_expr(x))
         return ctx->elaborate(x.expr(), t);
      return elaborate_call(ctx, x.bracket(), get_sequence(x.expr()), t);
   }

   static void
   ensure_can_prove(BasicContext& ctx, Elaboration e) {
      if (e.type() == ctx->get_concept()) {
         if (auto req = is<Constraint>(e.code())) {
            if (not discharge_constraint(ctx, req))
               semantics_error(ctx, "could not satisfy constraint "
                                    + quote(show_expr(req)));
         } else
           semantics_error(ctx, "sorry, have not implemented support for "
                                 "non-constraint concepts");
      } else
        ensure_proposition_reduces_to_true(ctx, e);
   }

   static Elaboration
   elaborate_assert(BasicContext& ctx, const AssertAst& x) {
      auto e = elaborate_guard(ctx, x.predicate());
      ensure_can_prove(ctx, e);
      assume_property(ctx, e);
      if (ctx->enabled(debug::logic))
         ctx->debug() << "assertion: " << show_expr(e.code()) << std::endl;
      return { ctx->get_void(), ctx->get_void() };
   }

   static Elaboration
   elaborate_type_synonym(BasicContext& ctx, const AliasAst& x) {
      auto name = make_name(ctx, *x.alias());
      ensure_unique_name(ctx, name);
      auto t_t = ctx->get_typename();
      LinkName lnk { name, t_t };
      auto t = elaborate_type(ctx, x.value());
      declare(ctx->current_env(), lnk, t);
      return { ctx->get_void(), ctx->get_void() };
   }

   static Elaboration
   elaborate_postulate(BasicContext& ctx, const PostulateAst& x) {
      auto nt = get_name_type(ctx, x.form(), x.type());
      LinkName lnk { nt.name(), nt.type() };
      auto val = ctx->build_postulate(lnk);
      return declare(ctx->current_env(), lnk, val)->value();
   }

   // Elaborate the collection of input types for an arrow type.
   static InputTypes
   elaborate_arrow_domain(BasicContext& ctx, const SequenceAst& x) {
      InputTypes src;
      for (auto y : x.sequence())
         src.push_back(elaborate_type(ctx, y));
      return src;
   }

   // Subroutine of elaborate_arrow.
   // Elaborate the domain of an arrow type.  It can be a single type,
   // or a multitude of types in a parenthesized comma-separated list.
   static InputTypes
   elaborate_arrow_domain(BasicContext& ctx, const Ast* x) {
      // FIXME: the maze of if-statements below reflects an AST
      // design weakness.
      if (auto y = is<EnclosureAst>(x))
         if (is_parenthesized(*y)) {
            if (y->expr() == nullptr)
               return { };
            else if (auto z = is<SequenceAst>(y->expr()))
               return elaborate_arrow_domain(ctx, *z);
            return { elaborate_type(ctx, y) };
         }
      return { elaborate_type(ctx, x) };
   }

   static Elaboration
   elaborate_arrow(BasicContext& ctx, const ArrowAst& x, const Type* t) {
      // FIXME: "-> is somewhat polymorphic; default to type elaboration.
      if (t == nullptr or has_type_values(t, ctx)) {
         ParameterScopeManager parms_scope { ctx.elaborator() };
         auto source = elaborate_arrow_domain(ctx, x.source());
         auto target = elaborate_type(ctx, x.target());
         // FIXME: Check for dependency.
         auto ft = ctx->make_arrow_type(target, source);
         return ctx->coerce({ ctx->get_typename(), ft }, t);
      }
      semantics_error(ctx, "ambiguous arrow expression", x.source());
      return { };
   }

   static Elaboration
   elaborate_prolong(BasicContext& ctx, const ProlongAst& x) {
      auto name = get_name(ctx, x.form());
      auto decls = ctx->current_env()->lookup(name);
      if (decls.empty())
         semantics_error(ctx, "cannot prolong inexistent entity", &x);
      auto type = elaborate_type(ctx, x.type());
      if (not are_equivalent(type, ctx->get_namespace(), ctx))
         semantics_error(ctx, "cannot prolong a non-scope entity", x.type());
      auto decl = decls.select(ctx->get_namespace());
      if (decl == nullptr)
         semantics_error(ctx, "entity does not designate a scope", x.form());
      auto ns = is<Namespace>(decl->value());
      // FIXME: cast in next statement is a sign of design bug.
      elaborate_members(ctx, x.extension(), const_cast<Namespace*>(ns));
      return { ctx->get_namespace(), ns };
   }

   static Elaboration
   elaborate_where(BasicContext& ctx, const WhereAst& x, const Type* t) {
      LocalScopeManager new_scope { ctx.elaborator() };
      vector<Elaboration> decls;
      for (auto y : x.locals())
         decls.push_back(ctx->elaborate(y));
      auto body = ctx->elaborate(x.expression(), t);
      return { body.type(), ctx->build_let(decls, body) };
   }

   static Elaboration
   elaborate_variant(BasicContext& ctx, const DatatypeAst& x, const Type* t) {
      const std::size_t n = x.members().size();
      Sequence<Constructor> ctors(n);
      for (std::size_t i = 0; i < n; ++i)
         ctors[i] = elaborate_associated_ctor(ctx, *x.members()[i], i);
      auto value = ctx->make_variant_type(ctors);
      return ctx->coerce({ ctx->get_typename(), value }, t);
   }

   static void
   sorry_for_unimplemented(BasicContext& ctx, const Ast& x) {
      ctx->sorry("unimplemented elaboration for " + quote(show(&x)));
   }

   static Elaboration
   elaborate_datatype(BasicContext& ctx, const DatatypeAst& x, const Type* t) {
      switch (x.sort().kind) {
      case token::record_tok:
         return elaborate_record(ctx, x);
      case token::variant_tok:
         return elaborate_variant(ctx, x, t);
      default:
         sorry_for_unimplemented(ctx, x);
      }
      return { };
   }

   static void
   print_ast_elaboration(BasicContext& ctx, const Ast* x, const Expression* e,
                         const Type* t) {
      if (ctx->enabled(debug::codegen)) {
         ctx->debug() << "======== Elaborator::elaborate ========" << std::endl;
         ctx->debug() << show(x) << std::endl;
         if (t == nullptr)
            ctx->debug() << "\tinference mode" << std::endl;
         else {
            ctx->debug() << "\tchecking mode, expecting:" << std::endl;
            ctx->debug() << show_type(t) << std::endl;
         }
         ctx->debug() << "\t\t-----" << std::endl;
         ctx->debug() << show(e) << std::endl;
         ctx->debug() << "=======================================" << std::endl;
      }
   }

   template<typename T>
   using Identity = T;

   Elaboration
   Elaborator::elaborate(const Ast* ast, const Type* type) {
      if (ast == nullptr)
         return coerce({ get_void(), nullptr }, type);

      struct V {
         BasicContext& ctx;
         const Type* type;
         Elaboration result;
 
         void operator()(const Ast& x) { sorry_for_unimplemented(ctx, x); }

         void operator()(const LiteralAst& x) {
            result = elaborate_literal(ctx, x, type);
         }

         void operator()(const IdentifierAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const OperatorAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const BracketAst& x) {
            result = elaborate_name(ctx, x, type);
         }

         void operator()(const AssertAst& x) {
            result = elaborate_assert(ctx, x);
         }

         void operator()(const AliasAst& x) {
            result = elaborate_type_synonym(ctx, x);
         }

         void operator()(const DatatypeAst& x) {
            result = elaborate_datatype(ctx, x, type);
         }

         void operator()(const RuleAst& x) {
            result = ctx->coerce(elaborate_rule(ctx, x), type);
         }

         void operator()(const UnaryAst& x) {
            result = elaborate_unary(ctx, x, type);
         }

         void operator()(const BinaryAst& x) {
            result = elaborate_binary(ctx, x, type);
         }

         void operator()(const BiSectionAst& x) {
            result = elaborate_name(ctx, *x.operation(), type);
         }

         void operator()(const LeftSectionAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const RightSectionAst& x) {
            result = elaborate_right_section(ctx, x, type);
         }

         void operator()(const RestrictAst& x) {
            result = ctx->coerce(elaborate_restrict(ctx, x), type);
         }

         void operator()(const QuantifiedAst& x) {
            result = ctx->coerce(elaborate_quantified(ctx, x), type);
         }

         void operator()(const ApplyAst& x) {
            result = elaborate_call(ctx, x, type);
         }

         void operator()(const JuxtaposeAst& x) {
            result = elaborate_juxtapose(ctx, x, type);
         }

         void operator()(const SequenceAst& x) {
            result = elaborate_brace_list(ctx, x, type);
         }

         void operator()(const EnclosureAst& x) {
            result = elaborate_enclosure(ctx, x, type);
         }

         void operator()(const DotAst& x) {
            result = elaborate_dot(ctx, x, type);
         }

         void operator()(const IntervalAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const ExprStmtAst& x) {
            result = ctx->coerce(elaborate_expr_stmt(ctx, x), type);
         }

         void operator()(const ReturnAst& x) {
            result = elaborate_return(ctx, x);
         }

         void operator()(const ThrowAst& x) {
            result = elaborate_throw(ctx, x, type);
         }

         void operator()(const LeaveAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const MatchAst& x) {
            result = elaborate_match(ctx, x, type);
         }

         void operator()(const CompoundAst& x) {
            result = elaborate_block(ctx, x);
         }

         void operator()(const AssignmentAst& x) {
            result = elaborate_assignment(ctx, x);
         }

         void operator()(const IfAst& x) {
            result = elaborate_conditional(ctx, x, type);
         }

         void operator()(const FilterAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const CaseAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const CollectAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const RepeatAst& x) {
            result = elaborate_repeat(ctx, x);
         }

         void operator()(const LambdaAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const WhereAst& x) {
            result = elaborate_where(ctx, x, type);
         }

         void operator()(const SourceFileAst& x) {
            result = ctx->coerce(elaborate_source_file(ctx, x), type);
         }

         void operator()(const PathAst& x) {
            sorry_for_unimplemented(ctx, x);
         }

         void operator()(const ImportAst& x) {
            result = ctx->coerce(elaborate_import(ctx, x), type);
         }

         void operator()(const DefinitionAst& x) {
            result = ctx->coerce(elaborate_definition<Identity>(ctx, x), type);
         }

         void operator()(const ProlongAst& x) {
            result = ctx->coerce(elaborate_prolong(ctx, x), type);
         }

         void operator()(const PostulateAst& x) {
            result = ctx->coerce(elaborate_postulate(ctx, x), type);
         }

         void operator()(const ArrowAst& x) {
            result = elaborate_arrow(ctx, x, type);
         }

         void operator()(const ParameterAst& x) {
            result = ctx->coerce(elaborate_type(ctx, &x), type);
         }

         void operator()(const DescriptionAst&) {
            result = ctx->coerce({ ctx->get_void(), nullptr }, type);
         }
      };

      LocationManager push_loc(this, *anchor(ast));
      BasicContext ctx { this };
      ast_visitor<V> v(ctx, type, make_elaboration(get_void(), nullptr));
      ast->accept(v);
      print_ast_elaboration(ctx, ast, v.result.code(), type);
      return v.result;
   }
}
