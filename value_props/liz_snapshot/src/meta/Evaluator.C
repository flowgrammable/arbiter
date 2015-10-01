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
#include <cstring>
#include <string>
#include <algorithm>
#include <iterator>
#include "Evaluator.H"
#include <iomanip>

namespace liz {
   // --------------------------------------
   // -- Template instantiation machinery --
   // --------------------------------------
   template<typename Map>
   static const Type* subst_type(Elaborator*, const Type*, const Map&);

   template<typename Map>
   static TypeElaboration
   substitute(Elaborator* context, TypeElaboration e, const Map& map) {
      const Type* x = subst_type(context, e.code(), map);
      const Type* t = subst_type(context, e.type(), map);
      return { t, x };
   }

   template<typename Map>
   static const TagType*
   substitute(Elaborator* ctx, const TagType& x, const Map& map) {
      return ctx->make_tag_type(x.tag(), substitute(ctx, x.type(), map));
   }

   template<typename Map>
   static const FixedArrayType*
   substitute(Elaborator* ctx, const FixedArrayType& x, const Map& map) {
      auto elem_type = substitute(ctx, x.elem_type(), map);
      auto length = subst_expr(ctx, x.length(), map);
      return ctx->make_fixed_array_type(elem_type, length);
   }

   template<typename Map>
   static const UintType*
   substitute(Elaborator* ctx, const UintType& x, const Map& map) {
      auto width = subst_expr(ctx, x.expr(), map);
      return ctx->make_uint_type(*ctx->get_int(),
                                 *is<Value>(width.code()));
   }

   // -- Instantiate the function type `ftype' in a given `context'
   template<typename Map>
   static const ArrowType*
   substitute(Elaborator* context, const ArrowType* ftype, const Map& map) {
      const int nargs = ftype->arity();
      InputTypes args(nargs);
      for (int i = 0; i < nargs; ++i)
         args[i] = substitute(context, ftype->argument(i), map);
      return context->make_arrow_type
         (substitute(context, ftype->target(), map), args);
   }

   // -- Instantiate the dependent type `ftype' in a given `ctx'.
   static const ProductType*
   substitute(Elaborator*, const ProductType*, Substitution);

   static const ProductType*
   substitute(Elaborator*, const ProductType*, const Evidence&);

   // -- Instantiate generative type
   template<typename Map>
   static const GenerativeType*
   substitute(Elaborator* ctx, const GenerativeType& t, const Map& map) {
      auto rhs = subst_type(ctx, t.value(), map);
      if (rhs == t.value())
         return &t;
      // FIXME: instantiate scope, when we support parameterized scopes.
      // FIXME: re-instate support for parameterized names.
      TypeElaboration te { ctx->get_typename(), rhs };
      return ctx->make_generative_type(t.name(), te, t.scope());
   }

   // -- Instantiate a dependent type.
   template<typename Map>
   static const QuantifiedType*
   substitute(Elaborator* ctx, const QuantifiedType* qt, const Map& map) {
      auto t = substitute(ctx, qt->abstract_instance(), map);
      auto c = qt->constraint();
      if (t == qt->abstract_instance())
         return qt;
      return ctx->make_quantified_type(qt->quantifier(), qt->formals(), t, c);
   }

   template<typename Map>
   static const Constructor*
   instantiate(Elaborator*, const Constructor&, const Map&);

   template<typename Map>
   static const Type*
   substitute(Elaborator* ctx, const VariantType& x, const Map& map) {
      Sequence<Constructor> ctors;
      ctors.reserve(x.constructors().size());
      for (const Constructor* c: x.constructors())
         ctors.push_back(instantiate(ctx, *c, map));
      return ctx->make_variant_type(ctors);
   }

   template<typename Map>
   static const Type*
   substitute(Elaborator* ctx, const RecordType& x, const Map& map) {
      const std::size_t n = x.components().size();
      Sequence<TagType> tags(n);
      for (std::size_t i = 0; i != n; ++i) {
         tags[i] = substitute(ctx, *x.components()[i], map);
      }
      return ctx->make_record_type(tags);
   }

   // -- Instantiate an abstract type variable.
   // FIXME: This is a mess. Get a better understanding of why this works.
   template<typename Map>
   static const Type*
   substitute(Elaborator* ctx, const TypeExpression& x, const Map& map) {
      if (auto sym = is<SymbolicValue>(x.expr().code())) {
         Elaboration e(sym->type(), &sym->formal());
         e = subst_expr(ctx, e, map);
         if (e.code() == &sym->formal())
            return &x;
         else
            return ctx->coerce_to_type(e);
      }
      // FIXME: This is a hack to support recursive variants.
      auto e = subst_expr(ctx, x.expr(), map);
      // FIXME: Substitution could have created a MatchSet. This is a
      //        shortcoming in the implementation of match set. Need better
      //        semantics for this intrinsic.
      e = evaluate(ctx, e);
      if (auto result = is<Type>(e.code()))
         return result;
      return ctx->make_type_expression(e);
   }

   static const Type*
   substitute(Elaborator* ctx, const TypeExpression& x, const Evidence& ev) {
      if (auto s = is<SymbolicValue>(x.expr().code()))
         if (auto e = ev(&s->formal()))
            return ctx->coerce_to_type(e);
      // FIXME: This is a hack to support recursive variants.
      auto e = subst_expr(ctx, x.expr(), ev);
      return ctx->make_type_expression(e);
   }

   template<typename Map>
   struct TypeSubtitutionVisitor : Expression::Visitor {
      Elaborator* ctx;
      const Map& map;
      const Type* result;
      TypeSubtitutionVisitor(Elaborator* c, const Map& m)
            : ctx(c), map(m), result() { }
      
      void visit(const Expression& x) {
         internal_error("alien expression of type " + show_cxx_type(&x));
      }
      void visit(const BasicType& x) { result = &x; }
      void visit(const TagType& x) { result = substitute(ctx, x, map); }
      void visit(const ReferenceType& x) {
         result = ctx->make_reference_type(substitute(ctx, x.referee(), map));
      }
      void visit(const ArrayType& x) {
         result = ctx->make_array_type(substitute(ctx, x.elem_type(), map));
      }
      void visit(const FixedArrayType& x) {
         result = substitute(ctx, x, map);
      }
      void visit(const UintType& x) {
         result = substitute(ctx, x, map);
      }
      void visit(const ArrowType& x) {
         result = substitute(ctx, &x, map);
      }
      void visit(const ProductType& x) {
         result = substitute(ctx, &x, map);
      }
      void visit(const ReadonlyType& x) {
         result = ctx->make_readonly_type(substitute(ctx, x.type(), map));
      }
      void visit(const TypeExpression& x) {
         result = substitute(ctx, x, map);
      }
      void visit(const QuantifiedType& x) {
         result = substitute(ctx, &x, map);
      }
      void visit(const GenerativeType& x) { result = substitute(ctx, x, map); }
      void visit(const VariantType& x) { result = substitute(ctx, x, map); }
      void visit(const RecordType& x) { result = substitute(ctx, x, map); }
   };
   
   template<typename Map>
   static const Type*
   subst_type(Elaborator* ctx, const Type* t, const Map& map) {
      if (t == nullptr)
         return nullptr;
      TypeSubtitutionVisitor<Map> v(ctx, map);
      t->accept(v);
      return v.result;
   }

   template<typename Map>
   static Arguments
   substitute(Elaborator* context, const Arguments& args, const Map& map) {
      Arguments v(args.size());
      for (std::size_t i = 0; i < args.size(); ++i)
         v[i] = subst_expr(context, args[i], map);
      return v;
   }

   template<typename Map>
   static const Constraint*
   substitute(Elaborator* context, const Constraint* c, const Map& map) {
      // FIXME: instantiate concepts.
      return context->build_constraint
         (c->constructor(), substitute(context, c->arguments(), map));
   }

   // -- Instantiate a name.
   template<typename Map>
   static const Name* subst_name(Elaborator*, const Name*, const Map&);

   template<typename Map>
   static const LinkName*
   substitute(Elaborator* context, const LinkName* lnk, const Map& map) {
      return context->build_name
         (lnk->name(), subst_type(context, lnk->type(), map));
   }

   template<typename Map>
   static LinkName
   substitute(Elaborator* context, const LinkName& lnk, const Map& map) {
      return { lnk.name(), subst_type(context, lnk.type(), map) };
   }

   // -- Instantiate a parameter by instantiating its type through
   // -- the substitution.  This is different from replacing the
   // -- formal by its image by the substitution.
   template<typename Map>
   static const Formal*
   subst_formal(Elaborator* ctx, const Formal* parm, const Map& map) {
      TypeElaboration t = substitute(ctx, parm->type(), map);
      auto lnk = substitute(ctx, parm->link_name(), map);
      if (t == parm->type() and lnk == parm->link_name())
         return parm;
      return ctx->build_formal(parm->position(), parm->level(), t, lnk);
   }

   template<typename Map>
   static Formals
   subst_formals(Elaborator* ctx, const Formals& fs, const Map& map) {
      Formals nfs;
      nfs.reserve(fs.size());
      for (auto f: fs)
         nfs.push_back(subst_formal(ctx, f, map));
      return nfs;
   }

   static Formals
   subst_and_remember_formals(Elaborator* ctx, const Formals& old_formals,
                              Substitution& subst)
   {
      const std::size_t n = old_formals.size();
      Formals new_formals(n);
      for (std::size_t i = 0; i < n; ++i) {
         new_formals[i] = subst_formal(ctx, old_formals[i], subst);
         if (new_formals[i] != old_formals[i])
            subst[old_formals[i]] = { new_formals[i]->type(), new_formals[i] };
      }
      return new_formals;
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

   // -- Instantiate a user-defined function `fun' in a given
   // -- `context' and substitution `subst'.
   const Lambda*
   substitute(Elaborator* ctx, const Lambda* fun, Substitution subst) {
      // 1. Don't work too hard on repeating yourself.
      if (subst.empty())
         return fun;
      if (const Expression* x =
          ctx->retrieve_specialization(fun, subst))
         return is<Lambda>(x);
      // 2, Register a declaration for this instance to cope
      //    recursion.
      auto lnk = substitute(ctx, fun->link_name(), subst);
      auto formals = subst_formals(ctx, fun->formals(), subst);
      Elaboration body = subst_expr(ctx, fun->body(), subst);
      auto inst = ctx->build_lambda(lnk, formals, body);
      ctx->register_specialization(inst, fun, subst);
      if (check_subst_is_closed(subst))
         if (not is_a_coercion(fun))
            ctx->remember_specialization({ inst->type(), inst });
      return inst;
   }

   static const ProductType*
   substitute(Elaborator* ctx, const ProductType* deptype, Substitution subst) {
      if (subst.empty())
         return deptype;
      auto formals = subst_and_remember_formals(ctx, deptype->source(), subst);
      auto target = substitute(ctx, deptype->target(), subst);
      auto restriction = subst_expr(ctx, deptype->restriction(), subst);
      return ctx->make_product_type(formals, target, restriction);
   }

   template<typename T, typename Map>
   static const T*
   instantiate(Elaborator* ctx, const T& x, unary_builder<T> builder,
               const Map& map) {
      auto fun = substitute(ctx, x.function(), map);
      auto arg = subst_expr(ctx, x.argument(), map);
      return (ctx->*builder)(fun, arg);
   }

   template<typename T, typename Map>
   static const T*
   instantiate(Elaborator* ctx, const T& x, binary_builder<T> builder,
               const Map& map) {
      auto fun = substitute(ctx, x.function(), map);
      auto lhs = subst_expr(ctx, x.lhs(), map);
      auto rhs = subst_expr(ctx, x.rhs(), map);
      return (ctx->*builder)(fun, lhs, rhs);
   }

   template<typename Map>
   static const Constructor*
   instantiate(Elaborator* ctx, const Constructor& c, const Map& map) {
      auto type = subst_type(ctx, c.type(), map);
      Elaboration impl = c.implementation();
      if (impl)
         subst_expr(ctx, c.implementation(), map);
      if (type == c.type() and impl == c.implementation())
         return &c;
      return ctx->build_constructor({ c.name(), type }, impl);
   }

   static const Value* reify(Elaborator*, Object);

   static bool
   is_universally_quantified_type(const Type* t) {
      if (auto qt = is<QuantifiedType>(t))
         return qt->quantifier() == Quantifier::forall;
      return false;
   }

   static inline Elaboration
   represents_rec_lambda(Elaborator* ctx, const Read& x, const Type* t) {
      if (auto lam = is<Lambda>(evaluate(ctx, { t, &x }).code())) {
         if (    is_universally_quantified_type(lam->type()) and
             not is_universally_quantified_type(t))
         return lam->body();
      }
      return { };
   }

   static const Expression*
   defer_recursive_call(Elaborator* ctx, const Read& x,
                        const Substitution& subst, const Type* t)
   {
      if (auto e = represents_rec_lambda(ctx, x, t))
         return ctx->build_subst_expr(subst, e);
      return nullptr;
   }

   static const Value*
   substitute_instance_parameter(Elaborator& ctx, const Value* v,
                                 const Substitution& subst)
   {
      if (auto t = is<Type>(v))
         return subst_type(&ctx, t, subst);
      const SymbolicValue* sym = is<SymbolicValue>(v);
      if (sym == nullptr)
         return v;
      if (auto e = subst.has(&sym->formal())) {
         if (auto new_v = is<Value>(e.code()))
            return new_v;
         else
            // FIXME: Must be proven.
            internal_error("substituting for a non-value");
      }
      return v;
   }

   namespace {
      struct InstantiatorVisitor : Expression::Visitor {
         Elaborator* ctx;
         const Substitution& subst;
         Elaboration result;
         InstantiatorVisitor(Elaborator* c, const Substitution& s,
                             const Type* t)
               : ctx(c),
                 subst(s),
                 result(t, nullptr)
         { }

         void visit(const Expression& x) {
            internal_error("alien expression of type " + show_cxx_type(&x));
         }
         void visit(const LinkName& x) {
            result.code(substitute(ctx, &x, subst));
         }
         void visit(const Value& v) { result.code(&v); }
         void visit(const Constructor& c) {
            result.code(instantiate(ctx, c, subst));
         }
         void visit(const Instance& x) {
            // FIXME: Should be a better way to do this. Dependent instance?
            InputTypes types = source(x.constructor()->type());
            const std::size_t n = x.arguments().size();
            Sequence<Value> vals(n);
            for (std::size_t i = 0; i != n; ++i)
               vals[i] = substitute_instance_parameter(*ctx, x.arguments()[i], subst);
            auto inst = ctx->build_instance(x.constructor(), vals);
            ctx->remember_specialization({ result.type(), inst });
            result.code(inst);
         }
         void visit(const Type& x) { result.code(subst_type(ctx, &x, subst)); }
         void visit(const Formula& x) {
            const std::size_t n = x.parameters().size();
            Formals parms(n);
            Substitution renamer;
            for (std::size_t i = 0; i < n; ++i)
               parms[i] = subst_formal(ctx, x.parameter(i), subst);
            Elaboration body = subst_expr(ctx, x.body(), renamer);
            body = subst_expr(ctx, x.body(), subst);
            result.code(ctx->build_formula(x.quantifier(), parms, body));
         }
         // FIXME: don't we need to instantiate the type of the formal too?
         void visit(const Formal& x) { result = subst(&x); }
         void visit(const SymbolicValue& x) { result = subst(&x.formal()); }
         void visit(const NiladicBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            auto params = substitute(ctx, x.params(), subst);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const UnaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            auto params = substitute(ctx, x.params(), subst);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const BinaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            auto params = substitute(ctx, x.params(), subst);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const TernaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), subst);
            auto params = substitute(ctx, x.params(), subst);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const Lambda& x) {
            result.code(substitute(ctx, &x, subst));
         }
         void visit(const Signature& x) {
            auto lnk = substitute(ctx, x.link_name(), subst);
            result.code(ctx->build_signature(lnk, x.sig_id()));
         }
         // FIXME: Quantified parameters may appear in the context of an
         //        expression.
         void visit(const Read& x) {
            if (auto rec = defer_recursive_call(ctx,x,subst,result.type())) {
               result.code(rec);
            } else {
               // FIXME: Create special case for quantified parameters.
               Elaboration s = subst_expr(ctx, x.address(), subst);
               result.code(ctx->build_read(s));
            }
         }
         void visit(const Initializer& x) {
            const std::size_t n = x.size();
            AssocArguments assoc_args(n);
            for (std::size_t i = 0; i != n; ++i)
               assoc_args[i] = make_pair(x[i].first,
                                         subst_expr(ctx, x[i].second, subst));
            result.code(ctx->build_initializer(assoc_args));
         }
         void visit(const InitializerList& x) {
            const std::size_t n = x.size();
            Arguments args(n);
            for (std::size_t i = 0; i != n; ++i)
               args[i] = subst_expr(ctx, x[i], subst);
            result.code(ctx->build_initializer_list(args));
         }
         void visit(const Write& x) {
            result.code
               (ctx->build_write(subst_expr(ctx, x.location(), subst),
                                 subst_expr(ctx, x.value(), subst)));
         }
         void visit(const Offset& x) {
            result.code
               (ctx->build_offset(subst_expr(ctx, x.address(), subst),
                                  subst_expr(ctx, x.delta(), subst)));
         }
         void visit(const Component& x) {
            auto n = substitute(ctx, x.link_name(), subst);
            result.code
               (ctx->build_component(subst_expr(ctx, x.whole(), subst), n));
         }
         void visit(const DotSelection& x) {
            auto n = substitute(ctx, x.link_name(), subst);
            result.code
               (ctx->build_dot(subst_expr(ctx, x.whole(), subst), n));
         }
         void visit(const Negate& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_negate, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Not& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_not, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Complement& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_complement, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Plus& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_plus, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Dash& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_dash, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Star& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_star, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Slash& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_slash, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Div& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_div, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Mod& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_mod, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Rem& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rem, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Langle& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_langle, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Rangle& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rangle, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Langleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_langleq, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Rangleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rangleq, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Eqeq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_eqeq, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Excleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_excleq, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const And& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_and, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const Or& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_or, subst));
            // result = evaluate(ctx, result);
         }
         void visit(const CallExpression& x) {
            auto fun = substitute(ctx, x.function(), subst);
            result.code
               (ctx->build_call(fun, substitute(ctx, x.arguments(), subst)));
            // This is causing problems.
            //result = evaluate(ctx, result);
         }
         void visit(const BinaryLogical& x) {
            result.code
               (ctx->build_logical
                (x.operation(), subst_expr(ctx, x.lhs(), subst),
                 subst_expr(ctx, x.rhs(), subst)));
         }
         void visit(const Constraint& x) {
            // FIXME: Handle parameterized concepts later.
            result.code
               (ctx->build_constraint
                (x.constructor(), substitute(ctx, x.arguments(), subst)));
         }
         void visit(const Alias& x) {
            auto lnk = substitute(ctx, x.alias().link_name(), subst);
            auto alias = ctx->build_signature(lnk);
            auto equality = subst_expr(ctx, x.equality(), subst);
            result.code(ctx->build_alias(*alias, equality));
         }
         void visit(const Return& x) {
            result.code
               (ctx->build_return(subst_expr(ctx, x.expression(), subst)));
         }
         void visit(const Throw& x) {
            result.code
               (ctx->build_throw(subst_expr(ctx, x.expression(), subst)));
         }
         void visit(const Loop& x) {
            result.code(ctx->build_loop(subst_expr(ctx, x.body(), subst)));
         }
         void visit(const Leave& x) {
            result.code(
               ctx->build_leave(subst_expr(ctx, x.expression(), subst)));
         }
         void visit(const IfExpression& x) {
            Elaboration c = subst_expr(ctx, x.condition(), subst);
            Elaboration tt = subst_expr(ctx, x.consequence(), subst);
            Elaboration ff = subst_expr(ctx, x.alternative(), subst);
            result.code(ctx->build_if(c, tt, ff));
         }
         void visit(const BindExpression& x) {
            auto lnk = substitute(ctx, x.link_name(), subst);
            auto init = subst_expr(ctx, x.initializer(), subst);
            result.code(ctx->build_bind(lnk, init));
         }
         void visit(const LetExpression& x) {
            std::vector<Elaboration> locals;
            locals.reserve(x.locals().size());
            for (auto local: x.locals())
               locals.push_back(subst_expr(ctx, local, subst));
            auto body = subst_expr(ctx, x.body(), subst);
            result.code(ctx->build_let(locals, body));
         }
         void visit(const Block& x) {
            result.code
               (ctx->build_block(substitute(ctx, x.statements(), subst)));
         }
         void visit(const PatternMatch& x) {
            auto scrutinee = subst_expr(ctx, x.scrutinee(), subst);
            PatternClauses clauses;
            clauses.reserve(x.clauses().size());
            for (auto clause: x.clauses()) {
               auto pattern = subst_expr(ctx, clause.pattern(), subst);
               auto action = subst_expr(ctx, clause.action(), subst);
               clauses.emplace_back(pattern, action);
            }
            result.code(ctx->build_pattern_match(scrutinee, clauses));
         }
         void visit(const PatternInstance& x) {
            auto formals = subst_formals(ctx, x.formals(), subst);
            result.code(ctx->build_pattern_instance(x.constructor(), formals));
         }
      };
   }

   Elaboration
   subst_expr(Elaborator* ctx, Elaboration expr, const Substitution& subst) {
      if (subst.empty())
         return expr;
      const Type* t = subst_type(ctx, expr.type(), subst);
      if (expr.code() == nullptr)
         return make_elaboration(t, nullptr);
      InstantiatorVisitor v(ctx, subst, t);
      expr.code()->accept(v);
      return v.result;
   }

   SimpleFuncElaboration
   substitute(Elaborator* ctx, SimpleFuncElaboration fun,
              const Substitution& subst)
   {
      if (subst.empty())
         return fun;
      auto t = substitute(ctx, fun.type(), subst);
      if (fun.code() == nullptr)
         return { t, nullptr };
      InstantiatorVisitor v(ctx, subst, t);
      fun.code()->accept(v);
      return { t, v.result.code() };
   }

   DependentElaboration
   substitute(Elaborator* ctx, DependentElaboration fun,
              const Substitution& subst)
   {
      if (subst.empty())
         return fun;
      auto t = substitute(ctx, fun.type(), subst);
      if (fun.code() == nullptr)
         return { t, nullptr };
      InstantiatorVisitor v(ctx, subst, t);
      fun.code()->accept(v);
      return { t, v.result.code() };
   }

   FunctionElaboration
   substitute(Elaborator* ctx, FunctionElaboration fun,
              const Substitution& subst) {
      if (auto t = fun.arrow_type())
         return substitute(ctx, SimpleFuncElaboration{ t, fun.code() }, subst);
      return substitute(ctx,
                        DependentElaboration{ fun.product_type(), fun.code() },
                        subst);
   }

   // --------------
   // -- Evidence --
   // --------------
   Evidence::Evidence() : ok(true), sub_evidence_map(), aliases() { }

   const Expression*
   Evidence::operator()(const Signature* s) const {
      for (const_iterator i = cbegin(); i != cend(); ++i)
         if (i->first->sig_id() == s->sig_id())
            return i->second;
      return nullptr;
   }

   Elaboration
   Evidence::operator()(const Formal* f) const {
      if (f == nullptr)
         return { };
      if (aliases.has(f))
         return aliases(f);
      return { };
   }


   void
   Evidence::send_to(const Signature* s, const Expression* f) {
      if (f == nullptr)
         system_error("no function resolution for signature "
                         + quote(show_expr(s)));
      iterator p = find(s);
      if (p != end() and p->first != s)
         system_error("conflicting resolution for signature "
                      + quote(show_expr(s)));
      insert(std::make_pair(s, f));
   }

   void
   Evidence::send_to(const Expression* c, const Evidence* e) {
      if (e == nullptr)
         system_error("no evidence for constraint "
                         + quote(show_expr(c)));
      ConstraintMap::iterator p = sub_evidence_map.find(c);
      if (p != sub_evidence_map.end() and p->first != c)
         system_error("conflicting resolution for constraint "
                         + quote(show_expr(c)));
      sub_evidence_map.insert(std::make_pair(c, e));
   }

   void
   Evidence::send_to(const Formal* f, Elaboration e) {
      if (f == nullptr)
         abort();
      if (not e)
         system_error("no evidence for alias " + quote(show_expr(f)));
      if (aliases.has(f))
         system_error("conflicting evidence for alias " + quote(show_expr(f)));
      aliases[f] = e;
   }

   void
   Evidence::subsume(const Evidence& ev) {
      if (ev.good()) {
         for (auto sig: ev)
            send_to(sig.first, sig.second);
         for (auto sub: ev.aliases)
            aliases[sub.first] = sub.second;
      }
      else
         set_bad();
   }

   // -- substitute evidence into function expressions.
   

   static const Lambda*
   substitute(Elaborator* ctx, const Lambda* fun, const Evidence& evidence) {
      Elaboration body = subst_expr(ctx, fun->body(), evidence);
      auto inst = ctx->build_lambda
         (substitute(ctx, fun->link_name(), evidence), fun->formals(), body);
      return inst;
   }

   static const ProductType*
   substitute(Elaborator* ctx, const ProductType* deptype,
              const Evidence& evidence)
   {
      auto restriction = subst_expr(ctx, deptype->restriction(), evidence);
      auto target = substitute(ctx, deptype->target(), evidence);
      return ctx->make_product_type(deptype->source(), target, restriction);
   }

   namespace {
      struct SignatureSubstitutionVisitor : Expression::Visitor {
         Elaborator* ctx;
         const Evidence& ev;
         Elaboration result;
         SignatureSubstitutionVisitor(Elaborator* c, const Type* t,
                                      const Evidence& e)
               : ctx(c), ev(e), result(t, nullptr) { }

         void visit(const Expression& x) {
            internal_error("alien expression of type " + show_cxx_type(&x));
         }
         void visit(const Value& v) { result.code(&v); }
         void visit(const LinkName& lnk) { result.code(&lnk); }
         void visit(const Constructor& c) {
            result.code(instantiate(ctx, c, ev));
         }
         void visit(const Formal& f) { result.code(&f); }
         void visit(const SymbolicValue& x) { result.code(&x); }
         void visit(const NiladicBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            Arguments params = substitute(ctx, x.params(), ev);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const UnaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            Arguments params = substitute(ctx, x.params(), ev);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const BinaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            Arguments params = substitute(ctx, x.params(), ev);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const TernaryBuiltinFunction& x) {
            const ArrowType* t = substitute(ctx, x.type(), ev);
            Arguments params = substitute(ctx, x.params(), ev);
            result.code(ctx->build_builtin(x.name(), t, x.code(), params));
         }
         void visit(const Lambda& x) { result.code(substitute(ctx, &x, ev)); }
         void visit(const Signature& x) { result.code(ev(&x)); }
         void visit(const Type& x) { result.code(subst_type(ctx, &x, ev)); }
         // FIXME:
         // void visit(const TypeExpression& x) {
         //    result = coerce_to_type
         //       (ctx, (evaluate(ctx, subst_expr
         //                           (ctx, x.expr, ev))));
         // }
         void visit(const Read& x) {
            Elaboration s = subst_expr(ctx, x.address(), ev);
            result.code(ctx->build_read(s));
         }
         void visit(const Write& x) {
            result.code
               (ctx->build_write
                (subst_expr(ctx, x.location(), ev),
                 subst_expr(ctx, x.value(), ev)));
         }
         void visit(const Offset& x) {
            result.code
               (ctx->build_offset
                (subst_expr(ctx, x.address(), ev),
                 subst_expr(ctx, x.delta(), ev)));
         }
         void visit(const Component& x) {
            auto n = substitute(ctx, x.link_name(), ev);
            result.code
               (ctx->build_component(subst_expr(ctx, x.whole(), ev), n));
         }
         void visit(const DotSelection& x) {
            auto n = substitute(ctx, x.link_name(), ev);
            result.code
               (ctx->build_dot(subst_expr(ctx, x.whole(), ev), n));
         }
         void visit(const Negate& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_negate, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Not& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_not, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Complement& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_complement, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Plus& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_plus, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Dash& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_dash, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Star& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_star, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Slash& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_slash, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Div& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_div, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Mod& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_mod, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Rem& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rem, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Langle& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_langle, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Rangle& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rangle, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Langleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_langleq, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Rangleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_rangleq, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Eqeq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_eqeq, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Excleq& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_excleq, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const And& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_and, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const Or& x) {
            result.code(instantiate(ctx, x, &Elaborator::build_or, ev));
            // result = evaluate(ctx, result);
         }
         void visit(const BinaryLogical& x) {
            result.code
               (ctx->build_logical
                (x.operation(),
                 subst_expr(ctx, x.lhs(), ev),
                 subst_expr(ctx, x.rhs(), ev)));
         }
         void visit(const CallExpression& x) {
            auto fun = substitute(ctx, x.function(), ev);
            result.code
               (ctx->build_call
                (fun, substitute(ctx, x.arguments(), ev)));
            //result = evaluate(ctx, result);
         }
         void visit(const Constraint& x) {
            // FIXME: Handle parameterized concepts later.
            result.code
               (ctx->build_constraint
                (x.constructor(), substitute(ctx, x.arguments(), ev)));
         }
         void visit(const Return& x) {
            Elaboration e = subst_expr(ctx, x.expression(), ev);
            result.code(ctx->build_return(e));
         }
         void visit(const Throw& x) {
            Elaboration e = subst_expr(ctx, x.expression(), ev);
            result.code(ctx->build_throw(e));
         }
         void visit(const Loop& x) {
            Elaboration e = subst_expr(ctx, x.body(), ev);
            result.code(ctx->build_loop(e));
         }
         void visit(const Leave& x) {
            if (x.expression().code() != nullptr) {
               Elaboration e = subst_expr(ctx, x.expression(), ev);
               result.code(ctx->build_leave(e));
            } else
               result.code(&x);
         }
         void visit(const IfExpression& x) {
            Elaboration cond = subst_expr(ctx, x.condition(), ev);
            Elaboration conseq = subst_expr(ctx, x.consequence(), ev);
            Elaboration alt = subst_expr(ctx, x.alternative(), ev);
            result.code(ctx->build_if(cond, conseq, alt));
         }
         void visit(const BindExpression& x) {
            auto lnk = substitute(ctx, x.link_name(), ev);
            auto init = subst_expr(ctx, x.initializer(), ev);
            result.code(ctx->build_bind(lnk, init));
         }
         void visit(const LetExpression& x) {
            std::vector<Elaboration> locals;
            locals.reserve(x.locals().size());
            for (auto local: x.locals())
               locals.push_back(subst_expr(ctx, local, ev));
            auto body = subst_expr(ctx, x.body(), ev);
            result.code(ctx->build_let(locals, body));
         }
         void visit(const Block& x) {
            result.code
               (ctx->build_block(substitute(ctx, x.statements(), ev)));
         }
         void visit(const Initializer& x) {
            const std::size_t n = x.size();
            AssocArguments assoc_args(n);
            for (std::size_t i = 0; i != n; ++i)
               assoc_args[i] = make_pair(x[i].first,
                                         subst_expr(ctx, x[i].second, ev));
            result.code(ctx->build_initializer(assoc_args));
         }
         void visit(const InitializerList& x) {
            const std::size_t n = x.size();
            Arguments args(n);
            for (std::size_t i = 0; i != n; ++i)
               args[i] = subst_expr(ctx, x[i], ev);
            result.code(ctx->build_initializer_list(args));
         }
         void visit(const PatternMatch& x) {
            auto scrutinee = subst_expr(ctx, x.scrutinee(), ev);
            PatternClauses clauses;
            clauses.reserve(x.clauses().size());
            for (auto clause: x.clauses()) {
               auto pattern = subst_expr(ctx, clause.pattern(), ev);
               auto action = subst_expr(ctx, clause.action(), ev);
               clauses.emplace_back(pattern, action);
            }
            result.code(ctx->build_pattern_match(scrutinee, clauses));
         }
      };
   }

   Elaboration
   subst_expr(Elaborator* ctx, Elaboration expr, const Evidence& evidence) {
      if (expr.code() == nullptr or evidence.empty())
         return expr;
      const Type* t = subst_type(ctx, expr.type(), evidence);
      SignatureSubstitutionVisitor v(ctx, t, evidence);
      expr.code()->accept(v);
      // Substitute existentials.
      return subst_expr(ctx, v.result, evidence.aliases);
   }

   SimpleFuncElaboration
   substitute(Elaborator* ctx, SimpleFuncElaboration fun,
              const Evidence& evidence)
   {
      if (evidence.empty())
         return fun;
      auto t = substitute(ctx, fun.type(), evidence);
      if (fun.code() == nullptr)
         return { t, nullptr };
      SignatureSubstitutionVisitor v(ctx, t, evidence);
      fun.code()->accept(v);
      // Substitute existentials.
      fun = { t, v.result.code() };
      return substitute(ctx, fun, evidence.aliases);
   }

   DependentElaboration
   substitute(Elaborator* ctx, DependentElaboration fun,
              const Evidence& evidence)
   {
      if (evidence.empty())
         return fun;
      auto t = substitute(ctx, fun.type(), evidence);
      if (fun.code() == nullptr)
         return { t, nullptr };
      SignatureSubstitutionVisitor v(ctx, t, evidence);
      fun.code()->accept(v);
      fun = { t, v.result.code() };
      return substitute(ctx, fun, evidence.aliases);
   }

   FunctionElaboration
   substitute(Elaborator* ctx, FunctionElaboration fun,
              const Evidence& evidence)
   {
      if (auto t = fun.arrow_type()) {
         SimpleFuncElaboration f { t, fun.code() };
         return substitute(ctx, f, evidence);
      }
      DependentElaboration f { fun.product_type(), fun.code() };
      return substitute(ctx, f, evidence);
   }

   // -- FreeVariableError --
   FreeVariableError::FreeVariableError(const std::string& m, Symbol s)
         : BasicError(m), sym(s)
   { }

   void
   FreeVariableError::format_message(std::ostream& os) const {
      os << msg << ": " << sym.string();
   }

   static void
   free_variable_error(const std::string& msg, Symbol s) {
      throw FreeVariableError(msg, s);
   }

   namespace Data {
      inline Namespace*
      to_namespace(Value v) {
         return static_cast<Namespace*>(Abstract(v));
      }
   }
   // ----------------
   // -- StoreStack --
   // ----------------

   void
   StoreStack::push(Store* s) {
      push_back(StoreRef(s));
   }

   void
   StoreStack::pop() {
      pop_back();
   }
   
   //  -- debugging functions
   void
   print_bindings(const Scope* scope) {
      Scope::const_iterator p = scope->begin();
      Scope::const_iterator last = scope->end();
      for (; p != last; ++p) {
         std::cerr << '\t' << p->name()->symbol()
                   << " +-> ";
         prefix_form(std::cerr, p->value().code());
         std::cerr << '\n';
      }
   }

   // --------------------------------
   // -- Evaluation by substitution --
   // --------------------------------

   // Types that should not be reified.
   static bool
   should_not_reify(Elaborator* context, Object obj) {
      return is<TypeExpression>(obj.type())
          or is<ArrowType>(obj.type())
          or is<RecordType>(obj.type())
          or is<VariantType>(obj.type())
          or is<ArrayType>(obj.type())
          or obj.type() == context->get_key()
          or obj.type() == context->get_concept();
   }

   // -- On occasions, we have closed expressions that we must
   // -- reduce to values.  And those values may be needed in
   // -- in the intermediate language.  This routine is responsible
   // -- reifying objects to their value expression forms.
   static const Value*
   reify(Elaborator* context, Object obj) {
      if (should_not_reify(context, obj))
         return obj.value();
      // FIXME: This is needed because generative types like `Z<int>` actually
      //        hold values of the underlying type.
      if (auto gen_t = is<GenerativeType>(obj.type()))
         return reify(context, { gen_t->value().code(), obj.value() });
      if (is<UintType>(obj.type()))
         return obj.value();
      switch (obj.type()->data_traits()->mode) {
      case Data::Mode::Void:
         return nullptr;
      case Data::Mode::Bool:
         return context->build_bool(bool(obj.value()), obj.type());
      case Data::Mode::Char:
         return context->build_char(obj.value(), obj.type());
      case Data::Mode::Int:
         return context->build_int(intmax_t(obj.value()), obj.type());
      case Data::Mode::Dfloat:
         return context->build_double(double(obj.value()), obj.type());
      case Data::Mode::String:
      case Data::Mode::Symbol:
         return context->build_string(obj.value(), obj.type());
      case Data::Mode::Pointer:
      default:
         if (obj.type() != context->get_typename())
            system_error("cannot reify value of type " + show(*obj.type()));
         return Data::to_type(obj.value());
      }
   }

   // -- FIXME: Why? Seriously. `Object` should live within one universe. It
   //           appears to be living a life as a Data::Value and an Elaboration.
   static Object
   unreify(Object obj) {
      auto e = static_cast<const Expression*>(obj.value());
      if (auto x = is<Bool>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Byte>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Char>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Int>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Uint>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Double>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<String>(e))
         return { obj.type(), x->rep() };
      else if (auto x = is<Array>(e))
         return { obj.type(), x->rep() };
      return obj;
   }

   // There are situations were we want to evaluate certain calls
   // (notably those producing types) at elaboration time.  However,
   // on occasions some of the arguments are not ground, so normal
   // evaluation should not be attempted.  If the invoked function is simple
   // enough we can still perform evaluation by substitution.
   // This routine determines whether a function is `simple enough'.
   // More generally, it determines whether an expression is simple
   // enough to be evaluated by substitution.
   static bool is_simple(Elaboration);

   static bool
   is_simple(const Arguments& seq) {
      bool result = true;
      for (std::size_t i = 0; result and i < seq.size(); ++i)
         result = is_simple(seq[i]);
      return result;
   }

   namespace {
      struct IsSimpleVisitor : Expression::Visitor {
         bool result;
         IsSimpleVisitor() : result(false) { }

         void visit(const Expression&) { }
         void visit(const Value&) { result = true; }
         void visit(const TagType& x) {
            result = is_simple(x.type());
         }
         void visit(const ReferenceType& x) {
            result = is_simple(x.referee());
         }
         void visit(const ArrowType& x) {
            result = is_simple(x.target());
            for (std::size_t i = 0; result and i < x.arity(); ++i)
               result = is_simple(x.argument(i));
         }
         void visit(const TypeExpression& x) {
            result = is_simple(x.expr());
         }
         void visit(const ReadonlyType& x) {
            result = is_simple(x.type());
         }
         void visit(const Formal&) { result = true; }
         void visit(const SymbolicValue&) { result = true; }
         void visit(const LinkName&) { result = true; }
         void visit(const Read& x) { result = is_simple(x.address()); }
         void visit(const UnaryExpression& x) {
            result = is_simple(x.argument());
         }
         void visit(const BinaryExpression& x) {
            result = is_simple(x.lhs()) and is_simple(x.rhs());
         }
         void visit(const BinaryLogical& x) {
            result = is_simple(x.lhs()) and is_simple(x.rhs());
         }
         void visit(const CallExpression& x) {
            result = is_simple(x.function()) and is_simple(x.arguments());
         }
         void visit(const Return& x) {
            result = is_simple(x.expression());
         }

         void visit(const Block& x) {
            // A block is simple only if it has at most one statement.
            result = x.size() < 2;
            for (int i = 0; result and i < x.size(); ++i)
               result = is_simple(x.statement(i));
         }
      };
   }

   static bool
   is_simple(Elaboration expr) {
      if (expr.code() == 0)
         return true;
      IsSimpleVisitor v;
      expr.code()->accept(v);
      return v.result;
   }

   // -- Simple evaluator for type expressions
   namespace {
      struct ParameterMap : std::map<Symbol, const Expression*> {
         bool is_defined_at(Symbol s) const {
            return find(s) != end();
         }

         const Expression* operator()(Symbol s) const {
            const_iterator p = find(s);
            return p == end() ? nullptr : p->second;
         }
      };
   }

   static const Type*
   simplify_type(Elaborator*, const Type*, const ParameterMap&);

   static Elaboration
   simplify(Elaborator*, Elaboration, const ParameterMap&);

   static TypeElaboration
   simplify(Elaborator* context, TypeElaboration expr,
            const ParameterMap& map) {
      const Type* t = simplify_type(context, expr.type(), map);
      const Type* e = simplify_type(context, expr.code(), map);
      return TypeElaboration(t, e);
   }

   static const TagType*
   simplify(Elaborator* ctx, const TagType& t, const ParameterMap& map) {
      auto x = simplify(ctx, t.type(), map);
      return ctx->make_tag_type(t.tag(), x);
   }

   static const GenerativeType*
   simplify(Elaborator* ctx, const GenerativeType& t,
            const ParameterMap& map) {
      TypeElaboration v { ctx->get_typename(),
                          simplify_type(ctx, t.value(), map) };
      if (v.code() == t.value().code())
         return &t;
      return ctx->make_generative_type(t.name(), v, t.scope());
   }

   namespace {
      struct TypeEvaluator : Expression::Visitor {
         Elaborator* context;
         const ParameterMap& map;
         const Type* result;
         TypeEvaluator(Elaborator* c, const ParameterMap& p)
               : context(c), map(p), result()
         { }

         void visit(const Expression& x) {
            internal_error("missed evaluation of " + quote(show(&x)));
         }

         void visit(const BasicType& x) { result = &x; }

         void visit(const TagType& x) {
            result = simplify(context, x.type(), map);
         }

         void visit(const ReferenceType& x) {
            result = context->make_reference_type
               (simplify(context, x.referee(), map));
         }
         void visit(const ArrayType& x) {
            auto elem_type = simplify(context, x.elem_type(), map);
            result = context->make_array_type(elem_type);
         }
         void visit(const FixedArrayType& x) {
            auto elem_type = simplify(context, x.elem_type(), map);
            auto length = simplify(context, x.length(), map);
            result = context->make_fixed_array_type(elem_type, length);
         }
         void visit(const UintType& x) {
            // Hmm. Found a uint type.
            auto width = simplify(context, x.expr(), map);
            result = context->make_uint_type(*context->get_int(),
                                             *is<Value>(width.code()));
         }
         void visit(const RecordType& x) {
            Sequence<TagType> fields;
            for (auto f : x.components())
               fields.push_back(simplify(context, *f, map));
            result = context->make_record_type(fields);
         }
         void visit(const VariantType& x) {
            result = &x;
         }
         void visit(const ArrowType& x) {
            TypeElaboration ret = simplify(context, x.target(), map);
            InputTypes args(x.arity());
            for (std::size_t i = 0; i < x.arity(); ++i)
               args[i] = simplify(context, x.argument(i), map);
            result = context->make_arrow_type(ret, args);
         }
         void visit(const ProductType& x) {
            auto target = simplify(context, x.target(), map);
            result = context->make_product_type(x.source(), target,
                                                x.restriction());
         }
         void visit(const ReadonlyType& x) {
            result = context->make_readonly_type
               (simplify(context, x.type(), map));
         }
         void visit(const TypeExpression& x) {
            Elaboration expr = simplify(context, x.expr(), map);
            if (const Type* t = is<Type>(expr.code()))
               result = t;
            else
               result = context->make_type_expression(expr);
         }
         void visit(const QuantifiedType& x) {
            result = context->make_quantified_type
               (x.quantifier(), x.formals(),
                simplify(context, x.abstract_instance(), map), x.constraint());
         }

         void visit(const RestrictedType& x) {
            auto t = simplify(context, x.type(), map);
            auto c = simplify(context, x.condition(), map);
            if (t != x.type() or c != x.condition())
               result = context->make_restricted_type(t, c);
            else
               result = &x;
         }

         void visit(const GenerativeType& x) {
            result = simplify(context, x, map);
         }
      };
   }

   static const Type*
   simplify_type(Elaborator* context, const Type* t,
                 const ParameterMap& map) {
      if (t == nullptr)
         return nullptr;
      TypeEvaluator v(context, map);
      t->accept(v);
      return v.result;
   }

   static LinkName
   simplify(Elaborator* ctx, const LinkName& n, const ParameterMap& map) {
      return { n.name(), simplify_type(ctx, n.type(), map) };
   }

   static Elaboration
   simplify(Elaborator*, Elaboration, const ParameterMap&);

   inline FunctionElaboration
   simplify(Elaborator* ctx, FunctionElaboration fun, const ParameterMap& map) {
      auto e = simplify(ctx, (Elaboration)fun, map);
      if (auto t = is<ArrowType>(e.type()))
         return { t, e.code() };
      return { is<ProductType>(e.type()), e.code() };
   }

   static Arguments
   simplify_args(Elaborator* ctx, const Arguments& args,
                 const ParameterMap& map)
   {
      const std::size_t n = args.size();
      Arguments xargs(n);
      for(std::size_t i = 0; i != n; ++i)
         xargs[i] = simplify(ctx, args[i], map);
      return xargs;
   }

   inline bool non_null(const void* ptr) { return ptr != nullptr; }
   inline const Value* to_value(Elaboration e) { return is<Value>(e); }

   static Elaboration
   realize_instance(Elaborator* ctx, const Instance& inst) {
      if (inst.constructor() == nullptr)
         internal_error("seriously? Found an instance with no constructor.");
      const Lambda* lam = nullptr;
      lam = is<Lambda>(inst.constructor()->implementation().code());
      if (lam == nullptr)
         return { };
      Substitution subst;
      for (std::size_t i = 0; i != lam->formals().size(); ++i)
         subst[lam->formals()[i]] =
            { lam->formals()[i]->type(), inst.arguments()[i] };
      if (not check_subst_is_closed(subst))
         return { };
      // FIXME: Need a way to handle recursive types.
      if (is<VariantType>(lam->body().code()))
         return { };
      return subst_expr(ctx, lam->body(), subst);
   }

   static const Type*
   get_target_type(const Type* t) {
      if (auto arrow_t = is<ArrowType>(t))
         return arrow_t->target().code();
      else if (auto prod_t = is<ProductType>(t))
         return prod_t->target().code();
      else
         return nullptr;
   }

   static const Instance*
   simplify_to_instance(Elaborator& ctx, const Constructor& ctor,
                        const Arguments args, const Type& t)
   {
      // Uses a different framework.
      if (&t == ctx.get_concept())
         return nullptr;
      Sequence<Value> vals(args.size());
      transform(args.begin(), args.end(), vals.begin(), to_value);
      if (all_of(vals.begin(), vals.end(), non_null)) {
         auto inst = ctx.build_instance(&ctor, vals);
         realize_instance(&ctx, *inst);
         ctx.remember_specialization({ &t, inst });
         return inst;
      }
      return nullptr;
   }

   static const Expression*
   simplify_ctor_call(Elaborator* ctx, const Constructor* ctor,
                      const Arguments& args)
   {
      auto ft = ctor->type();
      const Type* target_t = get_target_type(ft);
      if (target_t == nullptr)
         internal_error("call to enumerator " + quote(show(ctor->name())));
      // If can be reduced to an instance...
      if (auto inst = simplify_to_instance(*ctx, *ctor, args, *target_t))
         return inst;
      // Otherwise, keep this in weak-head normal form.
      else {
         if (auto t = is<ArrowType>(ctor->type()))
            return ctx->build_call({ t, ctor }, args);
         else
            return ctx->build_call({ is<ProductType>(ctor->type()),ctor },args);
      }
   }

   static Elaboration
   read_component(const Component* x) {
      if (auto ns = is<Namespace>(x->whole())) {
         auto d = ns->select(x->name(), x->type());
         if (d != nullptr and d->value().code() != nullptr)
            return d->value();
      }
      return { };
   }
      
   namespace {
      // Our evaluators are recursive, so they do not always maintain explicit
      // context stack of all calls.  When executing a 'return'-statement
      // contained in the input source code, we need some form of 
      // 'non-local' goto to jump to the call instance of 'eval()' that
      // initiated the evaluation of a function that contains the return
      // statement.  Exceptions of this class perform that non-local goto.
      template<typename T>
      struct ReturnJump {
         T value;
         explicit ReturnJump(T v) : value(v) { }
      };

      template<typename T>
      struct LeaveJump {
         const T value;
         explicit LeaveJump(const T& t) : value(t) { }
      };

      template<typename T>
      struct ThrowJump {
         const T value;
         explicit ThrowJump(const T& t) : value(t) { }
      };
   }

   static bool
   are_nonsymbolic_values(const Arguments& args) {
      for (auto arg: args)
         if (is<SymbolicValue>(arg.code()) or not is<Value>(arg.code()))
            return false;
      return true;
   }

   // True when an intrinsic refers to a hard-coded expression.
   // For example, the `ref` intrinsic must be reduced to `ReferenceType`.
   static bool
   intrinsic_is_expresion(const Name* n) {
      if (auto name = n) {
         const auto& str = name->symbol().string();
         return str == "uint" or str == "ref";
      }
      return false;
   }

   static inline bool
   should_simplify_builtin(const Name* n, const Arguments& args) {
      return intrinsic_is_expresion(n) or are_nonsymbolic_values(args);
   }

   static Elaboration
   simplify_builtin_function(Elaborator& ctx, const UnaryBuiltinFunction& fun,
                             const Arguments& args)
   {
      if (not should_simplify_builtin(fun.name(), args))
         return { };
      auto res = Data::Location(fun.code()(&ctx, fun.params(),
            unreify({ args[0].type(), args[0].code() }).value()));
      auto target_t = fun.type()->target().code();
      return { target_t, reify(&ctx, { target_t, res }) };
   }

   static Elaboration
   simplify_builtin_function(Elaborator& ctx, const BinaryBuiltinFunction& fun,
                             const Arguments& args)
   {
      if (not should_simplify_builtin(fun.name(), args))
         return { };
      auto res = Data::Location(fun.code()(&ctx, fun.params(),
         unreify({ args[0].type(), args[0].code() }).value(),
         unreify({ args[1].type(), args[1].code() }).value()));
      auto target_t = fun.type()->target().code();
      return { target_t, reify(&ctx, { target_t, res }) };
   }

   static Elaboration
   simplify_builtin_function(Elaborator& ctx, const TernaryBuiltinFunction& fun,
                             const Arguments& args)
   {
      if (not should_simplify_builtin(fun.name(), args))
         return { };
      auto res = Data::Location(fun.code()(&ctx, fun.params(),
         unreify({ args[0].type(), args[0].code() }).value(),
         unreify({ args[1].type(), args[1].code() }).value(),
         unreify({ args[2].type(), args[2].code() }).value()));
      auto target_t = fun.type()->target().code();
      return { target_t, reify(&ctx, { target_t, res }) };
   }

   static Elaboration
   simplify_builtin_function(Elaborator& ctx,
                             const DependentTernaryBuiltinFunction& fun,
                             const Arguments& args)
   {
      if (not are_nonsymbolic_values(args))
         return { };
      auto res = Data::Location(fun.code()(&ctx,
         unreify({ args[0].type(), args[0].code() }).value(),
         unreify({ args[1].type(), args[1].code() }).value(),
         unreify({ args[2].type(), args[2].code() }).value()));
      auto target_t = fun.type()->target().code();
      return { target_t, reify(&ctx, { target_t, res }) };
   }

   static Elaboration
   simplify_builtin_function(Elaborator* ctx, FunctionElaboration fun,
                             const Arguments& args)
   {
      if (auto f = is_unary(fun.code()))
         return simplify_builtin_function(*ctx, *f, args);
      else if (auto f = is_binary(fun.code()))
         return simplify_builtin_function(*ctx, *f, args);
      else if (auto f = is_ternary(fun.code()))
         return simplify_builtin_function(*ctx, *f, args);
      else if (auto f = is_dep_ternary(fun.code()))
         return simplify_builtin_function(*ctx, *f, args);
      else
         return { };
   }

   static Elaboration
   simplify_lambda_call(Elaborator* ctx, const Lambda* f, const Arguments& x) {
      ParameterMap new_map;
      const auto nargs = x.size();
      for (std::size_t i = 0; i < nargs; ++i)
         new_map[f->parameter(i)->symbol()] = x.at(i).code();
      try {
         return simplify(ctx, f->body(), new_map);
      }
      catch(ReturnJump<Elaboration> ret) {
         auto result = ret.value;
         if (is_closed(result))
            result.code(reify(ctx, ctx->eval(result)));
         return result;
      }
   }

   static const Expression*
   short_circuit_conjunction(Elaborator* ctx, Elaboration lhs, Elaboration rhs)
   {
      if (auto b = is<Bool>(lhs.code())) {
         if (b->rep() == true) return rhs.code();
         else                  return b;
      }
      if (auto b = is<Bool>(rhs.code())) {
         if (b->rep() == true) return lhs.code();
         else                  return b;
      }
      return ctx->build_logical(logical::conjunction, lhs, rhs);
   }

   static const Expression*
   short_circuit_disjunction(Elaborator* ctx, Elaboration lhs, Elaboration rhs)
   {
      if (auto b = is<Bool>(lhs.code())) {
         if (b->rep() == true) return b;
         else                  return rhs.code();
      }
      if (auto b = is<Bool>(rhs.code())) {
         if (b->rep() == true) return b;
         else                  return lhs.code();
      }
      return ctx->build_logical(logical::disjunction, lhs, rhs);
   }

   // Short circuit will attempt to evaluate a boolean with only one operand
   // if it is available.
   static const Expression*
   short_circuit_binary_logical(Elaborator* ctx, logical::Operation op,
                                Elaboration lhs, Elaboration rhs) {
      switch (op) {
         case logical::disjunction:
            return short_circuit_disjunction(ctx, lhs, rhs);
         case logical::conjunction:
            return short_circuit_conjunction(ctx, lhs, rhs);
         case logical::implication:
         case logical::equivalence:
            return ctx->build_logical(op, lhs, rhs);
      }
      return { };
   }

   static const Lambda*
   is_simple_lambda(FunctionElaboration fun) {
      if (auto l = is<Lambda>(fun)) {
         if (l->body() == Elaboration() or not is_simple({ l->type(), l }))
            return nullptr;
         return l;
      }
      return nullptr;
   }

   static AssocArguments
   simplify(Elaborator& ctx, const AssocArguments& assoc_args,
            const ParameterMap& map)
   {
      const std::size_t n = assoc_args.size();
      AssocArguments args(n);
      for (std::size_t i = 0; i != n; ++i)
         args[i] = { assoc_args[i].first,
                     simplify(&ctx, assoc_args[i].second, map) };
      return args;
   }

   static const Record*
   reduce_to_record(Elaborator& ctx, const AssocArguments& args) {
      const std::size_t n = args.size();
      Record* rec = ctx.build_record();
      for (std::size_t i = 0; i!= n; ++i) {
         if (auto v = is<Value>(args[i].second.code()))
            rec->bind(args[i].first, unreify({ args[i].second.type(), v }));
         else
            return nullptr;
      }
      return rec;
   }

   static Data::Value
   to_data_value(Elaboration e) {
      return unreify({ e.type(), e.code() }).value();
   }

   namespace {
      struct SimpleEvalVisitor : Expression::Visitor {
         Elaborator* context;
         const ParameterMap& map;
         Elaboration result;
         SimpleEvalVisitor(Elaborator* c, Elaboration e,
                           const ParameterMap& m)
               : context(c), map(m), result(e) { }

         void visit(const Expression& x) {
            // Conservatively assume that the result would  be `expr'.
            result.code(&x);
         }

         void visit(const Type& x) {
            result.code(simplify_type(context, &x, map));
         }

         void visit(const Component& x) {
            auto whole = simplify(context, x.whole(), map);
            auto lnk = simplify(context, x.link_name(), map);
            result.code(context->build_component(whole, lnk));
         }

         void visit(const Read& x) {
            Elaboration loc = simplify(context, x.address(), map);
            if (auto ref = is<Component>(loc)) {
               if (auto read_comp = read_component(ref))
                 result = read_comp;
            }
            else if (auto lnk = is<LinkName>(loc)) {
               if (map.is_defined_at(lnk->symbol()))
                  result.code(map(lnk->symbol()));
               else if (auto d = context->select_if_can(lnk->name(), lnk->type()))
                  result.code(d->value().code());
            }
            else if (auto f = is<Formal>(loc)) {
               if (map.is_defined_at(f->link_name().symbol()))
                  result.code(map(f->link_name().symbol()));
               else
                  result.code(context->build_read(loc));
            }
            else if (loc != x.address())
               result.code(context->build_read(loc));
         }
         
         void visit(const Negate& x) {
            result.code
               (context->build_negate
                (x.function(), simplify(context, x.argument(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }
         
         void visit(const Not& x) {
            result.code
               (context->build_not
                (x.function(), simplify(context, x.argument(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }
         
         void visit(const Complement& x) {
            result.code
               (context->build_complement
                (x.function(), simplify(context, x.argument(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }
         
         void visit(const Plus& x) {
            result.code
               (context->build_plus
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Dash& x) {
            result.code
               (context->build_dash
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Star& x) {
            result.code
               (context->build_star
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Slash& x) {
            result.code
               (context->build_slash
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Div& x) {
            result.code
               (context->build_div
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Quo& x) {
            result.code
               (context->build_quo
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Mod& x) {
            result.code
               (context->build_mod
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Rem& x) {
            result.code
               (context->build_rem
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Langle& x) {
            result.code
               (context->build_langle
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Rangle& x) {
            result.code
               (context->build_rangle
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Langleq& x) {
            result.code
               (context->build_langleq
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Rangleq& x) {
            result.code
               (context->build_rangleq
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Eqeq& x) {
            result.code
               (context->build_eqeq
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Excleq& x) {
            result.code
               (context->build_excleq
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const And& x) {
            result.code
               (context->build_and
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const Or& x) {
            result.code
               (context->build_or
                (x.function(), simplify(context, x.lhs(), map),
                 simplify(context, x.rhs(), map)));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }

         void visit(const BinaryLogical& x) {
            auto lhs = simplify(context, x.lhs(), map);
            auto rhs = simplify(context, x.rhs(), map);
            result.code(
               short_circuit_binary_logical(context, x.operation(), lhs, rhs));
            if (is_closed(result))
               result.code(reify(context, context->eval(result)));
         }
         
         void visit(const CallExpression& x) {
            auto fun = simplify(context, x.function(), map);
            auto args = simplify_args(context, x.arguments(), map);
            if (auto f = is<Constructor>(fun))
               result.code(simplify_ctor_call(context, f, args));
            else if (auto e = simplify_builtin_function(context, fun, args))
               result = e;
            else if (auto f = is_simple_lambda(fun))
               result = simplify_lambda_call(context, f, args);
            else
               result.code(context->build_call(fun, args));
         }
         
         void visit(const Return& x) {
            throw ReturnJump<Elaboration>
               (simplify(context, x.expression(), map));
         }

         void visit(const Loop& x) {
            try {
               result = simplify(context, x.body(), map);
            }
            catch(LeaveJump<Elaboration> e) {
               result = e.value;
            }
         }
         
         void visit(const Block& x) {
            // Note: We get here only when `x' has at most one statement.
            for (int i = 0; i < x.size(); ++i)
               result = simplify(context, x.statement(i), map);
         }

         void visit(const Initializer& x) {
            auto assoc_args = simplify(*context, x, map);
            if (auto rec = reduce_to_record(*context, assoc_args))
               result.code(rec);
            else
               result.code(context->build_initializer(assoc_args));
         }

         void visit(const InitializerList& x) {
            auto args = simplify_args(context, x, map);
            if (all_of(args.begin(), args.end(), to_value)) {
               Data::Array arr(args.size());
               transform(args.begin(), args.end(), arr.begin(), to_data_value);
               result.code(context->build_array(arr, result.type()));
            } else
               result.code(context->build_initializer_list(args));
         }
      };
   }
      
   static Elaboration
   simplify(Elaborator* context, Elaboration expr, const ParameterMap& map) {
      const Type* t = simplify_type(context, expr.type(), map);
      if (expr.code() == nullptr)
         return make_elaboration(t, nullptr);
      SimpleEvalVisitor v(context, make_elaboration(t, expr.code()), map);
      expr.code()->accept(v);
      return v.result;
   }
   
   Elaboration
   evaluate(Elaborator* context, Elaboration expr) {
      return simplify(context, expr, ParameterMap());
   }

   TypeElaboration
   evaluate(Elaborator* context, TypeElaboration texpr) {
      return simplify(context, texpr, ParameterMap());
   }

   // ------------------------
   // -- is_local predicate --
   // ------------------------

   namespace locality_pred {

      struct Locals {
         Formals fs;
         std::vector<LinkName> lns;

         Locals() : fs(), lns() { }

         void push_back(const Formal& f) { fs.push_back(&f); }
         void push_back(const LinkName& n) { lns.push_back(n); }

         bool has(const Formal& f) {
            return std::find(fs.begin(), fs.end(), &f) != fs.end();
         }

         bool has(const LinkName& n) {
            for (auto ln: lns)
               if (n.name() == ln.name() and n.type() == ln.type())
                  return true;
            return false;
         }
      };

      inline bool is_global_constant(Elaborator& ctx, const Expression& e) {
         if (auto cmpt = is<Component>(&e))
            if (auto ns = is<Namespace>(cmpt->whole().code()))
               if (ns == ctx.global_scope())
                  return is<ReadonlyType>(cmpt->type());
         return false;
      }

      bool is_local(Elaborator&, const Locals&, Elaboration);

      static bool
      is_local(Elaborator& ctx, const Locals& ls, const Arguments& seq) {
         bool result = true;
         for (std::size_t i = 0; result and i < seq.size(); ++i)
            result = is_local(ctx, ls, seq[i]);
         return result;
      }

      struct IsLocalVisitor : Expression::Visitor {
         bool result;
         Elaborator& ctx;
         Locals locals;
         IsLocalVisitor(Elaborator& ctx, const Locals& ls)
               : result(true), ctx(ctx), locals(ls) { }

         inline bool
         local(Elaboration e) { return is_local(ctx, locals, e); }

         void visit(const Expression& e) {
            internal_error("alien expression in `local`: " + show_expr(&e));
         }
         void visit(const Value&) { }
         void visit(const InitializerList& x) {
            for (auto elt: x)
               if (not local(elt)) {
                  result = false;
                  return;
               }
         }
         void visit(const NiladicBuiltinFunction&) { }
         void visit(const BinaryBuiltinFunction&) { }
         void visit(const TernaryBuiltinFunction&) { }
         void visit(const Type&) { }
         void visit(const Lambda& x) {
            for (auto formal: x.formals())
               locals.push_back(*formal);
            result = local(x.body());
         }
         void visit(const Block& x) {
            for (auto arg: x.statements())
               if (not local(arg)) {
                  result = false;
                  return;
               }
         }
         void visit(const UnaryExpression& x) {
            result = local(x.argument());
         }
         void visit(const BinaryExpression& x) {
            result = local(x.lhs()) and local(x.rhs());
         }
         void visit(const BinaryLogical& x) {
            result = local(x.lhs()) and local(x.rhs());
         }
         void visit(const CallExpression& x) {
            if (is_local(ctx, locals, x.arguments())) {
               if (local(x.function()))
                  return;
               if (ctx.assumptions().is_local(x.function()))
                  return;
            }
            result = false;
         }
         void visit(const BindExpression& x) {
            locals.push_back(x.link_name());
            result = local(x.initializer());
         }
         void visit(const Loop& x) {
            result = local(x.body());
         }
         void visit(const IfExpression& x) {
            result = local(x.condition())
                 and local(x.consequence())
                 and local(x.alternative());
         }
         void visit(const Component& x) {
            if (not is_global_constant(ctx, x))
               result = false;
         }
         void visit(const Read& x) { result = local(x.address()); }
         void visit(const Instance& x) {
            for (auto arg: x.arguments())
               if (not local({ nullptr, arg }))
                  result = false;
         }
         void visit(const Formal& x) {
            if (not locals.has(x))
               result = false;
         }
         void visit(const LinkName& x) {
            if (not locals.has(x))
               result = false;
         }
         void visit(const Initializer& x) {
            for (auto arg: x)
               if (not local(arg.second)) {
                  result = false;
                  return;
               }
         }
      };

      bool is_local(Elaborator& ctx, const Locals& ls, Elaboration e) {
         IsLocalVisitor v(ctx, ls);
         e.code()->accept(v);
         return v.result;
      }

      bool is_constexpr(Elaborator&, const Locals&, Elaboration);

      static bool
      is_constexpr(Elaborator& ctx, const Locals& ls, const Arguments& seq) {
         bool result = true;
         for (std::size_t i = 0; result and i < seq.size(); ++i)
            result = is_constexpr(ctx, ls, seq[i]);
         return result;
      }

      struct IsConstExprVisitor : Expression::Visitor {
         bool result;
         Elaborator& ctx;
         Locals locals;
         IsConstExprVisitor(Elaborator& ctx, const Locals& ls)
               : result(true), ctx(ctx), locals(ls) { }

         inline bool
         is_ce(Elaboration e) { return is_constexpr(ctx, locals, e); }

         void visit(const Expression& e) {
            internal_error("alien expression in `is_constexpr`: "
                           + to_string(&e));
         }
         void visit(const Value&) { }
         void visit(const InitializerList& x) {
            for (auto elt: x)
               if (not is_ce(elt)) {
                  result = false;
                  return;
               }
         }
         void visit(const NiladicBuiltinFunction&) { }
         void visit(const BinaryBuiltinFunction&) { }
         void visit(const TernaryBuiltinFunction&) { }
         void visit(const Type&) { }
         void visit(const Lambda& x) {
            for (auto formal: x.formals())
               locals.push_back(*formal);
            result = is_ce(x.body());
         }
         void visit(const Block& x) {
            for (auto arg: x.statements())
               if (not is_ce(arg)) {
                  result = false;
                  return;
               }
         }
         void visit(const UnaryExpression& x) {
            result = is_ce(x.argument());
         }
         void visit(const BinaryExpression& x) {
            result = is_ce(x.lhs()) and is_ce(x.rhs());
         }
         void visit(const BinaryLogical& x) {
            result = is_ce(x.lhs()) and is_ce(x.rhs());
         }
         void visit(const CallExpression& x) {
            if (is_constexpr(ctx, locals, x.arguments())) {
               if (is_local(ctx, locals, x.function()))
                  return;
               if (ctx.assumptions().is_local(x.function()))
                  return;
            }
            result = false;
         }
         void visit(const Component& x) {
            if (not is_global_constant(ctx, x))
               result = false;
         }
         void visit(const Read& x) { result = is_ce(x.address()); }
         void visit(const Instance& x) {
            for (auto arg: x.arguments())
               if (not is_ce({ nullptr, arg }))
                  result = false;
         }
         void visit(const Formal&) { result = false; }
         void visit(const Initializer& x) {
            for (auto arg: x)
               if (not is_ce(arg.second)) {
                  result = false;
                  return;
               }
         }
      };

      bool is_constexpr(Elaborator& ctx, const Locals& ls, Elaboration e) {
         IsConstExprVisitor v(ctx, ls);
         e.code()->accept(v);
         return v.result;
      }

   }

   bool is_local(Elaborator& ctx, Elaboration e) {
      return locality_pred::is_local(ctx, { }, e);
   }

   bool is_constexpr(Elaborator& ctx, Elaboration e) {
      return locality_pred::is_constexpr(ctx, { }, e);
   }


   // --------------------------------
   // -- Elaboration simplification --
   // --------------------------------

   static bool
   is_closed(const Arguments& seq) {
      bool result = true;
      for (std::size_t i = 0; result and i < seq.size(); ++i)
         result = is_closed(seq[i]);
      return result;
   }
   
   namespace {
      struct IsClosedVisitor : Expression::Visitor {
         bool result;
         std::vector<const Formal*> formals;
         IsClosedVisitor() : result(false), formals() { }

         void visit(const Expression&) { }
         void visit(const Formal&) { }
         void visit(const SymbolicValue&) { result = false; }
         void visit(const Value&) { result = true; }
         void visit(const NiladicBuiltinFunction&) { result = true; }
         void visit(const BinaryBuiltinFunction&) { result = true; }
         void visit(const TernaryBuiltinFunction&) { result = true; }
         void visit(const Type&) { result = true; }
         void visit(const ReferenceType& x) {
            result = is_closed(x.referee());
         }
         void visit(const ArrowType& x) {
            result = is_closed(x.target());
            for (std::size_t i = 0; i < x.arity() and result; ++i)
               result = is_closed(x.argument(i));
         }
         void visit(const ReadonlyType& x) {
            result = is_closed(x.type());
         }
         void visit(const TypeExpression& x) {
            result = is_closed(x.expr());
         }
         void visit(const ArrayType& x) {
            result = is_closed(x.elem_type());
         }
         void visit(const Lambda& x) {
            // We consider an undefined function as not closed.
            // FIXME: do we treat captures?
            result = x.body() != Elaboration();
         }

         void visit(const UnaryExpression& x) {
            result = is_closed(x.argument());
         }

         void visit(const BinaryExpression& x) {
            result = is_closed(x.lhs()) and is_closed(x.rhs());
         }

         void visit(const BinaryLogical& x) {
            result = is_closed(x.lhs()) and is_closed(x.rhs());
         }

         void visit(const CallExpression& x) {
            result = is_closed(x.function()) and is_closed(x.arguments());
         }

         void visit(const Component& x) {
            result = is_closed(x.whole());
         }
         void visit(const Read& x) {
            result = is_closed(x.address());
         }

         void visit(const Instance& x) {
            for (auto arg: x.arguments())
               if (not is_closed({ nullptr, arg })) {
                  result = false;
                  return;
               }
            result = true;
         }
      };
   }

   bool
   is_closed(Elaboration expr) {
      if (expr.code() == nullptr)
         return true;
      IsClosedVisitor v;
      expr.code()->accept(v);
      return v.result;
   }

   // -- CallFrame --
   CallFrame::CallFrame(FunctionElaboration f)
         : Store(dynamic), fun(f)
   { }

   Store*
   CallFrame::push_store() {
      st.push_back(Store(Contour::lexical));
      return &st.back();
   }

   void
   CallFrame::pop_store() {
      st.pop_back();
   }

   const Data::Value*
   CallFrame::address(Symbol name) const {
      auto decls = lookup(name);
      switch (decls.size()) {
      case 1: return &decls.begin()->value().value();
      case 0: return nullptr;
      default:
         internal_error("multiple bindings for parameter " + quote(name));
         return nullptr;
      }
   }

   // -- ControlStack --
   void
   ControlStack::push(FunctionElaboration f) {
      push_back(CallFrame(f));
   }

   void
   ControlStack::pop() {
      pop_back();
   }

   // ------------------------
   // -- Runtime evaluation --
   // ------------------------

   // -- stop evaluation and signal a runtime assertion failure.
   static void
   assertion_failure() {
      throw BasicError("runtime assertion failure");
   }

   // -- Evaluator --
   Evaluator::Evaluator(Reader& r)
         : Elaborator(r) {
      get_store()->push(global_scope()->store());
      push_call_frame({ });
   }

   Evaluator::~Evaluator()
   { }

   CallFrame*
   Evaluator::push_call_frame(FunctionElaboration f) {
      ctrl.push(f);
      return &ctrl.top();
   }

   void
   Evaluator::pop_call_frame() {
      ctrl.pop();
   }

   CallFrame*
   Evaluator::current_call_frame() {
      if (ctrl.empty())
         return nullptr;
      return &ctrl.top();
   }

   static Data::Value
   evaluate(Evaluator*, const Postulate& x) {
      free_variable_error("unrealized postulate", x.name()->symbol());
      return { };
   }

   static Data::Value
   evaluate(Evaluator* env, const TypeExpression& x) {
      return env->eval(x.expr()).value();
   }

   // Evaluate a reference to a namespace member.
   static Data::Value
   evaluate(Evaluator* env, const Component& x) {
      auto s = Data::to_namespace(env->eval(x.whole()).value())->store();
      Symbol n = x.symbol();
      auto var = s->select(n, x.type());
      return Data::Value(&var->value().value());
   }

   static Data::Value
   evaluate(Evaluator* env, const DotSelection& x) {
      auto s = Data::to_record(env->eval(x.whole()).value());
      auto var = s->select(x.symbol(), x.type());
      return Data::Value(&var->value().value());
   }
   
   // Evaluate a read expression
   static Data::Value
   evaluate(Evaluator* env, const Read& x) {
      Data::Value loc = env->eval(x.address()).value();
      return *Data::to_value_location(loc);
   }

   // Return the runtime entity, if any, found by lexical
   // lookup of a symbolic reference.
   static Variable*
   lexical_or_global_var(Evaluator* env, const LinkName& x) {
      auto n = x.symbol();
      if (auto var = env->get_store()->select_if_can(n, x.type()))
         return var;
      return env->global_scope()->store()->select(n, x.type());
   }
   
   // Evaluate a symbolic reference to an entity.
   static Data::Value
   evaluate(Evaluator* env, const LinkName& x) {
      auto var = lexical_or_global_var(env, x);
      if (var == 0)
         evaluation_error("there is no binding of symbol "
                          + quote(x.symbol()));
      return Data::Value(&var->value().value());
   }

   // Evaluate argument lists in a call.
   static Data::Value
   evaluate(Evaluator* env, const Arguments& args) {
      vector<Data::Value> vals;
      for (auto& arg : args)
         vals.push_back(env->eval(arg).value());
      return env->intern_array(vals);
   }

   static vector<Object>
   evaluate_arguments(Evaluator* env, const Arguments& args) {
      vector<Object> vals;
      for (auto& arg : args)
         vals.push_back(env->eval(arg));
      return vals;
   }

   static Data::Value
   evaluate(Evaluator* env, const AssocArguments& x) {
      const std::size_t n = x.size();
      Record* v = env->build_record();
      for (std::size_t i = 0; i < n; ++i)
         v->bind(x[i].first, env->eval(x[i].second));
      return Data::Value(v);
   }

   namespace {
      struct StoreManager {
         StoreManager(Evaluator* e, Store* s)
               : env(e) {
            env->get_store()->push(s);
         }
         ~StoreManager() { env->get_store()->pop(); }

         StoreRef operator->() { return env->get_store()->top(); }
      protected:
         Evaluator* const env;
      };

      struct LocalStoreManager : StoreManager {
         explicit LocalStoreManager(Evaluator* e)
               : StoreManager(e, e->current_call_frame()->push_store())
         { }

         ~LocalStoreManager() {
            env->current_call_frame()->pop_store();
         }
      };

      struct CallFrameManager : StoreManager {
         CallFrameManager(Evaluator* e, FunctionElaboration f)
               : StoreManager(e, e->push_call_frame(f))
         { }

         ~CallFrameManager() {
            env->pop_call_frame();
         }
      };
   }

   static Data::Value
   evaluate(Evaluator* env, CallFrameManager& frame,
            const Lambda* f, const vector<Object>& args) {
      // 1. Bind parameters to their values.
      const auto nargs = args.size();
      for (std::size_t i = 0; i < nargs; ++i)
         if (auto parm = f->parameter(i)->name())
            frame->bind(parm->symbol(), args[i]);
      // 2. Evaluate the body of the function.
      try {
         return env->eval(f->body()).value();
      }
      catch (ReturnJump<Data::Value> ret) {
         return ret.value;
      }
   }

   static Data::Value
   evaluate(Evaluator* env, const Constructor* ctor,
            const vector<Object>& args) {
//      auto ftype = is<ArrowType>(ctor->type());
      Sequence<Value> vals(args.size());
      for (std::size_t i = 0; i < args.size(); ++i) {
         vals[i] = reify(env, args[i]);
      }
      return Data::Value(env->build_instance(ctor, vals));
   }

   static vector<Object>
   reify_args(Evaluator* env, vector<Object> args) {
      auto ctx = static_cast<Elaborator*>(env);
      for (std::size_t i = 0; i != args.size(); ++i)
         args[i] = { args[i].type(), reify(ctx, args[i]) };
      return args;
   }

   static bool
   codomain_is_typename(Evaluator* env, const Type* t) {
      if (auto at = is<ArrowType>(t))
         return at->target().code() == env->get_typename();
      else if (auto at = is<ProductType>(t))
         return at->target().code() == env->get_typename();
      internal_error("trying to check the codomain of a non-function type");
      return false;
   }

   // Evaluate function call.
   static Data::Value
   evaluate(Evaluator* env, const CallExpression& x) {
      // 1. Figure out what to call.
      auto f = Data::to_function(env->eval(x.function()).value());
      // 2. Evaluate arguments in the caller's context.
      vector<Object> args = evaluate_arguments(env, x.arguments());
      if (codomain_is_typename(env, x.function().type()))
         args = reify_args(env, args);
      // 3. Establish new call frame.
      CallFrameManager frame(env, x.function());
      if (auto fun = is<NiladicBuiltinFunction>(f))
         return fun->code()(env);
      else if (auto fun = is<UnaryBuiltinFunction>(f))
         return fun->code()(env, fun->params(), args[0].value());
      else if (auto fun = is<BinaryBuiltinFunction>(f))
         return fun->code()(env, fun->params(), args[0].value(),
                            args[1].value());
      else if (auto fun = is<TernaryBuiltinFunction>(f))
         return fun->code()(env, fun->params(), args[0].value(),
                            args[1].value(), args[2].value());
      else if (auto fun = is<DependentNiladicBuiltinFunction>(f))
         return fun->code()(env);
      else if (auto fun = is<DependentUnaryBuiltinFunction>(f))
         return fun->code()(env, args[0].value());
      else if (auto fun = is<DependentBinaryBuiltinFunction>(f))
         return fun->code()(env, args[0].value(), args[1].value());
      else if (auto fun = is<DependentTernaryBuiltinFunction>(f))
         return fun->code()(env, args[0].value(), args[1].value(),
                            args[2].value());
      else if (auto fun = is<Lambda>(f))
         return evaluate(env, frame, fun, args);
      else if (auto ctor = is<Constructor>(f))
         return evaluate(env, ctor, args);
      internal_error("call to alien function");
      return Data::Value();     // never executed.
   }

   static Data::Value
   evaluate(Evaluator* env, const Assertion& x) {
      auto f = Data::to_function(env->eval(x.predicate()).value());
      auto pred = is<Lambda>(f);
      if (pred == nullptr)
         internal_error("predicate in assertion not a lambda");
      auto arg = env->eval(x.expression());
      CallFrameManager frame(env, x.predicate());
      if (!bool(evaluate(env, frame, pred, { arg })))
         // FIXME: provide more source-level context.
         assertion_failure();
      return arg.value();
   }

   static Data::Value
   evaluate(Evaluator* env, const Formal& x) {
      auto frame = env->current_call_frame();
      auto loc = frame->address(x.symbol());
      if (loc == nullptr)
         internal_error("unbound parameter " + quote(x.symbol())
                        + " in function "
                        + quote(show_expr(frame->function().code())));
      return Data::Value(loc);
   }

   static Data::Value
   evaluate(Evaluator* env, const Block& x) {
      LocalStoreManager new_scope(env);
      Data::Value v { };
      for (auto& s : x.statements())
         v = env->eval(s).value();
      return v;
   }

   static Data::Value
   evaluate(Evaluator* env, const Write& x) {
      Object rhs = env->eval(x.value());
      Object place = env->eval(x.location());
      *Data::to_value_location(place.value()) = rhs.value();
      return place.value();
   }

   static Data::Value
   evaluate(Evaluator* env, const UnaryExpression& x) {
      auto f = Data::to_function(env->eval(x.function()).value());
      Object arg = env->eval(x.argument());
      CallFrameManager frame(env, x.function());
      if (auto fun = is<UnaryBuiltinFunction>(f))
         return fun->code()(env, fun->params(), arg.value());
      else if (auto fun = is<Lambda>(f))
         return evaluate(env, frame, fun, { arg });
      else
         internal_error("call to alien unary operator");
      return { };
   }

   static Data::Value
   evaluate(Evaluator* env, const BinaryExpression& x) {
      auto f = Data::to_function(env->eval(x.function()).value());
      Object arg0 = env->eval(x.lhs());
      Object arg1 = env->eval(x.rhs());
      CallFrameManager frame(env, x.function());
      if (auto fun = is<BinaryBuiltinFunction>(f))
         return fun->code()(env, fun->params(), arg0.value(), arg1.value());
      else if (auto fun = is<Lambda>(f))
         return evaluate(env, frame, fun, { arg0, arg1 }); 
      else
         internal_error("call to alien binary expression");
      return { };
   }

   static Data::Value
   evaluate(Evaluator* env, const SubstExpr& x) {
      const auto& subst = x.substitution();
      auto expr = x.expression().code();
      if (auto spec = env->retrieve_specialization(expr, subst))
         return Data::Abstract(spec);
      internal_error("substitution expression has no specialization");
      return { };
   }

   static Data::Value
   evaluate(Evaluator* env, const BinaryLogical& x) {
      Object lhs = env->eval(x.lhs());
      switch (x.operation()) {
      case logical::conjunction:
         return bool(lhs.value()) ? env->eval(x.rhs()).value() : lhs.value();
         
      case logical::disjunction:
         return bool(lhs.value()) ? lhs.value() : env->eval(x.rhs()).value();
         
      case logical::implication:
         return bool(lhs.value()) ? env->eval(x.rhs()).value() : lhs.value();

      case logical::equivalence:
         return bool(lhs.value()) == bool(env->eval(x.rhs()).value());
      }
      evaluation_error("unknown binary logical operator");
      return { };
   }

   static Data::Value
   evaluate(Evaluator* env, const Return& x) {
      throw ReturnJump<Data::Value>(env->eval(x.expression()).value());
   }

   static Data::Value
   evaluate(Evaluator* env, const Throw& x) {
      throw ThrowJump<Data::Value>(env->eval(x.expression()).value());
   }

   static Data::Value
   evaluate(Evaluator* env, const IfExpression& x) {
      return bool(env->eval(x.condition()).value())
         ? env->eval(x.consequence()).value()
         : env->eval(x.alternative()).value();
   }

   static Data::Value
   evaluate(Evaluator* env, const Loop& x) {
      while (true) {
         try {
            env->eval(x.body());
         } catch (LeaveJump<Data::Value> exit) {
            return exit.value;
         }
      }
   }

   static Data::Value
   evaluate(Evaluator* env, const Leave& x) {
      throw LeaveJump<Data::Value>(env->eval(x.expression()).value());
   }

   static Data::Value
   evaluate(Evaluator* env, const BindExpression& x) {
      auto n = x.link_name();
      auto undef = make_object(n.type(), Data::Value());
      auto place = &env->get_store()->current_env()->
         bind(n.symbol(), undef)->value().value();
      *place = env->eval(x.initializer()).value();
      return Data::Value(place);
   }

   static Data::Value
   evaluate(Evaluator* env, const Namespace& x) {
      StoreManager namespace_store(env, const_cast<Namespace&>(x).store());
      for (auto& d: x.statements()) {
         try { env->eval(d); }
         // When a definition from a former prolong is found, terminate.
         catch (const RedefinitionError&) { break; }
      }
      return x.get();
   }

   static Data::Value
   evaluate(Evaluator* env, const LetExpression& x) {
      LocalStoreManager new_scope(env);
      for (auto& d : x.locals())
         env->eval(d);
      return env->eval(x.body()).value();
   }

   static Data::Value
   evaluate(Evaluator* env, const Import& x) {
      for (auto& e : x.load_unit()->statements())
         env->eval(e);
      return Data::Value(x.load_unit());
   }

   static void
   bind_instance_to_pattern(Evaluator& env, const PatternInstance& pinst,
                            const Instance& inst)
   {
      const Formals& formals = pinst.formals();
      for (std::size_t i = 0; i != formals.size(); ++i) {
         auto obj = make_object(formals[i]->type(), inst.arguments()[i]->get());
         env.get_store()->current_env()->bind(formals[i]->symbol(), obj);
      }
   }

   static Data::Value
   evaluate(Evaluator* env, const PatternMatch& x) {
      LocalStoreManager new_scope(env);
      auto v = env->eval(x.scrutinee());
      const Expression* subj = (const Expression*)(v.value());
      for (auto clause: x.clauses()) {
         if (auto ctor = is<Constructor>(clause.pattern())) {
            if (auto subj_ctor = is<Constructor>(subj))
               if (ctor->name() == subj_ctor->name())
                  return env->eval(clause.action()).value();
         }
         else if (auto pat_inst = is<PatternInstance>(clause.pattern())) {
            if (auto subj_inst = is<Instance>(subj)) {
               if (pat_inst->constructor()->name() ==
                   subj_inst->constructor()->name())
               {
                  bind_instance_to_pattern(*env, *pat_inst, *subj_inst);
                  return env->eval(clause.action()).value();
               }
            }
         } else
            internal_error("pattern match evaluating an unsupported case "
                           "pattern");
      }
      internal_error("no case for value of pattern match");
      return Data::Value();
   }

   namespace {
      struct EvaluationVisitor : Expression::Visitor {
         Evaluator* const env;  // current evaluation environment
         Data::Value result;  // result of visiting an expression node
         explicit EvaluationVisitor(Evaluator* e)
               : env(e), result() { }
         
         void visit(const Expression& x) {
            evaluation_error("unexpected expression of type "
                             + show_cxx_type(&x));
         }
         
         void visit(const Value& x) { result = x.get(); }
         void visit(const TypeExpression& x) { result = evaluate(env, x); }
         void visit(const Postulate& x) { result = evaluate(env, x); }
         void visit(const Assertion& x) { result = evaluate(env, x); }
         void visit(const DotSelection& x) { result = evaluate(env, x); }
         void visit(const Component& x) { result = evaluate(env, x); }
         void visit(const Block& x) { result = evaluate(env, x); }
         void visit(const LinkName& x) { result = evaluate(env, x); }
         void visit(const Formal& x) { result = evaluate(env, x); }
         void visit(const Initializer& x) { result = evaluate(env, x); }
         void visit(const InitializerList& x) { result = evaluate(env, x); }
         void visit(const LetExpression& x) { result = evaluate(env, x); }
         void visit(const Namespace& x) { result = evaluate(env, x); }
         void visit(const Read& x) { result = evaluate(env, x); }
         void visit(const Write& x) { result = evaluate(env, x); }
         void visit(const UnaryExpression& x) { result = evaluate(env, x); }
         void visit(const BinaryExpression& x) { result = evaluate(env, x); }
         void visit(const SubstExpr& x) { result = evaluate(env, x); }
         void visit(const BinaryLogical& x) { result = evaluate(env, x); }
         void visit(const CallExpression& x) { result = evaluate(env, x); }
         void visit(const Return& x) { result = evaluate(env, x); }
         void visit(const Throw& x) { result = evaluate(env, x); }
         void visit(const IfExpression& x) { result = evaluate(env, x); }
         void visit(const Loop& x) { result = evaluate(env, x); }
         void visit(const Leave& x) { result = evaluate(env, x); }
         void visit(const BindExpression& x) { result = evaluate(env, x); }
         void visit(const PatternMatch& x) { result = evaluate(env, x); }
         void visit(const Import& x) { result = evaluate(env, x); }
         void visit(const Alias&) { result = nullptr; }
      };
   }

   Object
   Evaluator::eval(Elaboration expr) {
      // Note: We could worry that a null AST may
      // be produced by erroneous parsing or transformation, and
      // attempt to signal a problem here.  However, doing so would
      // force us to duplicate the semantics of null expression
      // statement in various places.  We find it simpler to just
      // pretend that all null AST are null expression-statement.
      const Type* t = simplify_type(this, expr.type(), ParameterMap());
      EvaluationVisitor v(this);
      if (expr.code() != nullptr)
         expr.code()->accept(v);
      return make_object(t, v.result);
   }

   void
   Evaluator::evaluate_toplevel(const Ast* ast) {
      auto e = elaborate(ast);
      try {
         Object object = eval(e);
         output() << "\n~~> " << object
                  << ": " << *object.type()
                  << std::endl;
      }
      catch (const FreeVariableError&) {
         output() << "\n~~> " << pretty(e.code())
                  << ": " << *e.type()
                  << std::endl;
      }
   }
   
   void
   Evaluator::evaluate_toplevel(const AstSequence& seq) {
      for (std::size_t i = 0; i < length(seq); ++i)
         evaluate_toplevel(seq.at(i));
   }


   namespace Queries {

      constexpr char query_symbol = '$';

      inline bool
      is_repl_query(const std::string& str) {
         return not str.empty() and str[0] == query_symbol;
      }

      inline void
      print_unrecognized_query(std::ostream& os, const std::string& str) {
         os << "unrecognized query `" << str << "`. Type `$help` for "
            << "a list of queries.\n";
      }

      std::ostream&
      print_title(std::ostream& os, const std::string& str) {
         constexpr std::size_t n = 52;
         const std::size_t str_size = str.size() + 2;
         if (str_size >= n - 2)
            os << str;
         else {
            const std::size_t buff_size = (n - str_size) / 2;
            for (std::size_t i = 0; i != buff_size; ++i)
               os << '=';
            os << ' ' << str << ' ';
            for (std::size_t i = 0; i != buff_size; ++i)
               os << '=';
         }
         return os;
      }

      void print_help(Evaluator&, std::ostream&, const std::string&);

      Elaboration
      elaborate(Evaluator& eval, const std::string& code,
                const Type* t = nullptr)
      {
         input::Source src;
         auto tokens = tokenize(src.append(code));
         auto asts = eval.reader()->parse(tokens, eval.current_flags());
         return eval.elaborate(asts.front(), t);
      }

      void
      print_scope(Evaluator& eval, std::ostream& os, const std::string& arg) {
         if (arg.empty()) {
            auto top = *eval.get_store()->begin();
            for (auto var: top)
               os << "  " << var.first << ": "
                  << to_string(var.second.type()) << '\n';
         } else {
            try {
               print_title(os, "bindings") << '\n';
               auto e = elaborate(eval, arg, eval.get_namespace());
               e = evaluate(&eval, e);
               if (auto ns = is<Namespace>(e.code())) {
                  for (auto var: *ns)
                     os << "  " << var.first->symbol() << ": "
                        << to_string(var.second.type()) << '\n';
               } else
                  os << "`$print-scope` requires a scope; got: "
                     << show_expr(e.code()) << '\n';
            } catch(const BasicError& e) {
               e.issue_on(os);
               os << std::endl;
            }
         }
      }

      void
      print_assumptions(Evaluator& eval, std::ostream& os,
                        const std::string& arg)
      {
         if (not arg.empty())
            os << "error: `$print-assumptions` takes not arguments; got `"
               << arg << "`\n";
         else {
            print_title(os, "Assumptions") << '\n';
            for (auto fact: eval.assumptions())
               os << "  " << to_string(fact.code()) << ": "
                  << to_string(fact.type()) << '\n';
            for (auto e: eval.assumptions().local_exprs)
               os << "  is_local(" << to_string(e.code()) << ")\n";
         }
      }

      void
      print_eqs(Evaluator& eval, std::ostream& os, const std::string& arg) {
         if (not arg.empty())
            os << "error: `$print-eqs` takes not arguments; got `" << arg
               << "`\n";
         else {
            print_equivalences(eval.equivalences(), os);
         }
      }

      void
      print_size(Evaluator& eval, std::ostream& os, const std::string& arg) {
         os << "  expression size: ";
         if (arg.empty())
            os << 0 << '\n';
         else {
            try {
               os << expression_size(elaborate(eval, arg).code()) << '\n';
            } catch(const BasicError& e) {
               e.issue_on(os);
               os << std::endl;
            }
         }
      }

      struct ResolutionStatsMax {
         ResolutionStatsMax(): max{0,0} { }

         void operator() (ResolutionStats s) {
            if (s.candidate_size > max.candidate_size)
               max.candidate_size = s.candidate_size;
            if (s.assumption_set_size > max.assumption_set_size)
               max.assumption_set_size = s.assumption_set_size;
         }

         ResolutionStats max;
      };

      struct FuncStatsMax {
         FuncStatsMax(): max{0,0,0} { }

         void operator() (FuncStats s) {
            if (s.arity > max.arity)
               max.arity = s.arity;
            if (s.constraint_size > max.constraint_size)
               max.constraint_size = s.constraint_size;
            if (s.top_level_conjunction_count > max.top_level_conjunction_count)
               max.top_level_conjunction_count = s.top_level_conjunction_count;
         }

         FuncStats max;
      };

      void
      print_metrics(Evaluator& eval, std::ostream& os, const std::string&) {
         ResolutionStats max_res = std::for_each(eval.stats.res_stats.begin(),
                                                 eval.stats.res_stats.end(),
                                                 ResolutionStatsMax{ }).max;
         FuncStats max_func = std::for_each(eval.stats.func_stats.begin(),
                                            eval.stats.func_stats.end(),
                                            FuncStatsMax{ }).max;
         std::size_t ctx_size = (*eval.get_store()->begin())->size();
         os << "Metrics:\n"
            << "   context size:         " << ctx_size << '\n'
            << "   max candidate count:  " << max_res.candidate_size << '\n'
            << "   max assumption count: " << max_res.assumption_set_size <<'\n'
            << "   max function arity:   " << max_func.arity << '\n'
            << "   max constraint size:  " << max_func.constraint_size << '\n'
            << "   max cst conjuncts:     "
               << max_func.top_level_conjunction_count << '\n'
            << "   max overload depth:   "
               << eval.stats.overload_counter.max << '\n';
      }

      using query_func = void(*)(Evaluator&, std::ostream&, const std::string&);
      struct repl_query {
         std::string query;
         std::string description;
         query_func func;
      };

      repl_query queries[] = {
         {
            { "help" },
            { "prints details about queries" },
            print_help
         }, {
            { "print-scope" },
            { "print all of the bindings in the given scope; can give a "
              "namespace as an argument" },
            print_scope
         }, {
            { "print-assumptions" },
            { "print the assumptions in the current context" },
            print_assumptions
         }, {
            { "print-eqs" },
            { "print the current equivalence class" },
            print_eqs
         }, {
            { "expr-size" },
            { "prints the size of an expression" },
            print_size
         }, {
            { "metrics" },
            { "prints statistics about the current program" },
            print_metrics
         }
      };

      void
      print_help(Evaluator&, std::ostream& os, const std::string&) {
         os << "Liz queries:\n";
         for (auto i = std::begin(queries); i != std::end(queries); ++i)
            os << "  $" << std::left << std::setw(15) << i->query
               << i->description << '\n';
         os << "\nTo quit Liz, type `exit 0`.\n";
      }

      struct match_query {
         match_query(const std::string& str)
            : length(str.size() - 1), f(++str.begin()) { }
         std::size_t length;
         std::string::const_iterator f;

         // Checks the prefix of str
         bool operator()(const repl_query& q) {
            if (length < q.query.size())
               return false;
            return std::equal(q.query.begin(), q.query.end(), f);
         }
      };

      std::string
      get_args(const std::string& str) {
         auto i = ++str.begin();
         while(isalpha(*i) or *i == '-') ++i;
         while(*i == ' ') ++i;
         return { i, str.end() };
      }

      void
      exec_repl_command(Evaluator& eval, std::ostream& os,
                        const std::string& str)
      {
         auto i = std::find_if(std::begin(queries), std::end(queries),
                               match_query(str));
         os << '\n';
         if (i == std::end(queries))
            print_unrecognized_query(os, str);
         else
            (*i->func)(eval, os, get_args(str));
         os << '\n';
      }

   } // namespace Queries

   // This is the toplevel read-eval-print-loop
   void
   Evaluator::toplevel_loop(const Flags& flags) {
      Fragment fragment;
      FlagsManager new_flags(*this, flags);
      std::string str;
      const std::string prompt = "liz> ";
      output() << "\nWelcome to Liz! To quit, type `exit 0`. "
                  "For a list of queries, type `$help`\n\n";
      output() << prompt;
      while (std::getline(std::cin, str)) {
         if (not str.empty()) {
            if (Queries::is_repl_query(str)) {
               Queries::exec_repl_command(*this, output(), str);
            } else {
               try {
                  auto tokens = tokenize(fragment.source.append(str));
                  auto asts = reader()->parse(tokens, current_flags());
                  fragment.tokens.append(std::move(tokens));
                  evaluate_toplevel(asts);
                  fragment.asts.append(asts);
               }
               catch (const BasicError& e) {
                  e.issue_on(error());
                  error() << std::endl;
               }
            }
         }
         else if (not std::cin) {
            output() << std::endl;
            break;
         }
         output() << prompt;
      }
   }

   bool
   Evaluator::process_file(const Path& path, const Flags& flags) {
      FlagsManager new_flags(*this, flags);
      evaluate_toplevel(reader()->read_file(path, flags));
      return true;
   }
}
